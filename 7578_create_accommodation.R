##
##    Programme Name:  	create_accommodation_7578.R
##	
##    Objective:      	To create a DTS accommodation export for Stats NZ
##
##
##    Author: 		   	Mark Hatcher (Sector Trends, December, 2015)
##					 	Using code snippets and algorithms used for exporting IVS data export
##						(see P:\OTSP\IVS\5.Dissemination\Quarterly_production_code\IVS_NZ.stat)
##  

# clear everything
rm(list = ls())

# load some libaries 
library(dplyr)
library(stringr)
options(stringsAsFactors = FALSE)
# do not display in scientific notation
options(scipen=999, digits = 10)
setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')
# calculates "length of stay" - chops length of stay up into discreate units.
source('functions/fn_calc_los.R')
# creates a data.frame of column combinations for use in a group_by
source('functions/fn_create_column_combinations.R')
# uses column combinations to create various group_by aggregations
source('functions/fn_create_comb_aggregates.R')
# appends columns to a data.frame
source('functions/fn_create_df_with_all.R')
# appends a data.frame with four columns
source('functions/fn_create_year_end.R')
# converts floating point numbers to text
source('functions/fn_convert_to_text.R')
# creates  yearend lookup table
source('functions/fn_create_YE_lookup.R')
# takes a dimension lookup and creates a dimension hierarchy. These are specific formats for STATS_NZ
source('functions/fn_create_dim_hierarchy.R')
# takes a character vector and surrounds it with quotes hi --> "hi"
source('functions/fn_quote.R')

# PLAN OF ATTACK =====================================================================================

# PREAMBLE (data relationships)
# relationship between df_trips & df_accomodation: a person makes a trip and...
# stays in accomodation. When a person makes a day trip there is no need for..
# accomodation. Therefore only "Overnight trips" are included 
# A single trip can have multiple accomodation values as a person..
# can stay in different hotels in the same or different locations

# BASIC CODE STEPS
# LOAD and process two CSV files (Quarterly data)
# AGGREGATE - From the quarterly data, aggregate these into quarterly year-ending summaries.
# FILTER the year ending summaries such that only whole years are included (first 3 quarters are not included)
# CREATE satellite tables and formats that are specifically required by Stats NZ - STATS NZ require data to be
# in a start schema configuration.

# ====================================================================================================



# LOAD data
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)
df_accomodation <- read.csv("data/vw_DTSVisitAccommodation.csv", header = TRUE)

#=============================================================================

# PROCESS df_accomodation
df_accomodation <- df_accomodation %>% select(TripID, AccommodationType, NoNights)
# convert any NAs to zeros
df_accomodation[is.na(df_accomodation$NoNights), "NoNights"] <- 0
# clean up miss-spelling
df_accomodation[df_accomodation$AccommodationType == 
	"Employer provided accomodation", "AccommodationType"] <- 
	"Employer provided accommodation"

# calculate a column - converts number of nights into discreate categories
df_accomodation$LOS_Group <- fn_calc_los(df_accomodation$NoNights)

# delete uneeded columns & include only "Overnight trips" (nrow = 90693)
df_trips_overnight <- df_trips %>% filter(TripType == "Overnight Trip") %>% 
		select(c(TripIDNumber,TripType, TripYear,
		TripQtr, DestinationRTO, RespondentWeight, SmoothedTripWeight)) 

# clean up some columns - convert pesky NAs to zeros
df_trips_overnight[is.na(df_trips_overnight$RespondentWeight), "RespondentWeight"] <- 0
df_trips_overnight[is.na(df_trips_overnight$SmoothedTripWeight), "SmoothedTripWeight"] <- 0
df_trips_overnight[df_trips_overnight$DestinationRTO == "Other   ", "DestinationRTO"] <- "Other"
# clean up
rm(df_trips, fn_calc_los)


#=============================================================================
# CREATE a data frame of complete years 

# add 4 extra columns. These will be used in group by calculations to calculate YE aggregates (nrow = 90693)
df_trips_overnight <- fn_create_year_end(df_trips_overnight)

# create four data frames with unique year ending values
df_YE_Mar <- df_trips_overnight  %>% select(TripQtr, YEMar) %>% distinct() %>% rename(YE = YEMar)
df_YE_Jun <- df_trips_overnight  %>% select(TripQtr, YEJun) %>% distinct() %>% rename(YE = YEJun)
df_YE_Sep <- df_trips_overnight  %>% select(TripQtr, YESep) %>% distinct() %>% rename(YE = YESep)
df_YE_Dec <- df_trips_overnight  %>% select(TripQtr, YEDec) %>% distinct() %>% rename(YE = YEDec)


# Stack the four data frames; include whole year values; select a single column
df_YE_all <- bind_rows(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec) %>%
				group_by(YE) %>% 
				summarise(count = n()) %>% 
				filter(count == 4) %>% 
				select(YE)
# clean up			
rm(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec)
#=============================================================================

# COMBINE trips and accomodation into a single data frame
df_combined <- df_trips_overnight %>% 
	inner_join(df_accomodation, by = c("TripIDNumber"  = "TripID")) %>% 
	mutate(TotalNights = NoNights * SmoothedTripWeight, TotalRespondents = 1) %>%
	rename(TotalVisitors = RespondentWeight, TotalTrips = SmoothedTripWeight) %>%
	select(YEDec, YESep, YEJun, YEMar,  LOS_Group, DestinationRTO, 
		AccommodationType, TotalVisitors, TotalTrips, TotalNights, TotalRespondents) %>%
	group_by(YEDec, YESep, YEJun, YEMar, LOS_Group, DestinationRTO, AccommodationType ) %>%
	summarise(TotalVisitors = sum(TotalVisitors), TotalTrips = sum(TotalTrips), 
		TotalNights = sum(TotalNights), TotalRespondents = sum(TotalRespondents))

# clean up
rm(df_accomodation, df_trips_overnight)

#=============================================================================

# CREATE Year Ending aggregates and filter to include full ears

# create a vector of 4 different columns stacked on top of each other
YE <- c(df_combined$YEDec, df_combined$YESep, df_combined$YEJun, df_combined$YEMar)

# duplicate the same data frame 4 times and stack on top of each other (158188 rows)
df_four_quarters <- rbind(df_combined, df_combined, df_combined, df_combined)

# combine the previously created vector with the stacked data frames 
df_base_aggregates <- cbind(YE, df_four_quarters) %>%
	select(-c(YEDec, YESep, YEJun, YEMar))  %>%
	group_by(YE, LOS_Group, DestinationRTO, AccommodationType) %>%
	summarise(Total_Visitors = sum(TotalVisitors), Total_Trips = sum(TotalTrips), 
	Total_Nights = sum(TotalNights), Total_Respondents = sum(TotalRespondents)) %>%  
	filter(YE %in% df_YE_all$YE)



# RECONCILIATION POINT - Year Ending totals obtained from the following SAS reports
# P:\OTSP\SAS\DTS\Output\2010Q1\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 18160387 ]
# P:\OTSP\SAS\DTS\Output\2010Q2\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 10389425 ]
# P:\OTSP\SAS\DTS\Output\2010Q3\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 8509478 ]
# P:\OTSP\SAS\DTS\Output\2010Q4\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 10362275 ]
# Total of the above are: 47421565 

df_test_totals <- df_base_aggregates %>% 
					filter(YE == "YEDec2010")  %>% ungroup() %>%
					summarise_each(funs(sum), Total_Visitors, Total_Trips, 
						Total_Nights, Total_Respondents)

# ASSERT: total nights for year ending 2010Q4 are the same as SAS reports
stopifnot(as.integer(df_test_totals$Total_Nights) == 47421565)

# clean up
rm(df_combined, df_four_quarters, df_YE_all, df_chk_total)




#=============================================================================
# CREATE various aggregate combinations
# There are four dimenions columns. The total number of group_by combinations of these are:
# 2^4 = 16.  1 of these has been previously created (see 'df_base_aggregates' )...
# the remaining 15 combinations are created below.  Of these 15 combinations, 14 are created
# programming using a function call. The remaining combination is created as a single line of code

# Assign the measure and dimension names...these will be used for column headings and also to:
# create values for DimensionIndex.csv and MeasureIndex.csv files and to create filenames.

vct_dim_names <- c("Year_Ending", "LOS_Group", "Destination_RTO", "Accommodation_Type")
names(df_base_aggregates)[1:4] <- vct_dim_names

vct_measure_names <- c("Total_Visitors", "Total_Trips", "Total_Nights", "Total_Respondents")
names(df_base_aggregates)[5:8] <- vct_measure_names


# Create a "summarise" clause (for multiple use later)
lst_aggregations <- as.list(paste0("sum(", vct_measure_names,")"))
lst_sum_clause <- setNames(lst_aggregations, vct_measure_names)


# Sort order of the columns
vct_col_sort <- c(vct_dim_names, vct_measure_names)

# There are four columns resulting in 2^4 = 16 combinations...
# we now create 14 of these combinations
lst_combinations <- fn_create_column_combinations(vct_dim_names)

# Create a list of data frames Each list element is a data frame
lst_aggregations <- lapply(lst_combinations, function(x) 
	fn_create_comb_aggregates(df_base_aggregates, x, lst_sum_clause))

#Combine the list of data frames into a single data frame
df_aggregations <- do.call(bind_rows, lst_aggregations)

# Grand totals
df_totals <- df_base_aggregates %>% ungroup() %>% 
				summarise_(.dots = lst_sum_clause) %>% 
				fn_create_df_with_all(vct_dim_names, vct_col_sort)
# Clean up
rm(fn_create_column_combinations, fn_create_comb_aggregates, fn_create_df_with_all)
rm(fn_create_year_end, lst_aggregations, lst_sum_clause, vct_col_sort)


# combine all aggregates (i.e 2^4 combinations) into a single data frame
df_consolidated <- bind_rows(df_base_aggregates, df_aggregations, df_totals)

# ASSERT: No NAs bave been introduced before converting to text.
stopifnot(sum(is.na(df_consolidated)) == 0)

# Convert numeric columns to text with 0 decimal places
df_fin <- sapply(df_consolidated[, vct_measure_names], function(x) fn_convert_to_text(x)) %>%
 			# convert sapply's matrix to a data frame
 			as.data.frame() %>%
 			# club the original columns together with the new text columns
 			bind_cols(df_consolidated[, vct_dim_names, ], .)

#clean up
rm(df_base_aggregates, df_totals, df_aggregations, lst_combinations)
rm(fn_convert_to_text)

#=============================================================================
# LOOKUPS
# Import lookup tables and create lookup for year end. There should be a look up table for each dimensions
df_lu_acccom_type <- read.csv("inputs/DimenLookupAccommodationTypeAccommodation.csv", header = TRUE)
df_lu_dest_rto <- read.csv("inputs/DimenLookupDestinationRTOAccommodation.csv", header = TRUE)
df_lu_LOS <- read.csv("inputs/DimenLookupLOS_groupAccommodation.csv", header = TRUE)
df_lu_YE <- fn_create_YE_lookup(df_fin$Year_Ending) %>% as.data.frame()

# Based on the lookup tables created, replace string values with numeric lookups 
df_fin_lu <- df_fin %>% 
			inner_join(df_lu_LOS, by = c("LOS_Group" = "Description")) %>% 
			mutate(LOS_Group = Code) %>% 
			select(-c(Code, SortOrder)) %>% 
		
			inner_join(df_lu_dest_rto, by = c("Destination_RTO" = "Description")) %>% 
			mutate(Destination_RTO = Code) %>% 
			select(-c(Code, SortOrder)) %>%
		
			inner_join(df_lu_acccom_type, by = c("Accommodation_Type" = "Description")) %>% 
			mutate(Accommodation_Type = Code) %>% 
			select(-c(Code, SortOrder)) %>%
 		
 			inner_join(df_lu_YE, by = c("Year_Ending" = "YE")) %>% 
			mutate(Year_Ending = Code) %>% 
			select(-c(Code, SortOrder, Description)) %>%   
			as.data.frame()

# Create hierarchy data frames (a specific STATS NZ format for each dimension)
df_dh_acccom_type <- fn_create_dim_hierarchy(df_lu_acccom_type)
df_dh_dest_rto <- fn_create_dim_hierarchy(df_lu_dest_rto)
df_dh_LOS <- fn_create_dim_hierarchy(df_lu_LOS)
df_dh_YE <- fn_create_dim_hierarchy(df_lu_YE)

# For dimension levels, STATS NZ (wisely) want the character variables wrapped in "quotes"
df_lu_acccom_type$Description <- fn_quote(df_lu_acccom_type$Description)
df_lu_dest_rto$Description <- fn_quote(df_lu_dest_rto$Description)
df_lu_LOS$Description <- fn_quote(df_lu_LOS$Description)
df_lu_YE$Description <- fn_quote(df_lu_YE$Description)


# For some reason, stats NZ need a SortOrder column which is blank
df_lu_acccom_type$SortOrder <- "" 
df_lu_dest_rto$SortOrder <- ""
df_lu_LOS$SortOrder <- ""
df_lu_YE$SortOrder <- ""


# Drop YE columns...we used it for a join but not needed anymore
df_lu_YE$YE <- NULL


#=============================================================================
# INDEX Files
# These are used to map column names to human readable descriptions. 
# They are used in the following outputs: DimensionIndex.csv, FileIndex.csv, MeasureIndex.csv

# DimensionIndex.csv - create human readable names, wrap all character values in quotes (ie. fn_quote())
vct_dim_title <- fn_quote(c("Year ending", "Length of stay", "Regional tourism organisation", "Accommodation type"))
df_dimension_index <- data.frame(DimensionCode = fn_quote(vct_dim_names), DimensionTitle = vct_dim_title)

# MeasureIndex.csv - create human readable names, wrap all character valvues in quotes (ie. fn_quote())
vct_measure_title <- fn_quote(c("Total visitors", "Total trips", "Total nights", "Total respondents"))
df_measure_index <- data.frame(MeasureCode = fn_quote(vct_measure_names), MeasureTitle = vct_measure_title)


# FileIndex.csv data frame
df_file_index <- data.frame(TableID = "7578",
							TableCode = "TABLECODE7578",
							TableTitle = "Domestic Travel Survey: Accommodation",
							TableFileName = "",
							TableURL = "")
# clean up
rm(df_consolidated, fn_create_dim_hierarchy, fn_create_YE_lookup)

#=============================================================================
# FILE OUTPUT (output various data frames to csv files)
# prepare a names list of data frames. The names will be used to generate file names
# The order: data, lookups, dimension hierarchies, index files (dimension, measure, file) [use column order]

lst_output <- list(df_fin_lu, 
				df_lu_YE, df_lu_LOS, df_lu_dest_rto, df_lu_acccom_type,
				df_dh_YE, df_dh_LOS, df_dh_dest_rto, df_dh_acccom_type,
				df_dimension_index, df_measure_index, df_file_index)
# clean up
rm(df_dh_acccom_type, df_dh_dest_rto, df_dh_LOS, df_dh_YE, 
		df_fin, df_fin_lu, df_lu_acccom_type, df_lu_dest_rto, df_lu_LOS, df_lu_YE)

# We have the list, now we just need to name each list element (ie. data frame)
data_name <- paste0("Data", df_file_index$TableID)


# For the dimensions, we use the column names stored in 'df_dimension_index$DimensionCode' to generate
# names that will be used for the file names. We remove underscores and quotes with a sneaky reg ex expresion.
vct_dimension_file_names <- 
	paste0("DimenLookup", gsub("(\"|_)","",df_dimension_index$DimensionCode), df_file_index$TableID)


# gsub is to convert from names with underscores to camelCase. Also replace quotes (i.e " with blanks)
vct_hierarchy_file_names <- 
	paste0("DimenHierarchy", gsub("(\"|_)", "", df_dimension_index$DimensionCode), df_file_index$TableID)
	
# collect all the index file names into a single vector
vct_index_names <- c("DimensionIndex", "MeasureIndex", "FileIndex")

# assemble the above into a single vector and assign to the list
names(lst_output) <- c(data_name, vct_dimension_file_names, vct_hierarchy_file_names, vct_index_names) 

# clean up
rm(data_name, vct_dim_names, vct_hierarchy_file_names, vct_dimension_file_names, 
		vct_index_names, vct_measure_names)

# create a path to the output directory
sub_path_to_output <- paste0("outputs", "/", "Table_", df_file_index$TableID)
curr_path <- getwd()

# if the file path does not exist then create it
str_full_path <- file.path(curr_path, sub_path_to_output)
if (!file.exists(str_full_path)) dir.create(str_full_path)


# write the list of data.frames as csv files to "str_full_path"
# invisible() supresses console output.
invisible(lapply(seq_along(lst_output), 
		function(i) {     
			curr_file_name <- paste0(names(lst_output)[i],".csv")
			full_file_name <- paste0(file.path(str_full_path,curr_file_name))
			write.table(lst_output[[i]], full_file_name, sep = ",", 
				row.names = FALSE, quote = FALSE)
		}))
		
		

# compare outputs
# the first is the sum of the text columns		
lst_output$Data7578 %>% filter(Year_Ending == 45 & LOS_Group != 20 & 
			Destination_RTO != 34 & Accommodation_Type != 35) %>%
			mutate(tv = as.numeric(Total_Visitors), tt = as.numeric(Total_Trips),
			tn = as.numeric(Total_Nights), tr = as.numeric(Total_Respondents)) %>%
			summarise_each(funs(sum), tv, tt, tn, tr)
			
# the second is the sum of the numeric columns			
df_test_totals

		
# clean up
rm(curr_path, df_dimension_index, df_file_index, df_measure_index, 
		lst_output, str_full_path, sub_path_to_output)












