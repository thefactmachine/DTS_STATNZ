##
##    Programme Name:  	create_accommodation_7578.R
##	
##    Objective:      	To create a DTS accommodation export for Stats NZ
##
##
##    Author: 		   	Mark Hatcher (Sector Trends, December, 2015)
##					 	Using code snippets and algorithms used for exporting IVS data export
##						(see P:\OTSP\IVS\5.Dissemination\Quarterly_production_code\IVS_NZ.stat)

##	 Called by:			integrate.R

# clear everything except location of current directory.
rm(list = ls()[!(ls() %in% c("str_cur_dir"))])

# load some libaries 
library(dplyr)
library(stringr)

options(stringsAsFactors = FALSE)
# do not display in scientific notation
options(scipen=999, digits = 10)

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')

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
# takes df_trips and renames some values in POV so that it aligns with df_purpose_lu
source('functions/fn_clean_df_trips.R')



# PREAMBLE (data relationships)
# relationship between df_trips & df_accomodation: a person makes a trip and...
# stays in accomodation. When a person makes a day trip there is no need for..
# accomodation. Therefore only "Overnight trips" are included 
# A single trip can have multiple accomodation values as a person..
# can stay in different hotels in the same or different locations
# the data is quarterly data

# PLAN OF ATTACK =====================================================================================

# BASIC CODE STEPS
# LOAD and process two CSV files (Quarterly data)
# AGGREGATE - From the quarterly data, aggregate these into quarterly year-ending summaries.
# FILTER the year ending summaries such that only whole years are included (first 3 quarters are not included)
# CREATE satellite tables and formats that are specifically required by Stats NZ - STATS NZ require data to be
# in a start schema configuration.

# ====================================================================================================

# LOAD data
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)
# clean up some POV values
df_trips <- fn_clean_df_trips(df_trips)


# (1.2) load and clean purpose of visit look up table data. 
# file obtained from "p:\OTSP\SAS\DTS\Classifications\Purpose.xls"
df_purpose_lu <- read.csv("inputs/POV_to_POV_Group.csv", header = TRUE) %>% 
		rename_("POV" = "Trip.Purpose.Description", 
		"POV_Group" = "Trip.Purpose.Group.Description..Estimation.") %>%
		select(POV, POV_Group)
		
# ASSERT: df_purpose_lu$POV are all contained in df_trips$POV
stopifnot(nrow(anti_join(df_purpose_lu, df_trips, by = c("POV" = "POV"))) == 0)


# Select relevant columns in df_trips and include df_purpose_lu$POV_Group
df_trips <- df_trips %>% 
			select(TripYear, TripQtr, TripType, SmoothedTripWeight, 
				DestinationRTO, POV, RespondentWeight) %>%
			inner_join(df_purpose_lu,  by = c("POV" = "POV"))

#===========================================================================
# CREATE a data frame of complete years 
# add 4 extra columns. These will be used in group by calculations to calculate YE aggregates (nrow = 90693)
df_trips <- fn_create_year_end(df_trips)

# create four data frames with unique year ending values
df_YE_Mar <- df_trips  %>% select(TripQtr, YEMar) %>% distinct() %>% rename(YE = YEMar)
df_YE_Jun <- df_trips  %>% select(TripQtr, YEJun) %>% distinct() %>% rename(YE = YEJun)
df_YE_Sep <- df_trips  %>% select(TripQtr, YESep) %>% distinct() %>% rename(YE = YESep)
df_YE_Dec <- df_trips  %>% select(TripQtr, YEDec) %>% distinct() %>% rename(YE = YEDec)


# Stack the four data frames; include whole year values; select a single column
df_YE_all <- bind_rows(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec) %>%
			group_by(YE) %>% 
			summarise(count = n()) %>% 
			filter(count == 4) %>% 
			select(YE)
# clean up			
rm(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec)

#=============================================================================
# CALCULATE Quarterly aggregates and rename columns

df_trips_qtrly <- df_trips %>% 
	mutate(Total_Respondents = 1) %>%
	rename(Total_Visitors = RespondentWeight, Total_Trips = SmoothedTripWeight, 
		Destination_RTO = DestinationRTO, Trip_Type = TripType)  %>%
	select(YEDec, YESep, YEJun, YEMar, Destination_RTO, Trip_Type, POV, POV_Group,
		Total_Visitors, Total_Trips, Total_Respondents) %>%
	group_by(YEDec, YESep, YEJun, YEMar, Destination_RTO, Trip_Type, POV, POV_Group) %>%
	summarise(Total_Visitors = sum(Total_Visitors), Total_Trips = sum(Total_Trips), 
		Total_Respondents = sum(Total_Respondents))

#=============================================================================
# CREATE Year Ending aggregates and filter to include full ears
# Create a vector of 4 different columns stacked on top of each other
YE <- c(df_trips_qtrly$YEDec, df_trips_qtrly$YESep, df_trips_qtrly$YEJun, df_trips_qtrly$YEMar)

# Duplicate the same dataframe four times and stack on top of each other
df_four_quarters <- rbind(df_trips_qtrly, df_trips_qtrly, df_trips_qtrly, df_trips_qtrly)

df_base_aggregates <- cbind(YE, df_four_quarters) %>%
	select(-c(YEDec, YESep, YEJun, YEMar))  %>%
	group_by(YE, Destination_RTO, Trip_Type, POV, POV_Group) %>%
	summarise(Total_Visitors = sum(Total_Visitors), Total_Trips = sum(Total_Trips),
			Total_Respondents = sum(Total_Respondents))  %>%  
	filter(YE %in% df_YE_all$YE)







# RECONCILIATION POINT. df_base_aggregates contains quarterly year ending values
# this means that each row is the sum of 4 quarters.  To reconcile these, four source
# files were aggregated. The four source files were:
# P:\OTSP\SAS\DTS\Output\2009Q4\reports\Est_Qtr_Trip_Type_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q1\reports\Est_Qtr_Trip_Type_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q2\reports\Est_Qtr_Trip_Type_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q3\reports\Est_Qtr_Trip_Type_Purpose.xls
# Expected totals for day trips and overnight trips: 27,491,016 & 15,981,538 

# ASSERT: totals for day and night trips are equal to SAS reports
df_rec <- df_base_aggregates %>% ungroup() %>% filter(YE == "YESep2010") %>% 
				group_by(Trip_Type) %>% summarise(trips = sum(Total_Trips))
				
stopifnot(as.integer(df_rec[df_rec$Trip_Type == "Day Trip", "trips"]) == 27491016)
stopifnot(as.integer(df_rec[df_rec$Trip_Type == "Overnight Trip", "trips"]) == 15981538)

rm(df_rec)


#=============================================================================
# CREATE various aggregate combinations
# There are five dimenions columns. The total number of group_by combinations of these are:
# 2^5 = 32.  1 of these has been previously created (see 'df_base_aggregates' )...
# the remaining 31 combinations are created below.  Of these 31 combinations, 30 are created
# programming using a function call. The remaining combination is created as a single line of code

# Assign the measure and dimension names...these will be used for column headings and also to:
# create values for DimensionIndex.csv and MeasureIndex.csv files and to create filenames.


# Assign the measure and dimension names...these will be used for column headings and also to:
# create values for DimensionIndex.csv and MeasureIndex.csv files and to create filenames.

vct_dim_names <- c("Year_Ending", "Destination_RTO", "Trip_Type", "POV", "POV_Group")
names(df_base_aggregates)[1:5] <- vct_dim_names


vct_measure_names <- c("Total_Visitors", "Total_Trips", "Total_Respondents")
names(df_base_aggregates)[6:8] <- vct_measure_names

# Create a "summarise" clause (for multiple use later)
lst_aggregations <- as.list(paste0("sum(", vct_measure_names,")"))
lst_sum_clause <- setNames(lst_aggregations, vct_measure_names)

# Sort order of the columns
vct_col_sort <- c(vct_dim_names, vct_measure_names)

# There are five columns resulting in 2^5 = 32 combinations...
# we now create 30 of these combinations
lst_combinations <- fn_create_column_combinations(vct_dim_names)

# Create a list of data frames Each list element is a data frame
lst_aggregations <- lapply(lst_combinations, function(x) 
	fn_create_comb_aggregates(df_base_aggregates, x, lst_sum_clause))

# ASSERT: length(lst_aggregations) == (2^length(vct_dim_names)) - 2
v_length <- length(vct_dim_names)
stopifnot(length(lst_aggregations) == sum(choose(v_length, 1:(v_length-1))))
rm(v_length)


# Combine the list of data frames into a single data frame
df_aggregations <- do.call(bind_rows, lst_aggregations)


# Grand totals (This is a single row grand total)
df_totals <- df_base_aggregates %>% ungroup() %>% 
				summarise_(.dots = lst_sum_clause) %>% 
				fn_create_df_with_all(vct_dim_names, vct_col_sort)

# clean up
rm(fn_create_column_combinations, fn_create_comb_aggregates, fn_create_df_with_all)
rm(fn_create_year_end, lst_aggregations, lst_sum_clause, vct_col_sort)



# combine all aggregates (2^5 combinations) into a single data frame
df_consolidated <- bind_rows(df_base_aggregates, df_aggregations, df_totals)

# ASSERT: No NAs bave been introduced before converting to text.
stopifnot(sum(is.na(df_consolidated)) == 0)

# Convert numeric columns to text with 0 decimal places
df_fin <- sapply(df_consolidated[, vct_measure_names], function(x) fn_convert_to_text(x)) %>%
 			# convert sapply's matrix to a data frame
 			as.data.frame() %>%
 			# club the original columns together with the new text columns
 			bind_cols(df_consolidated[, vct_dim_names, ], .)

#clean up. Leave "df_consolidated" available for reconciliation purposes.
rm(df_base_aggregates, df_totals, df_aggregations, lst_combinations)
rm(fn_convert_to_text)

#=============================================================================
# LOOKUPS
# Create 5 lookups for each of the dimension variables
# Created sorted list of unique values but with "All" on the end

# (1) Create Lookup for POV_Group
vct_POV_Group_desc <- c(sort(unique(df_fin[df_fin$POV_Group != "All", "POV_Group"])), "All")
vct_codes <- 1:length(vct_POV_Group_desc)
df_lu_POV_Group <- data.frame(Code = vct_codes, Description = vct_POV_Group_desc,  SortOrder = vct_codes)
# clean up
rm(vct_POV_Group_desc, vct_codes)

# (2) Create lookup for POV (Purpose of Visit)
# Created sorted list of unique values but with "All" on the end
vct_POV_desc <- c(sort(unique(df_fin[df_fin$POV != "All", "POV"])), "All")
vct_codes <- 1:length(vct_POV_desc)
df_lu_POV <- data.frame(Code = vct_codes, Description = vct_POV_desc, SortOrder = vct_codes)
# clean up
rm(vct_POV_desc, vct_codes)

# (3) Create lookup for Trip_Type
vct_Trip_desc <- c(sort(unique(df_fin[df_fin$Trip_Type != "All", "Trip_Type"])), "All")
vct_codes <- 1:length(vct_Trip_desc)
df_lu_Trip_Type <- data.frame(Code = vct_codes, Description = vct_Trip_desc,  SortOrder = vct_codes)
# clean up
rm(vct_Trip_desc, vct_codes)

# (4) Create lookup Destination RTO (This is pre-built so just load it!)
df_lu_dest_rto <- read.csv("inputs/DimenLookupDestinationRTOAccommodation.csv", header = TRUE)

# (5) Finally create YE Look ups
df_lu_YE <- fn_create_YE_lookup(df_fin$Year_Ending) %>% as.data.frame()


# Based on the lookup tables created, replace string values with numeric lookups 
df_fin_lu <- df_fin %>% 

			inner_join(df_lu_YE, by = c("Year_Ending" = "YE")) %>% 
			mutate(Year_Ending = Code) %>% 
			select(-c(Code, SortOrder, Description)) %>%		
			
			inner_join(df_lu_dest_rto, by = c("Destination_RTO" = "Description")) %>% 
			mutate(Destination_RTO = Code) %>% 
			select(-c(Code, SortOrder)) %>%

			inner_join(df_lu_Trip_Type, by = c("Trip_Type" = "Description")) %>% 
			mutate(Trip_Type = Code) %>% 
			select(-c(Code, SortOrder)) %>%

			inner_join(df_lu_POV, by = c("POV" = "Description")) %>% 
			mutate(POV = Code) %>% 
			select(-c(Code, SortOrder)) %>%

			inner_join(df_lu_POV_Group, by = c("POV_Group" = "Description")) %>% 
			mutate(POV_Group = Code) %>% 
			select(-c(Code, SortOrder)) %>% 

			as.data.frame()

# ASSERT that all rows joined rows in the lookup data frame should be equal to original
stopifnot(nrow(df_fin_lu) == nrow(df_fin))


# Create hierarchy data frames (a specific STATS NZ format for each dimension)
df_dh_YE <- fn_create_dim_hierarchy(df_lu_YE)
df_dh_dest_rto <- fn_create_dim_hierarchy(df_lu_dest_rto)
df_dh_Trip_Type <- fn_create_dim_hierarchy(df_lu_Trip_Type)
df_dh_POV <- fn_create_dim_hierarchy(df_lu_POV)
df_dh_POV_Group <- fn_create_dim_hierarchy(df_lu_POV_Group)


# # For dimension levels, STATS NZ (wisely) want the character variables wrapped in "quotes"
df_lu_YE$Description <- fn_quote(df_lu_YE$Description)
df_lu_dest_rto$Description <- fn_quote(df_lu_dest_rto$Description) 
df_lu_Trip_Type$Description <- fn_quote(df_lu_Trip_Type$Description)
df_lu_POV$Description <- fn_quote(df_lu_POV$Description) 
df_lu_POV_Group$Description <- fn_quote(df_lu_POV_Group$Description)


# For some reason, stats NZ need a SortOrder column which is blank
df_lu_YE$SortOrder <-  ""
df_lu_dest_rto$SortOrder <- ""
df_lu_Trip_Type$SortOrder <- ""
df_lu_POV$SortOrder <-  ""
df_lu_POV_Group$SortOrder <- ""


#  Drop YE columns...we used it for a join but not needed anymore
df_lu_YE$YE <- NULL


#=============================================================================
# INDEX Files
# These are used to map column names to human readable descriptions. 
# They are used in the following outputs: DimensionIndex.csv, FileIndex.csv, MeasureIndex.csv

# DimensionIndex.csv - create human readable names, wrap all character values in quotes (ie. fn_quote())

vct_dim_title <- fn_quote (c("Year ending", "Regional tourism organisation", "Trip type", 
						"Purpose of visit", "Purpose of visit group"))
df_dimension_index <- data.frame(DimensionCode = fn_quote(vct_dim_names), DimensionTitle = vct_dim_title)

vct_measure_title <- fn_quote(c("Total visitors", "Total trips", "Total respondents"))
df_measure_index <-data.frame(MeasureCode = fn_quote(vct_measure_names), MeasureTitle = vct_measure_title)

# Create file index data frame
df_file_index <- data.frame(TableID = "7579",
							TableCode = "TABLECODE7581",
							TableTitle = "Domestic Travel Survey: Trips",
							TableFileName = "",
							TableURL = "")

# clean up
rm(df_consolidated, fn_create_dim_hierarchy, fn_create_YE_lookup)


#=============================================================================
# FILE OUTPUT (output various data frames to csv files)
# prepare a named list of data frames. The names will be used to generate file names
# The order: data, lookups, dimension hierarchies, index files (dimension, measure, file) [use column order]


lst_output <- 	list(
				# the data
				df_fin_lu, 
				# the dimension lookups (in column order)
				df_lu_YE, df_lu_dest_rto, df_lu_Trip_Type, df_lu_POV, df_lu_POV_Group,
				# the dimension hierarchies (in column order)
				df_dh_YE, df_dh_dest_rto, df_dh_Trip_Type, df_dh_POV, df_dh_POV_Group,
				# the indexes (dimension, measure and then file)
				df_dimension_index, df_measure_index, df_file_index)

# clean up
rm(df_fin_lu, df_lu_POV_Group, df_lu_POV, df_lu_Trip_Type, df_lu_dest_rto, 
df_lu_YE, df_dh_POV_Group, df_dh_POV, df_dh_Trip_Type, df_dh_dest_rto, df_dh_YE)

# We have the list, now we just need to name each list element (ie. data frame)
data_name <- paste0("Data", df_file_index$TableID)

# For the dimensions, we use the column names stored in 'df_dimension_index$DimensionCode' to generate
# names that will be used for the file names. We remove underscores and quotes with a sneaky reg ex expresion.
vct_dimension_file_names <- 
	paste0("DimenLookup", gsub("(\"|_)","",df_dimension_index$DimensionCode), df_file_index$TableID)

vct_hierarchy_file_names <- 
	paste0("DimenHierarchy", gsub("(\"|_)", "", df_dimension_index$DimensionCode), df_file_index$TableID)


# collect all the index file names into a single vector
vct_index_names <- c("DimensionIndex", "MeasureIndex", "FileIndex")

# assemble the above into a single vector and assign to the list
 
names(lst_output)<- c(data_name, vct_dimension_file_names, vct_hierarchy_file_names, vct_index_names)


# clean up
rm(data_name, vct_dim_names, vct_hierarchy_file_names , vct_dimension_file_names, 
		vct_index_names, vct_measure_names)


# create a path to the output directory
sub_path_to_output <- paste0("outputs", "/", str_cur_dir, "/", "Table_", df_file_index$TableID)
curr_path <- getwd()

# if the file path does not exist then create it                            
str_full_path <- file.path(curr_path, sub_path_to_output)

if (!file.exists(str_full_path)) dir.create(str_full_path, recursive = TRUE)
# write the list of data.frames as csv files to "str_full_path"
# invisible() supresses console output.
invisible(lapply(seq_along(lst_output), 
		function(i) {     
			curr_file_name <- paste0(names(lst_output)[i],".csv")
			full_file_name <- paste0(file.path(str_full_path,curr_file_name))
			write.table(lst_output[[i]], full_file_name, sep = ",", 
				row.names = FALSE, quote = FALSE)
		}))


# clean up
rm(curr_path, df_dimension_index, df_file_index, df_measure_index, 
		lst_output, str_full_path, sub_path_to_output)

# clean up
rm(df_fin, df_four_quarters, df_purpose_lu, df_trips, df_trips_qtrly, df_YE_all, YE)






