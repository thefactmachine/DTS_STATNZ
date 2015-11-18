# clear everything
rm(list = ls())

# load some libaries 
#library(lubridate)
library(dplyr)
library(stringr)

options(stringsAsFactors = FALSE)
# do not display in scientific notation
options(scipen=999, digits = 10)

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')

# calculates "length of stay"
source('functions/fn_calc_los.R')
# creates a data.frame of column combinations for use in a group_by
source('functions/fn_create_column_combinations.R')
# uses column combinations to create various group_by
source('functions/fn_create_comb_aggregates.R')
# appends columns to a data.frame such that resultant df is always 8 columns wide
source('functions/fn_create_df_with_all.R')
# appends a data.frame with four columns
source('functions/fn_create_year_end.R')
# converts floating point number to text
source('functions/fn_convert_to_text.R')
# creates  yearend lookup table
source('functions/fn_create_YE_lookup.R')
# takes a dimension lookup and creates a dimension hierarchy
source('functions/fn_create_dim_hierarchy.R')

# takes a character vector and surrounds it with quotes hi --> "hi"
source('functions/fn_quote.R')





# PREAMBLE (data relationships)
# relationship between df_trips & df_accomodation: a person makes a trip and...
# stays in accomodation. When a person makes a day trip there is no need for..
# accomodation. Therefore only "Overnight trips" are included 
# A single trip can have multiple accomodation values as a person..
# can stay in different hotels in the same or different locations
# the data is quarterly data

# PLAN OF ATTACK
# 1) load two CSV files 
# 2) process the two CVS files
# 3) create a data frame of complete years (where the number of quarters = 4)
# 4) combine the two data frames and aggregate by: 
#		a) year end (4 quarterly year ends) and
#		b) four dimension variables
# 5) aggregate data by: year end and 4 dimensions variables. Include full years only
# 6) create various aggregate combinations


# (1) LOAD data
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)
df_accomodation <- read.csv("data/vw_DTSVisitAccommodation.csv", header = TRUE)

#=============================================================================

# (2.1) PROCESS df_accomodation
# for df_accomodation: select relevent columns; create a new column
df_accomodation <- df_accomodation %>% select(TripID, AccommodationType, NoNights)
# convert any NAs to zeros
df_accomodation[is.na(df_accomodation$NoNights), "NoNights"] <- 0
# clean up miss-spelling
df_accomodation[df_accomodation$AccommodationType == 
	"Employer provided accomodation", "AccommodationType"] <- 
	"Employer provided accommodation"

# calculate a column
df_accomodation$LOS_Group <- fn_calc_los(df_accomodation$NoNights)


# (2.2) PROCESS df_trips
# delete uneeded columns & include only "Overnight trips" (nrow = 90693)
df_trips_overnight <- df_trips %>% filter(TripType == "Overnight Trip") %>% 
		select(c(TripIDNumber,TripType, TripYear,
		TripQtr, DestinationRTO, RespondentWeight, SmoothedTripWeight)) 

# clean up some columns
df_trips_overnight[is.na(df_trips_overnight$RespondentWeight), "RespondentWeight"] <- 0
df_trips_overnight[is.na(df_trips_overnight$SmoothedTripWeight), "SmoothedTripWeight"] <- 0
df_trips_overnight[df_trips_overnight$DestinationRTO == "Other   ", "DestinationRTO"] <- "Other"
# clean up
rm(df_trips, fn_calc_los)

#=============================================================================

# (3) CREATE a data frame of complete years. 
# add 4 extra columns. These will be used in group by calculations (nrow = 90693)
df_trips_overnight <- fn_create_year_end(df_trips_overnight)

# (3.1) create four data frames with unique year ending values
df_YE_Mar <- df_trips_overnight  %>% select(TripQtr, YEMar) %>% distinct() %>% rename(YE = YEMar)
df_YE_Jun <- df_trips_overnight  %>% select(TripQtr, YEJun) %>% distinct() %>% rename(YE = YEJun)
df_YE_Sep <- df_trips_overnight  %>% select(TripQtr, YESep) %>% distinct() %>% rename(YE = YESep)
df_YE_Dec <- df_trips_overnight  %>% select(TripQtr, YEDec) %>% distinct() %>% rename(YE = YEDec)



# (3.2) stack the four data frames; include whole year values; select a single column
df_YE_all <- bind_rows(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec) %>%
			group_by(YE) %>% summarise(count = n()) %>% 
			filter(count == 4) %>% select(YE)
# clean up			
rm(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec)



# (4) COMBINE two data frames into one
# combine the two data frames into one, do some renaming, grouping and aggregating
df_combined <- df_trips_overnight %>% 
	inner_join(df_accomodation, by = c("TripIDNumber"  = "TripID")) %>% 
	mutate(TotalNights = NoNights * SmoothedTripWeight, TotalRespondents = 1) %>%
	rename(TotalVisitors = RespondentWeight, TotalTrips = SmoothedTripWeight) %>%
	select(YEDec, YESep, YEJun, YEMar,  LOS_Group, DestinationRTO, 
		AccommodationType, TotalVisitors, TotalTrips, TotalNights, TotalRespondents) %>%
	group_by(YEDec, YESep, YEJun, YEMar, LOS_Group, DestinationRTO, AccommodationType ) %>%
	summarise(TotalVisitors = sum(TotalVisitors), TotalTrips = sum(TotalTrips), 
		TotalNights = sum(TotalNights), TotalRespondents = sum(TotalRespondents))

# RECONCILIATION POINT - Uncomment the following to reconcile againt source SAS report:
# P:\OTSP\SAS\DTS\Output\2010Q3\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 8,509,478 ]
# df_combined %>% filter(TripYear == 2010 & TripQtr ==3) %>% summarise(nights = sum(TotalNights))



# clean up
rm(df_accomodation, df_trips_overnight)

#=============================================================================

# (5) CREATE Year Ending aggregates and filter to include full ears
# create a vector of 4 different columns stacked on top of each other
YE <- c(df_combined$YEDec, df_combined$YESep, df_combined$YEJun, df_combined$YEMar)

# duplicate the same data frame 4 time and stack on top of each other (158188 rows)
df_four_quarters <- rbind(df_combined, df_combined, df_combined, df_combined)

# combine the previously created vector with the stacked data frames 
# unfiltered is 93734 rows, filtered is 86511 rows
df_base_aggregates <- cbind(YE, df_four_quarters) %>%
	select(-c(YEDec, YESep, YEJun, YEMar))  %>%
	group_by(YE, LOS_Group, DestinationRTO, AccommodationType) %>%
	summarise(Total_Visitors = sum(TotalVisitors), Total_Trips = sum(TotalTrips), 
	Total_Nights = sum(TotalNights), Total_Respondents = sum(TotalRespondents)) %>%  
	filter(YE %in% df_YE_all$YE)



# RECONCILIATION POINT - Uncomment the following to reconcile againt source SAS reports:
# P:\OTSP\SAS\DTS\Output\2010Q1\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 18160387 ]
# P:\OTSP\SAS\DTS\Output\2010Q2\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 10389425 ]
# P:\OTSP\SAS\DTS\Output\2010Q3\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 8509478 ]
# P:\OTSP\SAS\DTS\Output\2010Q4\reports\Est_Qtr_Nights_Accom_Type.xls [ total  = 10362275 ]
# expecting total of 47421565 [Sum of the 4 totals above]
# df_base_aggregates %>% ungroup() %>% filter(YE == "YEDec2010") %>% summarise(nights = sum(Total_Nights))


# clean up
rm(df_combined, df_four_quarters, df_YE_all, YE)


#=============================================================================
# (6) CREATE various aggregate combinations
# PREAMBLE for  (6)
# There are four dimenions columns. The total number of group_by combinations of these are:
# 2^4 = 16.  1 of these has been previously created (see 'df_base_aggregates' )...
# the remaining 15 combinations are created below.  Of these 15 combinations, 14 are created
# programmatically (see 6.5). The remaining combination is created as a single line of code (see 6.6)


# (6.1) get the dimenions names
vct_dim_names <- c("YE", "LOS_Group", "Destination_RTO", "Accommodation_Type")

names(df_base_aggregates)[1:4] <- vct_dim_names



# (6.2) create a "summarise" clause (for multiple use later)
lst_aggregations <- list("sum(Total_Visitors)", "sum(Total_Trips)", 
	"sum(Total_Nights)", "sum(Total_Respondents)")

vct_measure_names <- c("Total_Visitors", "Total_Trips", "Total_Nights", "Total_Respondents")

lst_sum_clause <- setNames(lst_aggregations, vct_measure_names)

# (6.3) sort order of the columns
vct_col_sort <- c(vct_dim_names, vct_measure_names)

# (6.4) There are four columns resulting in 2^4 = 16 combinations...
# we now create 14 of these combinations
lst_combinations <- fn_create_column_combinations(vct_dim_names)

# (6.5) Create a list of data frames Each list element is a data frame
# based on a column combination created in 6.4
lst_aggregations <- lapply(lst_combinations, function(x) 
	fn_create_comb_aggregates(df_base_aggregates, x, lst_sum_clause))

# (6.5.1) combine the list of data frames into a single data frame
df_aggregations <- do.call(bind_rows, lst_aggregations)

# (6.6) grand totals
df_totals <- df_base_aggregates %>% ungroup() %>% 
				summarise_(.dots = lst_sum_clause) %>% 
				fn_create_df_with_all(vct_dim_names, vct_col_sort)
# clean up
rm(fn_create_column_combinations, fn_create_comb_aggregates, fn_create_df_with_all)
rm(fn_create_year_end, lst_aggregations, lst_sum_clause, vct_col_sort)

#=============================================================================

# (7) combine all aggregates into a single data frame
# total rows = 86511 + 71114 + 1 = 157626
df_consolidated <- bind_rows(df_base_aggregates, df_aggregations, df_totals)

# RECONCILIATION POINT - reconcile df_aggregations to 47421565 for "YEDec2010" [see above]
# df_aggregations %>% filter(YE == "YEDec2010" & LOS_Group == "All" 
# & DestinationRTO == "All" & AccommodationType == "All")



# (7.1) convert numeric columns to text with 0 decimal places
df_fin <- sapply(df_consolidated[, vct_measure_names], function(x) fn_convert_to_text(x)) %>%
 			# convert sapply's matrix to a data frame
 			as.data.frame() %>%
 			# club the original columns together with the new text columns
 			bind_cols(df_consolidated[, vct_dim_names, ], .)

#clean up
rm(df_base_aggregates, df_totals, df_aggregations, lst_combinations)
rm(fn_convert_to_text)





#=============================================================================
# (8) LOOKUPS
# (8.1) import lookup tables and create lookup for year end
df_lu_acccom_type <- read.csv("inputs/DimenLookupAccommodationTypeAccommodation.csv", header = TRUE)
df_lu_dest_rto <- read.csv("inputs/DimenLookupDestinationRTOAccommodation.csv", header = TRUE)
df_lu_LOS <- read.csv("inputs/DimenLookupLOS_groupAccommodation.csv", header = TRUE)



source('functions/fn_create_YE_lookup.R')

df_lu_YE <- fn_create_YE_lookup(df_fin$YE) %>% as.data.frame()

# (8.2) based on the lookup tables created, replace string values with numeric lookups for the...
# 4 dimensions
df_fin_lu <- df_fin %>% 
			inner_join(df_lu_LOS, by = c("LOS_Group" = "Description")) %>% 
			mutate(LOS_Group = Code) %>% 
			select(-c(Code, SortOrder)) %>% 
		
			inner_join(df_lu_dest_rto, by = c("Destination_RTO" = "Description")) %>% 
			mutate(DestinationRTO = Code) %>% 
			select(-c(Code, SortOrder)) %>%
		
			inner_join(df_lu_acccom_type, by = c("Accommodation_Type" = "Description")) %>% 
			mutate(Accommodation_Type = Code) %>% 
			select(-c(Code, SortOrder)) %>%
 		
 			inner_join(df_lu_YE, by = c("YE" = "YE")) %>% 
			mutate(YE = Code) %>% 
			select(-c(Code, SortOrder, Description)) %>%   
		
			rename(Year_ending = YE) %>%
			as.data.frame()



			
			
# (8.3) create dimension hierarchy from dimension lookups in 8.1
df_dh_acccom_type <- fn_create_dim_hierarchy(df_lu_acccom_type)
df_dh_dest_rto <- fn_create_dim_hierarchy(df_lu_dest_rto)
df_dh_LOS <- fn_create_dim_hierarchy(df_lu_LOS)
df_dh_YE <- fn_create_dim_hierarchy(df_lu_YE)



# (8.3.1) surround character columns with quotes
df_lu_acccom_type$Description <- fn_quote(df_lu_acccom_type$Description)
df_lu_dest_rto$Description <- fn_quote(df_lu_dest_rto$Description)
df_lu_LOS$Description <- fn_quote(df_lu_LOS$Description)
df_lu_YE$Description <- fn_quote(df_lu_YE$Description)




# (8.3.2) Set sort order to blank
df_lu_acccom_type$SortOrder <- "" 
df_lu_dest_rto$SortOrder <- ""
df_lu_LOS$SortOrder <- ""
df_lu_YE$SortOrder <- ""


# (8.3.3) Drop YE columns...we used it for a join but not needed anymore
df_lu_YE$YE <- NULL







# (8.4) create Dimension and Measure Index df's fn_quote() surounds with "
df_dimension_index <- 
		data.frame(DimensionCode = fn_quote(vct_dim_names), 
		DimensionTitle = fn_quote(gsub("_", " ", vct_dim_names)))


df_measure_index <-
		data.frame(MeasureCode = fn_quote(vct_measure_names), 
		MeasureTitle = fn_quote(gsub("_", " ", vct_measure_names)))


# (8.5) create file index data frame
df_file_index <- data.frame(TableID = "7578",
							TableCode = "TABLECODE7578",
							TableTitle = "Domestic Travel Survey: Accommodation",
							TableFileName = "",
							TableURL = "")

# clean up
rm(df_consolidated, fn_create_dim_hierarchy, fn_create_YE_lookup)

#=============================================================================
# (9)  FILE OUTPUT
# (9.1) prepare a list containing all dataframes (to be csv files) that will be output
# the order doesn't matter...but for the sake of accuracy, lets impose some structure:
# data, dimension_lookups, dimension_hierarchies, indexes (dimension, measure, file)



lst_output <- list(df_fin_lu, 
				df_lu_YE, df_lu_LOS, df_lu_dest_rto, df_lu_acccom_type,
				df_dh_YE, df_dh_LOS, df_dh_dest_rto, df_dh_acccom_type,
				df_dimension_index, df_measure_index, df_file_index)
# clean up
rm(df_dh_acccom_type, df_dh_dest_rto, df_dh_LOS, df_dh_YE, 
		df_fin, df_fin_lu, df_lu_acccom_type, df_lu_dest_rto, df_lu_LOS, df_lu_YE)


# (9.2) give the list some meaningful names that will be used as file names
# use the information contained in the previous data.frames to encourage consistency



data_name <- paste0("Data", df_file_index$TableID)

# gsub is to convert from names with underscores to camelCase. Also replace quotes (i.e " with blanks)
vct_dimension_names <- 
	paste0("DimenLookup", gsub("(\"|_)","",df_dimension_index$DimensionCode), df_file_index$TableID)


# gsub is to convert from names with underscores to camelCase. Also replace quotes (i.e " with blanks)
vct_hierarchy_names <- 
	paste0("DimenHierarchy", gsub("(\"|_)", "", df_dimension_index$DimensionCode), df_file_index$TableID)
	

vct_index_names <- c("DimensionIndex", "MeasureIndex", "FileIndex")



# assemble the above into a single vector and assign to the list
vct_list_names <- c(data_name, vct_dimension_names, vct_hierarchy_names, vct_index_names) 
names(lst_output) <- vct_list_names

# clean up
rm(data_name, vct_dim_names, vct_hierarchy_names, vct_dimension_names, 
		vct_index_names, vct_list_names, vct_measure_names)


# (9.3) prepare / create output directory

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
		
		
# clean up
rm(curr_path, df_dimension_index, df_file_index, df_measure_index, 
		lst_output, str_full_path, sub_path_to_output)












