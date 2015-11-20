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
library(tidyr)
library(xlsx)

options(stringsAsFactors = FALSE)
# do not display in scientific notation
options(scipen=999, digits = 10)

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')

# calculates "length of stay"
#source('functions/fn_calc_los.R')
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
# this is a collection of statements that changes a few values so it can join with "df_purpose_lu"
source('functions/fn_clean_trips.R')
# this function cleans up "vw_DTSTripSpend.csv" It removes "alcohol" from "Food and Alcohol"
source('functions/fn_clean_spend.R')
# loads a prepares an activity classification table
source('functions/fn_get_classification.R')
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

# LOAD and Prepare data
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)

df_trips <- df_trips %>% 
		select(TripIDNumber, TripYear, TripQtr, TripType, DestinationRTO, SmoothedTripWeight) %>%
		rename(Total_Trips = SmoothedTripWeight)

# convert those pesky NAs to zero.
df_trips[is.na(df_trips$Total_Trips) == TRUE, "Total_Trips"] <- 0

# This function gets an XL spreasheet and cleans some values so it matches with df_activities
df_activity_classification <- fn_get_classification()

df_activities <- read.csv("data/vw_DTSVisitActivities.csv", header = TRUE) %>%
			select(-AnswerNumber, -LegNumber)

# clean up a rogue value
df_activities[df_activities$Activities == "Business   " ,  "Activities"] <- "Business"

# append activity group to df_activies
df_activities <- df_activities %>% 
 			inner_join(df_activity_classification, by = c("Activities" = "Activity")) %>%
 			select(TripID, Activity_Group, Activities)

		
df_combined <- inner_join(df_trips, df_activities, by = c("TripIDNumber" = "TripID"))


#========================================================================
# RECONCILIATION POINT. The following code snippet should reconcile to 
# P:\OTSP\SAS\DTS\Output\2010Q3\reports\Est_Qtr_Trip_Type_Purpose.xls 
# The number of trips is 8,465,427

df_rec <- df_combined %>% filter(TripYear == 2010 & TripQtr == 3) %>% 
			distinct(TripIDNumber) %>% summarise(total = sum(Total_Trips))

stopifnot(round(df_rec$total, 0) == 8465427)
rm(df_rec, df_activity_classification)

#========================================================================
#=============================================================================
# CREATE a data frame of complete years. 

# add 4 extra columns: (YEMar, YEJun, YESep, YEDec) These will be used in group by calculations 
df_combined <- fn_create_year_end(df_combined)

# Create four data frames with unique year ending values
df_YE_Mar <- df_combined %>% select(TripQtr, YEMar) %>% distinct() %>% rename(YE = YEMar)
df_YE_Jun <- df_combined %>% select(TripQtr, YEJun) %>% distinct() %>% rename(YE = YEJun)
df_YE_Sep <- df_combined %>% select(TripQtr, YESep) %>% distinct() %>% rename(YE = YESep)
df_YE_Dec <- df_combined %>% select(TripQtr, YEDec) %>% distinct() %>% rename(YE = YEDec)


# Stack the four data frames; include whole year values; select a single column
df_YE_all <- bind_rows(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec) %>%
			group_by(YE) %>% summarise(count = n()) %>% 
			filter(count == 4) %>% select(YE)
# clean up			
rm(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec)


#=============================================================================
# CALCULATE Quarterly aggregates and rename columns
df_activity_qtrly <- df_combined %>% 
						select(YEMar, YEJun, YESep, YEDec, Activity_Group, Total_Trips) %>% 
						group_by(YEMar, YEJun, YESep, YEDec, Activity_Group) %>%
						summarise(Total_Activities = sum(Total_Trips), Raw_Count = n())

#=============================================================================
# CREATE Year Ending aggregates and filter to include full ears

# create a vector of 4 different columns stacked on top of each other
YE <- c(df_activity_qtrly$YEDec, df_activity_qtrly$YESep, df_activity_qtrly$YEJun, df_activity_qtrly$YEMar)

# Duplicate the same dataframe four times and stack on top of each other
df_four_quarters <- rbind(df_activity_qtrly, df_activity_qtrly, df_activity_qtrly, df_activity_qtrly)


df_base_aggregates <- cbind(YE, df_four_quarters) %>%
	select(-c(YEDec, YESep, YEJun, YEMar))  %>%
	group_by(YE, Activity_Group) %>%
	summarise(Total_Activities = sum(Total_Activities), Raw_Count = sum(Raw_Count))  %>%  
	filter(YE %in% df_YE_all$YE)
	
#=============================================================================
#=============================================================================
# RECONCILIATION POINT. df_base_aggregates contains quarterly year ending values
# this means that each row is the sum of 4 quarters.  To reconcile these, four source
# files were aggregated. The four source files were:

# P:\OTSP\SAS\DTS\Output\2010Q1\reports\Est_Qtr_Trip_Type_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q2\reports\Est_Qtr_Trip_Type_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q3\reports\Est_Qtr_Trip_Type_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q4\reports\Est_Qtr_Trip_Type_Purpose.xls
# see summary of the above reports at this location
# \workings\activity_YE_2010_Q4\Reconciliation_trips_to_activity.xlsx

# NOTE: There are no SAS reports to directly reconcile to that record..
# activities.

# Following reconciles to 44,077,294 (total for YE 2010)

stopifnot(as.integer(df_trips %>% filter(TripYear == 2010) %>% 
	summarise(total = sum(Total_Trips))) == 44077294)

# Following results in 23,330 activities (unweighted for YE 2010)
stopifnot(as.integer(df_base_aggregates %>% filter(YE == "YEDec2010") %>% 
	summarise(total = sum(Raw_Count)) %>% select(total)) == 23330)


# Following produces 340349 - 23330 = 317019  (total unweighted activities)
stopifnot(df_combined %>% filter(TripYear != 2010) 
	%>% summarise(total = n()) == 317019)

# This total reconciles to source data (i.e 340,349 rows)
stopifnot(read.csv("data/vw_DTSVisitActivities.csv", header = TRUE) %>% 
	summarise(count = n()) == 340349)

# After doing the calculations above we dont need the "Raw_Count" column..

df_base_aggregates <- df_base_aggregates %>% select(-Raw_Count)

#=============================================================================
# CREATE various aggregate combinations
# There are two dimenions columns. The total number of group_by combinations of these are:
# 2^2 = 2.  1 of these has been previously created (see 'df_base_aggregates' )...
# the remaining 3 combinations are created below. Of these 3 combinations, 2 are created
# programmatically using a function call. The remaining combination is created as a single line of code

# Assign the measure and dimension names...these will be used for column headings and also to:
# create values for DimensionIndex.csv and MeasureIndex.csv files and to create filenames.


vct_dim_names <- c("Year_Ending", "Activity_Group")
names(df_base_aggregates)[1:2] <- vct_dim_names

vct_measure_names <- c("Total_Activities")
names(df_base_aggregates)[3] <- vct_measure_names


# Create a "summarise" clause (for multiple use later)
lst_aggregations <- as.list(paste0("sum(", vct_measure_names,")"))
lst_sum_clause <- setNames(lst_aggregations, vct_measure_names)


# Sort order of the columns
vct_col_sort <- c(vct_dim_names, vct_measure_names)

# There are two columns resulting in 2^2 = 4 combinations...
# We now create 2 of these combinations
lst_combinations <- fn_create_column_combinations(vct_dim_names)

# Create a list of data frames Each list element is a data frame
lst_aggregations <- lapply(lst_combinations, function(x) 
	fn_create_comb_aggregates(df_base_aggregates, x, lst_sum_clause))


# ASSERT: length(lst_aggregations) == (2^length(vct_dim_names)) - 2
v_length <- length(vct_dim_names)
stopifnot(length(lst_aggregations) == sum(choose(v_length, 1:(v_length-1))))
rm(v_length)


# Combine the list of data frames into a single data frame
df_aggregations <- do.call(bind_rows, lst_aggregations) %>% as.data.frame()


# ASSERT: There are no NA's that have been introducted as a result of the above
stopifnot(nrow(df_aggregations[is.na(df_aggregations$Total_Activities),]) == 0)


# grand totals (This is a single row grand total)
df_totals <- df_base_aggregates %>% ungroup() %>% 
				summarise_(.dots = lst_sum_clause) %>% 
				fn_create_df_with_all(vct_dim_names, vct_col_sort)


# clean up
rm(fn_create_column_combinations, fn_create_comb_aggregates, fn_create_df_with_all)
rm(fn_create_year_end, lst_aggregations, lst_sum_clause, vct_col_sort)


#=============================================================================

# Combine all aggregates into a single data frame
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
# Create lookups for the dimension tables
# (1) Create YE Look ups
df_lu_YE <- fn_create_YE_lookup(df_fin$Year_Ending) %>% as.data.frame()

# (2) Create lookup for Activity_Grouo
vct_AG_desc <- c(sort(unique(df_fin[df_fin$Activity_Group != "All", "Activity_Group"])), "All")
vct_codes <- 1:length(vct_AG_desc)
df_lu_Activity_Group <- data.frame(Code = vct_codes, Description = vct_AG_desc,  SortOrder = vct_codes)

# clean up
rm(vct_AG_desc, vct_codes)


# Based on the lookup tables created, replace string values with numeric lookups 
df_fin_lu <- df_fin %>% 
			inner_join(df_lu_YE, by = c("Year_Ending" = "YE")) %>% 
			mutate(Year_Ending = Code) %>% 
			select(-c(Code, SortOrder, Description)) %>%		

			inner_join(df_lu_Activity_Group, by = c("Activity_Group" = "Description")) %>% 
			mutate(Activity_Group = Code) %>% 
			select(-c(Code, SortOrder)) %>%
			as.data.frame()

# ASSERT that all rows joined rows in the lookup data frame should be equal to original
stopifnot(nrow(df_fin_lu) == nrow(df_fin))

# Create hierarchy data frames (a specific STATS NZ format for each dimension)
df_dh_YE <- fn_create_dim_hierarchy(df_lu_YE)
df_dh_Activity_Group <- fn_create_dim_hierarchy(df_lu_Activity_Group)

# For dimension levels, STATS NZ (wisely) want the character variables wrapped in "quotes"
df_lu_YE$Description <- fn_quote(df_lu_YE$Description)
df_lu_Activity_Group$Description <- fn_quote(df_lu_Activity_Group$Description)

# For some reason, stats NZ need a SortOrder column which is blank
df_lu_YE$SortOrder <- ""
df_lu_Activity_Group$SortOrder <- ""

# Drop YE columns...we used it for a join but not needed anymore
df_lu_YE$YE <- NULL

#=============================================================================
# INDEX Files
# These are used to map column names to human readable descriptions. 
# They are used in the following outputs: DimensionIndex.csv, FileIndex.csv, MeasureIndex.csv

# DimensionIndex.csv - create human readable names, wrap all character values in quotes (ie. fn_quote())
vct_dim_title <- fn_quote(c("Year ending", "Activity group"))
df_dimension_index <- data.frame(DimensionCode = fn_quote(vct_dim_names), DimensionTitle = vct_dim_title)

# MeasureIndex.csv - create human readable names, wrap all character valvues in quotes (ie. fn_quote())
vct_measure_title <- fn_quote(c("Total activities"))
df_measure_index <- data.frame(MeasureCode = fn_quote(vct_measure_names), MeasureTitle = vct_measure_title)

# FileIndex.csv data frame
df_file_index <- data.frame(TableID = "7581",
							TableCode = "TABLECODE7581",
							TableTitle = "Domestic Travel Survey: Activities",
							TableFileName = "",
							TableURL = "")

# clean up
rm(df_consolidated, fn_create_dim_hierarchy, fn_create_YE_lookup)

# HERE on Friday 20th November

#=============================================================================
# (9)  FILE OUTPUT
# (9.1) prepare a list containing all dataframes (to be csv files) that will be output
# the order doesn't matter...but for the sake of accuracy, lets impose some structure:
# data, dimension_lookups, dimension_hierarchies, indexes (dimension, measure, file)
# total files: (2 * number dimensions) + 4 .  In this case 14 files

# ACHTUNG - IMPORTANT  Order of the following is important as it needs to align with..
# assigning names

lst_output <- 	
	list(
	# the data
	df_fin_lu, 
	# the dimension lookups (in column order)
	df_lu_YE, df_lu_Activity_Group,
	# the dimension hierarchies (in column order)
	df_dh_YE, df_dh_Activity_Group, 
	# the indexes (dimension, measure and then file)
	df_dimension_index, df_measure_index, df_file_index)


# clean up
rm(df_fin_lu, df_lu_YE, df_lu_Activity_Group, df_dh_YE, df_dh_Activity_Group)


# (9.2) give the list some meaningful names that will be used as file names
# use the information contained in the previous data.frames to encourage consistency

data_name <- paste0("Data", df_file_index$TableID)

 
vct_dimension_file_names <- 
	paste0("DimenLookup", gsub("(\"|_)","",df_dimension_index$DimensionCode), df_file_index$TableID)

vct_hierarchy_file_names <- 
	paste0("DimenHierarchy", gsub("(\"|_)", "", df_dimension_index$DimensionCode), df_file_index$TableID)






# these are always the same
vct_index_names <- c("DimensionIndex", "MeasureIndex", "FileIndex")


# assemble the above into a single vector and assign to the list
vct_list_names <- c(data_name, vct_dimension_file_names, vct_hierarchy_file_names, vct_index_names)


names(lst_output) <- vct_list_names


# clean up
rm(data_name, vct_dim_names, vct_hierarchy_file_names , vct_dimension_file_names, 
		vct_index_names, vct_list_names, vct_measure_names)





# (9.3) prepare / create output directory
sub_path_to_output <- paste0("outputs", "/", "Table_", df_file_index$TableID)


curr_path <- getwd()
str_full_path <- file.path(curr_path, sub_path_to_output) 


# if the file path does not exist then create it                            
if (!file.exists(str_full_path)) dir.create(str_full_path)


#....and finally write the entire list to a set of csv files.
# write the list of data.frames as csv files to "str_full_path"
# invisible() supresses console output.
invisible(lapply(seq_along(lst_output), 
		function(i) {     
			curr_file_name <- paste0(names(lst_output)[i],".csv")
			full_file_name <- paste0(file.path(str_full_path,curr_file_name))
			write.table(lst_output[[i]], full_file_name, sep = ",", 
				row.names = FALSE, quote = FALSE)
		}))

# NUMBER OF FILES PRODUCED:
# (2 * Number Dimensions ) + 4 = 8

# clean up
rm(curr_path, df_dimension_index, df_file_index, df_measure_index, 
		lst_output, str_full_path, sub_path_to_output)

# clean up
rm(df_fin, df_four_quarters, df_trips, df_YE_all, YE)






