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

options(stringsAsFactors = FALSE)
# do not display in scientific notation
options(scipen=999, digits = 10)

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')

# creates a data.frame of column combinations for use in a group_by
source('functions/fn_create_column_combinations.R')
# uses column combinations to create various group_by
source('functions/fn_create_comb_aggregates.R')
# appends columns to a data.frame such that resultant df is always 8 columns wide
source('functions/fn_create_df_with_all.R')
# appends a data.frame with four columns
source('functions/fn_create_year_end.R')

# converts floating point number to text with two decimals
source('functions/fn_convert_to_text_two_decimals.R')

# creates  yearend lookup table
source('functions/fn_create_YE_lookup.R')
# takes a dimension lookup and creates a dimension hierarchy
source('functions/fn_create_dim_hierarchy.R')
# this is a collection of statements that changes a few values so it can join with "df_purpose_lu"
source('functions/fn_clean_trips.R')
# this function cleans up "vw_DTSTripSpend.csv" It removes "alcohol" from "Food and Alcohol"
source('functions/fn_clean_spend.R')
# takes a character vector and surrounds it with quotes hi --> "hi"
source('functions/fn_quote.R')


# PLAN OF ATTACK =====================================================================================

# BASIC CODE STEPS
# LOAD and process two CSV files (Quarterly data)
# AGGREGATE - From the quarterly data, aggregate these into quarterly year-ending summaries.
# FILTER the year ending summaries such that only whole years are included (first 3 quarters are not included)
# CREATE satellite tables and formats that are specifically required by Stats NZ - STATS NZ require data to be
# in a start schema configuration.

# ====================================================================================================

# LOAD data
# RELATIONSHIP BETWEEN TRIPS AND TRIP_SPEND
# trips has 137081 rows trip_spend contains six different categories. Each of these categories
# has 137081 rows.  The number of rows in trip_spend is 6 * 137081 = 822486
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)


# Load and clean purpose of visit look up table data. 
# file obtained from "p:\OTSP\SAS\DTS\Classifications\Purpose.xls"
df_purpose_lu <- read.csv("inputs/POV_to_POV_Group.csv", header = TRUE) %>% 
		rename_("POV" = "Trip.Purpose.Description", 
		"POV_Group" = "Trip.Purpose.Group.Description..Estimation.") %>%
		select(POV, POV_Group)

df_trips <- fn_clean_trips(df_trips, df_purpose_lu)


# ASSERT: df_purpose_lu$POV are all contained in df_trips$POV
stopifnot(nrow(anti_join(df_purpose_lu, df_trips, by = c("POV" = "POV"))) == 0)

df_trip_spend <- read.csv("data/vw_DTSTripSpend.csv" , header = TRUE) %>% 
			filter(is.na(TripID) != TRUE)

# in the original "vw_DTSTripSpend.csv" data, SpendType has 6 levels. We now get
# accomodation and transport from "vw_DTSTrips.csv" and create 2 additional levels.
# also "alcohol" is taken out from "food and alcohol" to prevent double counting "alcohol"
df_spend_combined <- fn_clean_spend(df_trip_spend, df_trips)

# clean off unecessary columns from df_trips.
df_trips <- df_trips %>% select(-ExpenditureWeight, -ImputedSpendAccom, 
				-ImputedSpendTrnsport, -Accom_Spend, -Trans_Spend)


# join the two tables together....expecting 1050260 rows
df_trips_combined 	<- df_trips %>% inner_join(df_spend_combined, 
						by = c("TripIDNumber" = "TripIDNumber"))


#=============================================================================
# CREATE a data frame of complete years. 

# add 4 extra columns. These will be used in group by calculations to calculate YE aggregates
df_trips_combined <- fn_create_year_end(df_trips_combined)

# create four data frames with unique year ending values
df_YE_Mar <- df_trips_combined %>% select(TripQtr, YEMar) %>% distinct() %>% rename(YE = YEMar)
df_YE_Jun <- df_trips_combined %>% select(TripQtr, YEJun) %>% distinct() %>% rename(YE = YEJun)
df_YE_Sep <- df_trips_combined %>% select(TripQtr, YESep) %>% distinct() %>% rename(YE = YESep)
df_YE_Dec <- df_trips_combined %>% select(TripQtr, YEDec) %>% distinct() %>% rename(YE = YEDec)


# Stack the four data frames; include whole year values; select a single column
df_YE_all <- bind_rows(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec) %>%
			group_by(YE) %>% summarise(count = n()) %>% 
			filter(count == 4) %>% select(YE)
# clean up			
rm(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec)
rm(df_purpose_lu, df_spend_combined, df_trip_spend, df_trips)

#=============================================================================
# CALCULATE quarterly aggregates and rename columns
df_trips_qtrly <- df_trips_combined %>%  
	rename(Destination_RTO = DestinationRTO, Trip_Type = TripType, Spend_Type = SpendType) %>%
	group_by(YEDec, YESep, YEJun, YEMar, Destination_RTO, Trip_Type, POV, POV_Group, Spend_Type) %>%
	summarise(Expenditure = sum(Expenditure))


#=============================================================================
# CREATE Year Ending aggregates and filter to include full ears

# Create a vector of 4 different columns stacked on top of each other
YE <- c(df_trips_qtrly$YEDec, df_trips_qtrly$YESep, df_trips_qtrly$YEJun, df_trips_qtrly$YEMar)

# Duplicate the same dataframe four times and stack on top of each other
df_four_quarters <- rbind(df_trips_qtrly, df_trips_qtrly, df_trips_qtrly, df_trips_qtrly)

# combine the previously created vector with the stacked data frames 
df_base_aggregates <- cbind(YE, df_four_quarters) %>%
	select(-c(YEDec, YESep, YEJun, YEMar))  %>%
	group_by(YE, Destination_RTO, Trip_Type, POV, POV_Group, Trip_Type, Spend_Type) %>%
	summarise(Expenditure = sum(Expenditure))  %>%  
	filter(YE %in% df_YE_all$YE)


#=============================================================================
# RECONCILIATION POINT. df_base_aggregates contains quarterly year ending values
# this means that each row is the sum of 4 quarters.  To reconcile these, four source
# files were aggregated. The four source files were:
# P:\OTSP\SAS\DTS\Output\2009Q4\reports\Est_Qtr_Expend_Type_Item_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q1\reports\Est_Qtr_Expend_Type_Item_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q2\reports\Est_Qtr_Expend_Type_Item_Purpose.xls
# P:\OTSP\SAS\DTS\Output\2010Q3\reports\Est_Qtr_Expend_Type_Item_Purpose.xls
# These four quarterly reports have been manually consolidated here:
# "/workings/spend_YE_2010_Q3/YE_2010_Q3_Consolidated.xlsx"
# total for all categories should be:  8,486,492,593

temp_a 	<- df_base_aggregates %>% filter(YE == "YESep2010") %>% 
				group_by(Trip_Type) %>% summarise(total = sum(Expenditure))

temp_b <- df_base_aggregates %>% filter(YE == "YESep2010") %>% 
				group_by(POV_Group) %>% summarise(total = sum(Expenditure))
				
# ASSERT: minimal differences between temp_a and temp_b
stopifnot((sum(temp_a$total) - sum(temp_b$total)) < 0.5)
# ASSERT that totals are approximately equal to 8,486,492,593
stopifnot((sum(temp_a$total) - 8486492593) < 0.5)
rm(temp_a, temp_b)


#=============================================================================
# CREATE various aggregate combinations
# There are six dimenions columns. The total number of group_by combinations of these are:
# 2^6 = 64.  1 of these has been previously created (see 'df_base_aggregates' )...
# the remaining 63 combinations are created below.  Of these 63 combinations, 62 are created
# programmatically using a function call. The remaining combination is created as a single line of code

# Assign the measure and dimension names...these will be used for column headings and also to:
# create values for DimensionIndex.csv and MeasureIndex.csv files and to create filenames.


# Assign the measure and dimension names...these will be used for column headings and also to:
# create values for DimensionIndex.csv and MeasureIndex.csv files and to create filenames.
vct_dim_names <- c("Year_Ending", "Destination_RTO", "Trip_Type", "POV", "POV_Group", "Spend_Type")
names(df_base_aggregates)[1:6] <- vct_dim_names


vct_measure_names <- c("Expenditure")
names(df_base_aggregates)[7] <- vct_measure_names

# Create a "summarise" clause (for multiple use later)
lst_aggregations <- as.list(paste0("sum(", vct_measure_names,")"))
lst_sum_clause <- setNames(lst_aggregations, vct_measure_names)

# Sort order of the columns
vct_col_sort <- c(vct_dim_names, vct_measure_names)


# There are four columns resulting in 2^6 = 64 combinations...
# we now create 62 of these
lst_combinations <- fn_create_column_combinations(vct_dim_names)

# Create a list of data frames Each list element is a data frame
lst_aggregations <- lapply(lst_combinations, function(x) 
	fn_create_comb_aggregates(df_base_aggregates, x, lst_sum_clause))

# ASSERT: length(lst_aggregations) == (2^length(vct_dim_names)) - 2
v_length <- length(vct_dim_names)
stopifnot(length(lst_aggregations) == sum(choose(v_length, 1:(v_length-1))))
rm(v_length)


# Combine the list of data frames into a single data frame 1,188,514 rows here
df_aggregations <- do.call(bind_rows, lst_aggregations) %>% as.data.frame()

# ASSERT: There are no NA's that have been introducted as a result of the above
stopifnot(nrow(df_aggregations[is.na(df_aggregations$Expenditure),]) == 0)

# Grand totals (This is a single row grand total)
df_totals <- df_base_aggregates %>% ungroup() %>% 
				summarise_(.dots = lst_sum_clause) %>% 
				fn_create_df_with_all(vct_dim_names, vct_col_sort)


# clean up
rm(fn_create_column_combinations, fn_create_comb_aggregates, fn_create_df_with_all)
rm(fn_create_year_end, lst_aggregations, lst_sum_clause, vct_col_sort)


# combine all aggregates into a single data frame
df_consolidated <- bind_rows(df_base_aggregates, df_aggregations, df_totals)




# ASSERT: No NAs bave been introduced before converting to text.
stopifnot(sum(is.na(df_consolidated)) == 0)


# Convert numeric columns to text with 0 decimal places
df_fin <- sapply(df_consolidated[, vct_measure_names], function(x) fn_convert_to_text_two_decimals(x)) %>%
 			# convert sapply's matrix to a data frame
 			as.data.frame() %>%
 			# club the original columns together with the new text columns
 			bind_cols(df_consolidated[, vct_dim_names, ], .)

#clean up. Leave "df_consolidated" available for reconciliation purposes.
rm(df_base_aggregates, df_totals, df_aggregations, lst_combinations)
rm(fn_convert_to_text_two_decimals)



#=============================================================================
# LOOKUPS
# Create 6 lookups for each of the dimension variables
# (1) Create YE Look ups
df_lu_YE <- fn_create_YE_lookup(df_fin$Year_Ending) %>% as.data.frame()

# (2) Create lookup Destination RTO (This is pre-built so just load it!)
df_lu_dest_rto <- read.csv("inputs/DimenLookupDestinationRTOAccommodation.csv", header = TRUE)

# (3) Create lookup for Trip_Type
vct_Trip_desc <- c(sort(unique(df_fin[df_fin$Trip_Type != "All", "Trip_Type"])), "All")
vct_codes <- 1:length(vct_Trip_desc)
df_lu_Trip_Type <- data.frame(Code = vct_codes, Description = vct_Trip_desc,  SortOrder = vct_codes)
# clean up
rm(vct_Trip_desc, vct_codes)

# (4) Create lookup for POV (Purpose of Visit)
vct_POV_desc <- c(sort(unique(df_fin[df_fin$POV != "All", "POV"])), "All")
vct_codes <- 1:length(vct_POV_desc)
df_lu_POV <- data.frame(Code = vct_codes, Description = vct_POV_desc, SortOrder = vct_codes)
# clean up
rm(vct_POV_desc, vct_codes)

# (5) Create lookup for POV_Group (Purpose of Visit Group)
vct_POV_Group_desc <- c(sort(unique(df_fin[df_fin$POV_Group != "All", "POV_Group"])), "All")
vct_codes <- 1:length(vct_POV_Group_desc)
df_lu_POV_Group <- data.frame(Code = vct_codes, Description = vct_POV_Group_desc,  SortOrder = vct_codes)
# clean up
rm(vct_POV_Group_desc, vct_codes)

# (6) Finally for Spend_Type
vct_Spend_Type_desc <- c(sort(unique(df_fin[df_fin$Spend_Type != "All", "Spend_Type"])), "All")
vct_codes <- 1:length(vct_Spend_Type_desc)
df_lu_Spend_Type <- data.frame(Code = vct_codes, Description = vct_Spend_Type_desc,  SortOrder = vct_codes)
rm(vct_Spend_Type_desc, vct_codes)

# Based on the lookup tables created, replace string values with numeric lookups 
# note that we try and always do things in column order. This helps prevent mistakes.

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
			
			inner_join(df_lu_Spend_Type, by = c("Spend_Type" = "Description")) %>% 
			mutate(Spend_Type = Code) %>% 
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
df_dh_Spend_Type <- fn_create_dim_hierarchy(df_lu_Spend_Type)

# For dimension levels, STATS NZ (wisely) want the character variables wrapped in "quotes"
df_lu_YE$Description <- fn_quote(df_lu_YE$Description)
df_lu_dest_rto$Description <- fn_quote(df_lu_dest_rto$Description)
df_lu_Trip_Type$Description <- fn_quote(df_lu_Trip_Type$Description)
df_lu_POV$Description <- fn_quote(df_lu_POV$Description)
df_lu_POV_Group$Description <- fn_quote(df_lu_POV_Group$Description)
df_lu_Spend_Type$Description <- fn_quote(df_lu_Spend_Type$Description)

# For some reason, stats NZ need a SortOrder column which is blank
df_lu_YE$SortOrder <- ""
df_lu_dest_rto$SortOrder <- ""
df_lu_Trip_Type$SortOrder <- ""
df_lu_POV$SortOrder <-  ""
df_lu_POV_Group$SortOrder <- ""
df_lu_Spend_Type$SortOrder <- ""

# Drop YE columns...we used it for a join but not needed anymore
df_lu_YE$YE <- NULL


#=============================================================================
# INDEX Files
# These are used to map column names to human readable descriptions. 
# They are used in the following outputs: DimensionIndex.csv, FileIndex.csv, MeasureIndex.csv

# DimensionIndex.csv - create human readable names, wrap all character values in quotes (ie. fn_quote())

vct_dim_title <- fn_quote (c("Year ending", "Regional tourism organisation", 
			"Trip type" , "Purpose of visit", "Purpose of visit group", "Spend type"))

df_dimension_index <- data.frame(DimensionCode = fn_quote(vct_dim_names), DimensionTitle = vct_dim_title)

vct_measure_title <- fn_quote(c("Expenditure"))
df_measure_index <- data.frame(MeasureCode = fn_quote(vct_measure_names), MeasureTitle = vct_measure_title)

# Create file index data frame
df_file_index <- data.frame(TableID = "7580",
							TableCode = "TABLECODE7580",
							TableTitle = "Domestic Travel Survey: Visitor Spend",
							TableFileName = "",
							TableURL = "")
# clean up
rm(df_consolidated, fn_create_dim_hierarchy, fn_create_YE_lookup)


#=============================================================================
# FILE OUTPUT (output various data frames to csv files)
# prepare a named list of data frames. The names will be used to generate file names
# The order: data, lookups, dimension hierarchies, index files (dimension, measure, file) [use column order]

lst_output <- 	
				list(
				# the data
				df_fin_lu, 
				# the dimension lookups (in column order)
				df_lu_YE, df_lu_dest_rto, df_lu_Trip_Type, df_lu_POV, df_lu_POV_Group, df_lu_Spend_Type,
				# the dimension hierarchies (in column order)
				df_dh_YE, df_dh_dest_rto, df_dh_Trip_Type, df_dh_POV, df_dh_POV_Group, df_dh_Spend_Type,
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
names(lst_output) <- c(data_name, vct_dimension_file_names, vct_hierarchy_file_names, vct_index_names)


rm(data_name, vct_dim_names, vct_hierarchy_file_names , vct_dimension_file_names, 
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


# clean up
rm(curr_path, df_dimension_index, df_file_index, df_measure_index, 
		lst_output, str_full_path, sub_path_to_output)

# clean up
rm(df_fin, df_four_quarters, df_trips_qtrly, df_YE_all, YE)






