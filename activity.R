# clear everything
rm(list = ls())

# load some libaries 
#library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

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


#========================================================================
# (1) LOAD and Prepare data
# 1,a TRIPS
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)

# nrow 137,081
df_trips <- df_trips %>% 
		select(TripIDNumber, TripYear, TripQtr, TripType, DestinationRTO, SmoothedTripWeight) %>%
		rename(Total_Trips = SmoothedTripWeight)


# 1.b ACTIVITIES
# This function gets an XL spreasheet and cleans some values so it matches with df_activities
df_activity_classification <- fn_get_classification()

# nrow = 340,349
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
# (2) CREATE a data frame of complete years. 
# add 4 extra columns: (YEMar, YEJun, YESep, YEDec) These will be used in group by calculations 
df_combined <- fn_create_year_end(df_combined)



# (2.1) create four data frames with unique year ending values
df_YE_Mar <- df_combined %>% select(TripQtr, YEMar) %>% distinct() %>% rename(YE = YEMar)
df_YE_Jun <- df_combined %>% select(TripQtr, YEJun) %>% distinct() %>% rename(YE = YEJun)
df_YE_Sep <- df_combined %>% select(TripQtr, YESep) %>% distinct() %>% rename(YE = YESep)
df_YE_Dec <- df_combined %>% select(TripQtr, YEDec) %>% distinct() %>% rename(YE = YEDec)


# (3.2) stack the four data frames; include whole year values; select a single column
# df_YE_all contains a list of quarterly year ends for "complete" years. These are
# years which contain 4 quarters.
df_YE_all <- bind_rows(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec) %>%
			group_by(YE) %>% summarise(count = n()) %>% 
			filter(count == 4) %>% select(YE)
# clean up			
rm(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec)


#=============================================================================
# (3) CALCULATE Quarterly aggregates and rename columns

# 16-11-2010  HERE HERE HERE HERE HERE HERE ======================


df_trips_qtrly <- df_trips_combined %>%  
	rename(Destination_RTO = DestinationRTO, Trip_Type = TripType, Spend_Type = SpendType) %>%
	group_by(YEDec, YESep, YEJun, YEMar, Destination_RTO, Trip_Type, POV, POV_Group, Spend_Type) %>%
	summarise(Expenditure = sum(Expenditure))


#=============================================================================
# (5)   CREATE Year Ending aggregates and filter to include full ears
# (5.1) create a vector of 4 different columns stacked on top of each other
YE <- c(df_trips_qtrly$YEDec, df_trips_qtrly$YESep, df_trips_qtrly$YEJun, df_trips_qtrly$YEMar)

# (5.2) duplicate the same dataframe four times and stack on top of each other
df_four_quarters <- rbind(df_trips_qtrly, df_trips_qtrly, df_trips_qtrly, df_trips_qtrly)



df_base_aggregates <- cbind(YE, df_four_quarters) %>%
	select(-c(YEDec, YESep, YEJun, YEMar))  %>%
	group_by(YE, Destination_RTO, Trip_Type, POV, POV_Group, Trip_Type, Spend_Type) %>%
	summarise(Expenditure = sum(Expenditure))  %>%  
	filter(YE %in% df_YE_all$YE)
#=============================================================================

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
				group_by(Trip_Type) %>% summarise(total = sum(Expenditure)) %>%
				mutate(totals = formatC(round(total,0), format="fg", big.mark = ","))


temp_b <- df_base_aggregates %>% filter(YE == "YESep2010") %>% 
				group_by(POV_Group) %>% summarise(total = sum(Expenditure)) %>%
				mutate(totals = formatC(round(total,0), format="fg", big.mark = ","))				


#=============================================================================
#=============================================================================
# (6) CREATE various aggregate combinations
# PREAMBLE for  (6)
# There are five dimenions columns. The total number of group_by combinations from these are:
# 2^4 = 32.  

# 1 of these has been previously created (see 'df_base_aggregates' ). This is:
# group_by(A, B, C, D)
# the remaining 31 combinations are created below.  Of these 31 combinations, 30 are created
# programmatically (see 6.5). The remaining combination is created as a single line of code (see 6.6)


# (6.1) get the dimenions names
vct_dim_names <- names(df_base_aggregates)[1:6]

# get measure names
vct_measure_names <- names(df_base_aggregates)[7]



# (6.2) create a "summarise" clause (for multiple use later)
lst_aggregations <- as.list(paste0("sum(", vct_measure_names,")"))



lst_sum_clause <- setNames(lst_aggregations, vct_measure_names)

# (6.3) sort order of the columns
vct_col_sort <- c(vct_dim_names, vct_measure_names)

# (6.4) There are five columns resulting in 2^5 = 32 combinations...
# we now create 30 of these combinations
lst_combinations <- fn_create_column_combinations(vct_dim_names)

# (6.5) Create a list of data frames Each list element is a data frame
# based on a column combination created in 6.4
lst_aggregations <- lapply(lst_combinations, function(x) 
	fn_create_comb_aggregates(df_base_aggregates, x, lst_sum_clause))

# ASSERT: length(lst_aggregations) == (2^length(vct_dim_names)) - 2
v_length <- length(vct_dim_names)
stopifnot(length(lst_aggregations) == sum(choose(v_length, 1:(v_length-1))))
rm(v_length)


# (6.5.1) combine the list of data frames into a single data frame
# 1,188,514 rows here
df_aggregations <- do.call(bind_rows, lst_aggregations) %>% as.data.frame()

# ASSERT: There are no NA's that have been introducted as a result of the above
stopifnot(nrow(df_aggregations[is.na(df_aggregations$Expenditure),]) == 0)


# (6.6) grand totals (This is a single row grand total)
df_totals <- df_base_aggregates %>% ungroup() %>% 
				summarise_(.dots = lst_sum_clause) %>% 
				fn_create_df_with_all(vct_dim_names, vct_col_sort)


# clean up
rm(fn_create_column_combinations, fn_create_comb_aggregates, fn_create_df_with_all)
rm(fn_create_year_end, lst_aggregations, lst_sum_clause, vct_col_sort)


#=============================================================================

# (7) combine all aggregates into a single data frame
# total rows = 313577 + 1188514  + 1 = 1,502,092
df_consolidated <- bind_rows(df_base_aggregates, df_aggregations, df_totals)



# RECONCILIATION POINT
# reconcile df_aggregations to 8486492593 for "YESep2010" [see above]
# df_aggregations %>% filter(YE == "YESep2010"  & Destination_RTO == "All" & 
# Trip_Type == "All" & POV == "All"  & POV_Group == "All" & Spend_Type == "All" )


# (7.1) convert numeric columns to text with 0 decimal places
df_fin <- sapply(df_consolidated[, vct_measure_names], function(x) fn_convert_to_text(x)) %>%
 			# convert sapply's matrix to a data frame
 			as.data.frame() %>%
 			# club the original columns together with the new text columns
 			bind_cols(df_consolidated[, vct_dim_names, ], .)

#clean up. Leave "df_consolidated" available for reconciliation purposes.
rm(df_base_aggregates, df_totals, df_aggregations, lst_combinations)
rm(fn_convert_to_text)



#=============================================================================
# (8) LOOKUPS

# (8.1) Finally create YE Look ups
df_lu_YE <- fn_create_YE_lookup(df_fin$YE) %>% as.data.frame()


# (8.2) Create lookup Destination RTO (This is pre-built so just load it!)
df_lu_dest_rto <- read.csv("inputs/DimenLookupDestinationRTOAccommodation.csv", header = TRUE)



# (8.3) Create lookup for Trip_Type
vct_Trip_desc <- c(sort(unique(df_fin[df_fin$Trip_Type != "All", "Trip_Type"])), "All")
vct_codes <- 1:length(vct_Trip_desc)
df_lu_Trip_Type <- data.frame(Code = vct_codes, Description = vct_Trip_desc,  SortOrder = vct_codes)
# clean up
rm(vct_Trip_desc, vct_codes)


# (8.4) Create lookup for POV (Purpose of Visit)
# Created sorted list of unique values but with "All" on the end
vct_POV_desc <- c(sort(unique(df_fin[df_fin$POV != "All", "POV"])), "All")
vct_codes <- 1:length(vct_POV_desc)
df_lu_POV <- data.frame(Code = vct_codes, Description = vct_POV_desc, SortOrder = vct_codes)
# clean up
rm(vct_POV_desc, vct_codes)


# (8.5) Create lookup for POV_Group (Purpose of Visit Group)
# Created sorted list of unique values but with "All" on the end
vct_POV_Group_desc <- c(sort(unique(df_fin[df_fin$POV_Group != "All", "POV_Group"])), "All")
vct_codes <- 1:length(vct_POV_Group_desc)
df_lu_POV_Group <- data.frame(Code = vct_codes, Description = vct_POV_Group_desc,  SortOrder = vct_codes)
# clean up
rm(vct_POV_Group_desc, vct_codes)


# (8.6) Finally for Spend_Type
vct_Spend_Type_desc <- c(sort(unique(df_fin[df_fin$Spend_Type != "All", "Spend_Type"])), "All")
vct_codes <- 1:length(vct_Spend_Type_desc)
df_lu_Spend_Type <- data.frame(Code = vct_codes, Description = vct_Spend_Type_desc,  SortOrder = vct_codes)
rm(vct_Spend_Type_desc, vct_codes)







# We get the lookups and the dataset and replace the original string values with numeric values 
# in the lookups:

# note that we try and always do things in column order. This helps prevent mistakes.



df_fin_lu <- df_fin %>% 

			inner_join(df_lu_YE, by = c("YE" = "YE")) %>% 
			mutate(YE = Code) %>% 
			select(-c(Code, SortOrder, Description)) %>%		
			rename(Year_ending = YE) %>%
			
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


# (8.6) create dimension hierarchy from dimension lookups from 8.1 to 8.6
df_dh_YE <- fn_create_dim_hierarchy(df_lu_YE)
df_dh_dest_rto <- fn_create_dim_hierarchy(df_lu_dest_rto)
df_dh_Trip_Type <- fn_create_dim_hierarchy(df_lu_Trip_Type)
df_dh_POV <- fn_create_dim_hierarchy(df_lu_POV)
df_dh_POV_Group <- fn_create_dim_hierarchy(df_lu_POV_Group)
df_dh_Spend_Type <- fn_create_dim_hierarchy(df_lu_Spend_Type)



# (8.7) create Dimension and Measure Index df's
df_dimension_index <- 
		data.frame(DimensionCode = vct_dim_names, 
		DimensionTitle = gsub("_", " ", vct_dim_names))


df_measure_index <-
		data.frame(MeasureCode = vct_measure_names, 
		MeasureTitle = gsub("_", " ", vct_measure_names))




# (8.8) create file index data frame
df_file_index <- data.frame(TableID = "Spend",
							TableCode = "TABLECODESpend",
							TableTitle = "Domestic Travel Survey: Spend",
							TableFileName = "",
							TableURL = "")

# clean up
rm(df_consolidated, fn_create_dim_hierarchy, fn_create_YE_lookup)



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
	df_lu_YE, df_lu_dest_rto, df_lu_Trip_Type, df_lu_POV, df_lu_POV_Group, df_lu_Spend_Type,
	# the dimension hierarchies (in column order)
	df_dh_YE, df_dh_dest_rto, df_dh_Trip_Type, df_dh_POV, df_dh_POV_Group, df_dh_Spend_Type,
	# the indexes (dimension, measure and then file)
	df_dimension_index, df_measure_index, df_file_index)


# clean up
rm(df_fin_lu, df_lu_POV_Group, df_lu_POV, df_lu_Trip_Type, df_lu_dest_rto, 
df_lu_YE, df_dh_POV_Group, df_dh_POV, df_dh_Trip_Type, df_dh_dest_rto, df_dh_YE)



# (9.2) give the list some meaningful names that will be used as file names
# use the information contained in the previous data.frames to encourage consistency


data_name <- paste0("data", df_file_index$TableID)
 
vct_dimension_names <- 
	paste0("DimenLookup", df_dimension_index$DimensionCode, df_file_index$TableID)

	
vct_hierarchy_names <- 
	paste0("DimHierarchy", df_dimension_index$DimensionCode, df_file_index$TableID)


# these are always the same
vct_index_names <- c("DimensionIndex", "MeasureIndex", "FileIndex")


# assemble the above into a single vector and assign to the list
vct_list_names <- c(data_name, vct_dimension_names, vct_hierarchy_names, vct_index_names) 

names(lst_output) <- vct_list_names


# clean up
rm(data_name, vct_dim_names, vct_hierarchy_names, vct_dimension_names, 
		vct_index_names, vct_list_names, vct_measure_names)






# (9.3) prepare / create output directory

sub_path_to_output <- paste0("outputs", "/", df_file_index$TableID)
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


# clean up
rm(curr_path, df_dimension_index, df_file_index, df_measure_index, 
		lst_output, str_full_path, sub_path_to_output)

# clean up
rm(df_fin, df_four_quarters, df_purpose_lu, df_trips, df_trips_qtrly, df_YE_all, YE)






