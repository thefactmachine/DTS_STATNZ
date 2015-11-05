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
# (1.1) load trips data
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)


# (1.2) load and clean purpose of visit look up table data. 
# file obtained from "p:\OTSP\SAS\DTS\Classifications\Purpose.xls"
df_purpose_lu <- read.csv("inputs/Purpose.csv", header = TRUE) %>% 
		rename_("POV" = "Trip.Purpose.Description", 
		"POV_Group" = "Trip.Purpose.Group.Description..Estimation.") %>%
		select(POV, POV_Group)
		
		

# clean up some columns
df_trips[is.na(df_trips$SmoothedTripWeight), "SmoothedTripWeight"] <- 0
df_trips[df_trips$DestinationRTO == "Other   ", "DestinationRTO"] <- "Other"

# the following aligns the purpose of visit (POV) values in df_trips to df_purpose_lu
# this is required to join the two tables
df_trips[is.na(df_trips$POV), "POV"] <- "None"
df_trips[df_trips$POV == "REFUSED", "POV"] <- "Refused"
df_trips[df_trips$POV == "Business   "  , "POV"] <- "Business"
df_trips[df_trips$POV == "Other Reason (specify)"  , "POV"] <- "Other"
df_trips[df_trips$POV == "Education Or Study"  , "POV"] <- "Education/Study"
df_trips[df_trips$POV == "Conference Or Convention"  , "POV"] <- "Conference or Convention"
df_trips[df_trips$POV == "Attending wedding/Family Occasion/Funeral"  , "POV"]  <- "Attending Wedding/Family Occasion/Funerals"
df_trips[df_trips$POV == "Visiting relatives"  , "POV"] <- "Visiting Relatives"
df_trips[df_trips$POV == "Participating in sports activity"  , "POV"]  <- "Participation in Sports Activity"
df_trips[df_trips$POV == "Publicised special event"  , "POV"] <- "Publicised Special Event"
	
# ASSERT: df_purpose_lu$POV are all contained in df_trips$POV
stopifnot(nrow(anti_join(df_purpose_lu, df_trips, by = c("POV" = "POV"))) == 0)


# Select relevant columns in df_trips and include df_purpose_lu$POV_Group
df_trips <- df_trips %>% 
			select(TripYear, TripQtr, TripType, SmoothedTripWeight, 
				DestinationRTO, POV, RespondentWeight) %>%
			inner_join(df_purpose_lu,  by = c("POV" = "POV"))


# RECONCILIATION POINT. The following code snippet should reconcile to 
# # P:\OTSP\SAS\DTS\Output\2010Q3\reports\Est_Qtr_Trip_Type_Purpose.xls
 
# df_trips %>% filter(TripYear == 2010 & TripQtr == 3) %>% 
#	group_by(TripType, POV_Group) %>% 
#	summarise(total = sum(SmoothedTripWeight)) %>% 
#	mutate(total = formatC(round(total,0), format="fg", big.mark = ","))

#=============================================================================
# (3) CREATE a data frame of complete years. 
# add 4 extra columns: (YEMar, YEJun, YESep, YEDec) These will be used in group by calculations 
df_trips <- fn_create_year_end(df_trips)

# (3.1) create four data frames with unique year ending values
df_YE_Mar <- df_trips  %>% select(TripQtr, YEMar) %>% distinct() %>% rename(YE = YEMar)
df_YE_Jun <- df_trips  %>% select(TripQtr, YEJun) %>% distinct() %>% rename(YE = YEJun)
df_YE_Sep <- df_trips  %>% select(TripQtr, YESep) %>% distinct() %>% rename(YE = YESep)
df_YE_Dec <- df_trips  %>% select(TripQtr, YEDec) %>% distinct() %>% rename(YE = YEDec)


# (3.2) stack the four data frames; include whole year values; select a single column
# df_YE_all contains a list of quarterly year ends for "complete" years. These are
# years which contain 4 quarters.
df_YE_all <- bind_rows(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec) %>%
			group_by(YE) %>% summarise(count = n()) %>% 
			filter(count == 4) %>% select(YE)
# clean up			
rm(df_YE_Mar, df_YE_Jun, df_YE_Sep, df_YE_Dec)

#=============================================================================
# (4) CALCULATE Quarterly aggregates and rename columns

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
# (5)   CREATE Year Ending aggregates and filter to include full ears
# (5.1) create a vector of 4 different columns stacked on top of each other
YE <- c(df_trips_qtrly$YEDec, df_trips_qtrly$YESep, df_trips_qtrly$YEJun, df_trips_qtrly$YEMar)

# (5.2) duplicate the same dataframe four times and stack on top of each other
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
# Expected totals for day trips and overnight trips: 27,491,016 & 15,981,539 

# Uncomment the following to get the totals above
# df_base_aggregates %>% ungroup() %>% filter(YE == "YESep2010") %>% 
#	group_by(Trip_Type) %>% summarise(trips = sum(Total_Trips)) %>%
#	mutate(trips = formatC(round(trips,0), format="fg", big.mark = ","))

#=============================================================================


#UP TO HERE.....TO DO: aggregate combinations.....

 














#=============================================================================
# (6) CREATE various aggregate combinations
# PREAMBLE for  (6)
# There are four dimenions columns. The total number of group_by combinations of these are:
# 2^4 = 16.  1 of these has been previously created (see 'df_base_aggregates' )...
# the remaining 15 combinations are created below.  Of these 15 combinations, 14 are created
# programmatically (see 6.5). The remaining combination is created as a single line of code (see 6.6)


# (6.1) get the dimenions names
vct_dim_names <- c("YE", "LOS_Group", "Destination_RTO", "Accomodation_Type")

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
		
			inner_join(df_lu_acccom_type, by = c("Accomodation_Type" = "Description")) %>% 
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

# (8.4) create Dimension and Measure Index df's
df_dimension_index <- 
		data.frame(DimensionCode = vct_dim_names, 
		DimensionTitle = gsub("_", " ", vct_dim_names))

df_measure_index <-
		data.frame(MeasureCode = vct_measure_names, 
		MeasureTitle = gsub("_", " ", vct_measure_names))


# (8.5) create file index data frame
df_file_index <- data.frame(TableID = "Accomodation",
							TableCode = "TABLECODEAccomodation",
							TableTitle = "Domestic Travel Survey: Accomodation",
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

data_name <- paste0("data", df_file_index$TableID)

vct_dimension_names <- 
	paste0("DimenLookup", df_dimension_index$DimensionCode, df_file_index$TableID)

vct_hierarchy_names <- 
	paste0("DimHierarchy", df_dimension_index$DimensionCode, df_file_index$TableID)

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
rm(df_consolidated, fn_create_YE_lookup)






