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

# (8.4.a) create Dimension and Measure Index df's


vct_dim_names
vct_measure_names




# clean up
rm(df_consolidated, fn_create_YE_lookup)



# SUMMARY OF OUTPUT TABLES:
#1	Data
# example "data7571"

#2	Dimension lookups (one per dimension - mapping of dimension code to dimension value) 
# example = "DimenLookupAgeRange7571"

#3  Dimension hierarchy (maps total to details) (one per dimension)
# example = "DimenHierarchyAgeRange7571"

#4  MeasureIndex (MeasureCode and Measure Title) Eg 4 rows. "Total Visitors", "Total_Visitors"
# example = "MeasureIndex"

#5  DimensionIndex (DimensionCode, DimensionTitle)
# example = "DimensionIndex"

#5  FileIndex ???





# WORKOUT  Parent / Child
# get "df_lu_YE" in the correct format.




