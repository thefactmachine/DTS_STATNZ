(list = ls())
library(lubridate)
library(dplyr)

options(stringsAsFactors = FALSE)
setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')
source('functions/fn_create_date.R')
source('functions/fn_create_whole_years.R')
source('functions/fn_calc_lagged_date.R')
source('functions/fn_calculate_component_quarters.R')
source('functions/fn_create_year_end.R')
source('functions/fn_calc_los_group.R')




# load some data
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)
df_accomodation <- read.csv("data/vw_DTSVisitAccommodation.csv", header = TRUE)
   

# create a quarter end date from quarter number and year
df_trips$QEDate <- fn_create_date(df_trips$TripQtr, df_trips$TripYear)

# create a data frame of unique whole years (there are 53)
df_whole_years <- fn_create_whole_years(df_trips)

# a specific year end consists of 4 component quarters. These are time contiguous  (there are 56)
vct_component_quarters <- fn_calculate_component_quarters(df_whole_years)


# delete uneeded columns & include only "Overnight trip" as.. these have associated Accomodation values
df_trips <- df_trips %>% select(c(SurveyResponseID,TripIDNumber,TripType, QEDate,TripYear,
		TripQtr, DestinationRTO, RespondentWeight, SmoothedTripWeight)) %>%
		filter(TripType == "Overnight Trip")



# add 4 extra columns. These will be used in group by calculations
df_trips <- fn_create_year_end(df_trips)

 
# include only the necessary stuff for df_accomodation
df_accomodation <- df_accomodation %>% select(TripID, AccommodationType, NoNights)

# calculate length of stay group
df_accomodation$LOS_Group <- fn_calc_los_group(df_accomodation$NoNights)



 
# combine the two data frames into one, do some renaming, grouping and aggregating
df_combined <- df_trips %>% 
	inner_join(df_accomodation, by = c("TripIDNumber"  = "TripID")) %>% 
	mutate(TotalNights = NoNights * SmoothedTripWeight, TotalRespondents = 1) %>%
	rename(TotalVisitors = RespondentWeight, TotalTrips = SmoothedTripWeight) %>%
	select(YEDec, YESep, YEJun, YEMar, QEDate, LOS_Group, DestinationRTO, 
		AccommodationType, TotalVisitors, TotalTrips, TotalNights, TotalRespondents) %>%
	group_by(YEDec, YESep, YEJun, YEMar, QEDate, LOS_Group, DestinationRTO, AccommodationType ) %>%
	summarise(TotalVisitors = sum(TotalVisitors), TotalTrips = sum(TotalTrips), 
		TotalNights = sum(TotalNights), TotalRespondents = sum(TotalRespondents))

# create a vector of 4 different columns stacked on top of each other
YE <- c(df_combined$YEDec, df_combined$YESep, df_combined$YEJun, df_combined$YEMar)

# duplicate the same data frame 4 time and stack on top of each other (158188 rows)
df_four_quarters <- rbind(df_combined, df_combined, df_combined, df_combined)


# combine the previously created vector with the stacked data frames  (test is 93734)
df_four_quartersTEST <- cbind(YE, df_four_quarters) %>%
	select(-c(YEDec, YESep, YEJun, YEMar, QEDate))  %>%
	group_by(YE, LOS_Group, DestinationRTO, AccommodationType) %>%
	summarise(TotalVisitors = sum(TotalVisitors), TotalTrips = sum(TotalTrips), 
	TotalNights = sum(TotalNights), TotalRespondents = sum(TotalRespondents))












test <- df_four_quarters %>% filter(QEDate %in% vct_whole_years)
		


		

# TODO : 1) check raw data on import and convert NA to zero
# 2)  replace RBIND and CBIND with bind_rows and bind(cols)

# need to filter out years..."qryRCode-queryUNION-HAVING4"
# need to work out QEDate





aa <- df_combined$NoNights
















	

# https://danieljhocking.wordpress.com  /2014/12/03/lags-and-moving-means-in-dplyr/ 
 

zz <- df_trips %>% filter(QEDate %in% vct_component_quarters)



# example stuff  ===========================================

df_example <- read.csv("workings/exampleData.csv", header = TRUE)

df_example$Quarter_Ending <- as.Date(df_example$Quarter_Ending, "%Y-%m-%d")

 

library(zoo)























