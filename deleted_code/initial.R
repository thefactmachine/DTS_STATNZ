rm(list = ls())
library(lubridate)
library(dplyr)

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')
options(stringsAsFactors = FALSE)
source('functions/fn_create_date.R')
source('functions/fn_create_whole_years.R')
source('functions/fn_calc_lagged_date.R')
source('functions/fn_calculate_component_quarters.R')

# load some data
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)
df_accomodation <- read.csv("data/vw_DTSVisitAccommodation.csv", header = TRUE)

# create a quarter end date from quarter number and year
df_trips$QEDate <- fn_create_date(df_trips$TripQtr, df_trips$TripYear)

# create a data frame of unique whole years
df_whole_years <- fn_create_whole_years(df_trips)

# a specific year end consists of 4 component quarters. These are time contiguous
vct_component_quarters <- fn_calculate_component_quarters(df_whole_years)

# deleted uneeded columns & include only "Overnight trip" as..
# only these have associated Accomodation.
df_trips <- df_trips %>% select(c(SurveyResponseID,TripIDNumber,TripType, QEDate,TripYear,
		TripQtr, DestinationRTO, RespondentWeight, SmoothedTripWeight)) %>%
		filter(TripType == "Overnight Trip")

# include only the necessary stuff for df_accomodation
df_accomodation <- df_accomodation %>% select(TripID, AccommodationType, NoNights)

aa <- df_trips %>% inner_join(df_accomodation, by = c("TripIDNumber"  = "TripID")) 
	

# https://danieljhocking.wordpress.com/2014/12/03/lags-and-moving-means-in-dplyr/ 
 

zz <- df_trips %>% filter(QEDate %in% vct_component_quarters)









		





