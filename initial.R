
rm(list = ls())
# library(lubridate)
library(dplyr)

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')
options(stringsAsFactors = FALSE)
source('functions/fn_create_date.R')

df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)
df_accomodation <- read.csv("data/vw_DTSVisitAccommodation.csv", header = TRUE)

# create a quarter end date from quarter number and year
df_trips$QEDate <- fn_create_date(df_trips$TripQtr, df_trips$TripYear)


mark <- df_trips %>% select(c(SurveyResponseID,TripIDNumber,TripType, QEDate,TripYear,
TripQtr, DestinationRTO, RespondentWeight, SmoothedTripWeight))


 