
rm(list = ls())
library(lubridate)
library(dplyr)

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')
options(stringsAsFactors = FALSE)
source('functions/fn_create_date.R')
source('functions/fn_create_whole_years.R')

df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)
df_accomodation <- read.csv("data/vw_DTSVisitAccommodation.csv", header = TRUE)

# create a quarter end date from quarter number and year
df_trips$QEDate <- fn_create_date(df_trips$TripQtr, df_trips$TripYear)

# create a data frame of unique whole years
df_whole_years <- fn_create_whole_years(df_trips)





mark <- df_trips %>% select(c(SurveyResponseID,TripIDNumber,TripType, QEDate,TripYear,
TripQtr, DestinationRTO, RespondentWeight, SmoothedTripWeight))



		








unDate <- sort(unique(mark$QEDate))
unDate <- unDate[-c(56)]

df.date <- data.frame(date = unDate)
df.test <- df.date %>% mutate(lagged = lag(date, 3), less9m = fn_calc_lagged_date(date))

df.clean.date <- 


data.frame(date = df.test[df.test$lagged == df.test$less9m & 

!is.na(df.test$lagged), "date"])



















 

  
  
  
  


