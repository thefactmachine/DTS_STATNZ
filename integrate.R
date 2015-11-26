##
##    Programme Name:    integrate.R
##
##    Objective:         To call four scripts that create various Domestic Travel Survey outputs.
##
##
##    Author:            Mark Hatcher (Sector Trends, December, 2015)


# clear everything
rm(list = ls())

# create a string based on date (YYYYMMDD) and time (HHMM) for current folder
str_cur_dir <- format(Sys.time(), "%Y%m%d-%H%M")

# this line may need to be changed depending on where the code is run from.
setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')

#setwd('E:\\dts_stats_nz_export')


# the following R scripts uses data from the following sub directories:

# functions/                    "various functions used by source files"

# inputs/                       "various csv files used for dimension lookups or concordances"

# data/                          "four data tables obtained from the following location (as at 31-08-2015)
# http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/domestic-travel-survey/data-download
# as the DTS surveyed is discontinued, it is considered uncessary to obtained the data from a dynamic source such as TRED.
# for the sake of performance it may be better to read the fairly large amount of data locally than accross a network


# a script for each data extract
system.time(source('7578_create_accommodation.R', echo = FALSE))
system.time(source('7579_create_trips.R', echo = FALSE))
system.time(source('7580_create_spend.R', echo = FALSE))
system.time(source('7581_create_activity.R', echo = FALSE))


