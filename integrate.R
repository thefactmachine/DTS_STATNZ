##
##    Programme Name:  	integrate.R
##	
##    Objective:      	To call four scripts that create various Domestic Travel Survey outputs.
##
##
##    Author: 		   	Mark Hatcher (Sector Trends, December, 2015)


# clear everything
rm(list = ls())

# supress warnings
options(warn=-1)

# create a string based on date (YYYYMMDD) and time (HHMM) for current folder
str_cur_dir <- format(Sys.time(), "%Y%m%d-%H%M")

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')

system.time(source('7578_create_accommodation.R', echo = FALSE))
system.time(source('7579_create_trips.R', echo = FALSE))
system.time(source('7580_create_spend.R', echo = FALSE))
system.time(source('7581_create_activity.R', echo = FALSE))


# turn warnings back on
options(warn=0)