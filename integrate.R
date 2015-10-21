list = ls()
library(lubridate)
library(dplyr)

options(stringsAsFactors = FALSE)
setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')

source('functions/fn_calc_lagged_date.R')
source('functions/fn_calc_los.R')
source('functions/fn_create_column_combinations.R')
source('functions/fn_create_df_with_all.R')
source('functions/fn_create_comb_aggregates.R')


# load some data
df_trips <- read.csv("data/vw_DTSTrips.csv", header = TRUE)
df_accomodation <- read.csv("data/vw_DTSVisitAccommodation.csv", header = TRUE)
   

# delete uneeded columns & include only "Overnight trip" as.. these have associated Accomodation values
df_trips <- df_trips %>% select(c(SurveyResponseID,TripIDNumber,TripType, TripYear,
		TripQtr, DestinationRTO, RespondentWeight, SmoothedTripWeight)) %>%
		filter(TripType == "Overnight Trip")



# add 4 extra columns. These will be used in group by calculations
df_trips <- fn_create_year_end(df_trips)

df_YE_Mar <- df_trips %>% select(TripQtr, YEMar) %>% distinct() %>% rename(YE = YEMar)
df_YE_Jun <- df_trips %>% select(TripQtr, YEJun) %>% distinct() %>% rename(YE = YEJun)
df_YE_Sep <- df_trips %>% select(TripQtr, YESep) %>% distinct() %>% rename(YE = YESep)
df_YE_Dec <- df_trips %>% select(TripQtr, YEDec) %>% distinct() %>% rename(YE = YEDec)



df_YE_all <- df_YE_Mar %>% bind_rows(df_YE_Jun) %>% 
	bind_rows(df_YE_Sep) %>% bind_rows(df_YE_Dec) %>% group_by(YE) %>% 
	summarise(count = n()) %>% filter(count == 4) %>% select(YE)


# include only the necessary stuff for df_accomodation
df_accomodation <- df_accomodation %>% select(TripID, AccommodationType, NoNights)

# calculate length of stay group
df_accomodation$LOS_Group <- fn_calc_los(df_accomodation$NoNights)


# combine the two data frames into one, do some renaming, grouping and aggregating
df_combined <- df_trips %>% 
	inner_join(df_accomodation, by = c("TripIDNumber"  = "TripID")) %>% 
	mutate(TotalNights = NoNights * SmoothedTripWeight, TotalRespondents = 1) %>%
	rename(TotalVisitors = RespondentWeight, TotalTrips = SmoothedTripWeight) %>%
	select(YEDec, YESep, YEJun, YEMar,  LOS_Group, DestinationRTO, 
		AccommodationType, TotalVisitors, TotalTrips, TotalNights, TotalRespondents) %>%
	group_by(YEDec, YESep, YEJun, YEMar, LOS_Group, DestinationRTO, AccommodationType ) %>%
	summarise(TotalVisitors = sum(TotalVisitors), TotalTrips = sum(TotalTrips), 
		TotalNights = sum(TotalNights), TotalRespondents = sum(TotalRespondents))



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


# ==================================================================
# get the names of the dimensions
vct_dim_names <- names(df_base_aggregates)[1:4]
lst_aggregations <- list("sum(Total_Visitors)", "sum(Total_Trips)", 
	"sum(Total_Nights)", "sum(Total_Respondents)")

agg_names <- c("Total_Visitors", "Total_Trips", "Total_Nights", "Total_Respondents")
vct_col_sort <- c(vct_dim_names, agg_names)


lst_combinations <- fn_create_column_combinations(vct_dim_names)

lst_sum_clause <- setNames(lst_aggregations, agg_names)

# send each element of 'lst_combinations' to 'fn_create_comb_aggregates()'
lst_aggregations <- lapply(lst_combinations, function(x) 
	fn_create_comb_aggregates(df_base_aggregates, x, lst_sum_clause))

# combine the list of data frames into a consolidated data frame..
# do.call(function(), list of args) [rows = 71,114]
df.aggregations <- do.call(bind_rows, lst_aggregations)

df_totals <- df_base_aggregates %>% ungroup() %>% 
				summarise_(.dots = lst_sum_clause) %>% 
				fn_create_df_with_all(vct_dim_names, vct_col_sort)

# total rows = 86511 + 71114 + 1 = 157626
df_consolidated <- bind_rows(df_base_aggregates, df.aggregations, df_totals)




#================================

#		1)  add summarise non_standard evaluation
#		2)  iterate through list
#		3)  stack on top
#		4)  need to filter out NAs at the beggining and convert to 0		
#

# create a data frame of unique whole years (there are 53)
# TODO : 1) check raw data on import and convert NA to zero
# 2)  replace RBIND and CBIND with bind_rows and bind(cols)

# need to filter out years..."qryRCode-queryUNION-HAVING4"
# need to work out QEDate



# https://danieljhocking.wordpress.com  /2014/12/03/lags-and-moving-means-in-dplyr/ 
 
