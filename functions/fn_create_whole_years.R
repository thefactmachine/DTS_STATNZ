fn_create_whole_years <- function(a_df_trips) {

	
	df_return <- 
		a_df_trips %>% select(QEDate, TripYear, TripQtr) %>% distinct() %>% 
		arrange() %>% 
		mutate(lagged = lag(QEDate, 3), 
		less9m = fn_calc_lagged_date(QEDate)) %>% 
		filter(lagged == less9m & !is.na(lagged)) %>% 
		rename(whole_year = QEDate) 	
	return(df_return)

}


