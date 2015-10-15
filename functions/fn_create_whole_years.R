fn_create_whole_years <- function(a_df_trips) {

	fn_calc_lagged_date <- function(dte_date) {
		return(ceiling_date(dte_date %m-% months(9), unit = "month") - days(1))
	}

	df_return <- 
		a_df_trips %>% select(QEDate) %>% distinct() %>% 
		arrange() %>% 
		mutate(lagged = lag(QEDate, 3), 
		less9m = fn_calc_lagged_date(QEDate)) %>% 
		filter(lagged == less9m & !is.na(lagged)) %>% 
		rename(whole_year = QEDate) %>%
		select(whole_year)
	
	return(df_return)

}


