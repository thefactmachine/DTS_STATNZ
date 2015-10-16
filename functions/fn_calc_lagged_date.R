fn_calc_lagged_date <- function(dte_date, lag_month = 9) {
	# this function receives an end of month date (eg 31-12-2001) and then returns
	# an end of month which is lagged according to the lag_month parameter.
	# An example: dte_date = 31-12-2000, lag_month = 3, returns: 30-9-2000		
	
	return(ceiling_date(dte_date %m-% months(lag_month), unit = "month") - days(1))
	
	}
	