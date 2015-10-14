fn_create_date <- function(vct_q, vct_y) {
	# receives: a quarter(i.e. 1,2,3,4), a year: returns vector of dates
	
	# define a mapping from quarters to [day + month] combinations.
	df_map <- data.frame(quarter = c(1,2,3,4), 
		day_month = c("31-3", "30-6", "31-9", "31-12"))
	
	vct_date <- 
		# apply the mapping
		df_map$day_month[match(vct_q, df_map$quarter)] %>%
		# concatenate the year with the day_month portion
		paste(vct_y, sep = "-") %>%
		# convert the string into an R date
		as.Date("%d-%m-%Y")
	
	# make the return value explicit
	return(vct_date)
}