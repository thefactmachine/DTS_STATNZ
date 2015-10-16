fn_calculate_component_quarters <- function(a_df_whole_years) {
	vct_base_year <- df_whole_years[rep(seq_len(nrow(a_df_whole_years)), each = 4),]
	vct_month_offset <-rep(c(0,3,6,9), nrow(a_df_whole_years))
	vct_date_group  <-  fn_calc_lagged_date(vct_base_year, vct_month_offset)
	df_year_end_dates <- data.frame(year_end = vct_base_year, 
		month_offset = vct_month_offset, quarters = vct_date_group)	
	vct_unique_component_quarters <- unique(df_year_end_dates$quarters)
}


