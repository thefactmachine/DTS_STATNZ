# a specific year end consists of 4 component quarters. These are time contiguous  (there are 56)

vct_component_quarters <- fn_calculate_component_quarters(df_whole_years$whole_year)

# create a quarter end date from quarter number and year
df_trips$QEDate <- fn_create_date(df_trips$TripQtr, df_trips$TripYear)

# create a data frame of unique whole years (there are 53)
df_wy <- df_trips %>% 
		fn_create_whole_years() %>% 
		# creates four columns of year ends
		fn_create_year_end()


vct_wy <-  unique(c(df_wy$YEMar, df_wy$YEJun, df_wy$YESep, df_wy$YEDec))