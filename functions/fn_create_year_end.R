fn_create_year_end <- function(a_df_trips) {
	# function creates extra columns to group by.  

	a_df_trips$YEMar <- ifelse(a_df_trips$TripQtr <= 1, paste0("YEMar",a_df_trips$TripYear), 
		paste0("YEMar",a_df_trips$TripYear + 1))

	a_df_trips$YEJun <- ifelse(a_df_trips$TripQtr <= 2, paste0("YEJun",a_df_trips$TripYear), 
		paste0("YEJun",a_df_trips$TripYear + 1))

	a_df_trips$YESep <- ifelse(a_df_trips$TripQtr <= 3, paste0("YESep",a_df_trips$TripYear), 
		paste0("YESep",a_df_trips$TripYear + 1))

	a_df_trips$YEDec <- ifelse(a_df_trips$TripQtr <= 4, paste0("YEDec",a_df_trips$TripYear), 
		paste0("YEDec",a_df_trips$TripYear + 1))

	return(a_df_trips)

}