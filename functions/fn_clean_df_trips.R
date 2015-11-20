fn_clean_df_trips <- function(a_df_trips) {
	# clean up some columns
	a_df_trips[is.na(a_df_trips$SmoothedTripWeight), "SmoothedTripWeight"] <- 0

	a_df_trips[is.na(a_df_trips$RespondentWeight), "RespondentWeight"] <- 0	
	a_df_trips[a_df_trips$DestinationRTO == "Other   ", "DestinationRTO"] <- "Other"	
	# the following aligns the purpose of visit (POV) values in df_trips to df_purpose_lu
	# this is required to join the two tables
	a_df_trips[is.na(a_df_trips$POV), "POV"] <- "None"
	a_df_trips[a_df_trips$POV == "REFUSED", "POV"] <- "Refused"
	a_df_trips[a_df_trips$POV == "Business   "  , "POV"] <- "Business"
	a_df_trips[a_df_trips$POV == "Other Reason (specify)"  , "POV"] <- "Other"
	a_df_trips[a_df_trips$POV == "Education Or Study"  , "POV"] <- "Education/Study"
	a_df_trips[a_df_trips$POV == "Conference Or Convention"  , "POV"] <- "Conference or Convention"	
	a_df_trips[a_df_trips$POV == "Attending wedding/Family Occasion/Funeral"  , "POV"]  <- 
		"Attending Wedding/Family Occasion/Funerals"	
	a_df_trips[a_df_trips$POV == "Visiting relatives"  , "POV"] <- "Visiting Relatives"
	a_df_trips[a_df_trips$POV == "Participating in sports activity"  , "POV"]  <- 
		"Participation in Sports Activity"	
	a_df_trips[a_df_trips$POV == "Publicised special event"  , "POV"] <- "Publicised Special Event"
	return(a_df_trips)
	
}
