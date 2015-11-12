fn_clean_trips <- function(a_df_trips, a_df_purpose_lu) {
# this function just cleans up a few values in the trips table
	a_df_trips[a_df_trips$DestinationRTO == "Other   ", "DestinationRTO"] <- "Other"

	# the following aligns the purpose of visit (POV) values in a_df_trips to df_purpose_lu
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
	a_df_trips[is.na(a_df_trips$ImputedSpendAccom),"ImputedSpendAccom"] <- 0
	a_df_trips[is.na(a_df_trips$ExpenditureWeight), "ExpenditureWeight"] <- 0

	# the following stuff adds in some computed columns, and then adds a POV_group
	# which is a grouping of "purpose of visit"

	a_df_trips <- a_df_trips %>% 
			select(TripIDNumber, TripYear, TripQtr, TripType, 
				DestinationRTO, POV, ExpenditureWeight, ImputedSpendAccom, ImputedSpendTrnsport)

	a_df_trips <- a_df_trips %>% 
			mutate(Accom_Spend = ExpenditureWeight * ImputedSpendAccom) %>%
			mutate(Trans_Spend = ExpenditureWeight * ImputedSpendTrnsport)

	a_df_trips  <- a_df_trips  %>% inner_join(df_purpose_lu, by = c("POV" = "POV"))		
	return(a_df_trips)

}