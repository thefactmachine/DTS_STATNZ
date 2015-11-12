fn_clean_spend <- function(a_df_trip_spend, a_df_trips) {
	a_df_trip_spend[a_df_trip_spend$SpendType == "Other   ", "SpendType"] <- "Other"

	# nrow() here is 822486 (6 * 137081)
	df_trip_spend_calc <- a_df_trips[, c("TripIDNumber", "ExpenditureWeight")] %>% 
		inner_join(a_df_trip_spend, by = c("TripIDNumber" = "TripID")) %>% 
		mutate(Expenditure = ExpenditureWeight * ImputedSpendAmt) %>%
		select(TripIDNumber, SpendType, Expenditure)


	

	# remove "alcohol" from "food and alcohol" and call it "FoodAndBeverages"
	# nrows = 2 * 137081 = 274162
	df_alcohol_beverage <- df_trip_spend_calc %>% 
						filter(SpendType == "Alcohol" | SpendType == "Food And Alcohol") %>% 
						spread(SpendType, Expenditure) %>%
						select(TripIDNumber, Alcohol, FoodAndAlcohol = one_of("Food And Alcohol")) %>%
						mutate(FoodAndBeverages = FoodAndAlcohol - Alcohol) %>%
						select(-FoodAndAlcohol) %>% 
						gather("SpendType", "Expenditure", 2:3)


	
	# This is the data frame: 822486 (6 * 137081) This data frames removes the "alcohol" double counting
	# in the original data set. Format: TripIDNumber, SpendType, Expenditure
	df_trip_spend_new <- df_trip_spend_calc %>% 
						filter(!SpendType %in% c("Alcohol", "Food And Alcohol")) %>%
						bind_rows(df_alcohol_beverage)


	#ASSERT: There is zero accomodation spend for every "day trips"
	stopifnot(sum(df_trips[a_df_trips$TripType == "Day Trip", "Accom_Spend"]) == 0)



	# there are 90693 rows (i.e overnight trips) with accomodation expense
	df_spend_accomodation <- a_df_trips %>% 
						filter(TripType == "Overnight Trip") %>% 
						select(TripIDNumber, Expenditure = Accom_Spend) %>% 
						mutate(SpendType = "Accomodation") %>%
						select(TripIDNumber, SpendType, Expenditure)




	# there are the full 137,081 rows for transport spend
	df_spend_transport <- a_df_trips %>% 
						select(TripIDNumber, Expenditure = Trans_Spend) %>% 
						mutate(SpendType = "Transport") %>%
						select(TripIDNumber, SpendType, Expenditure)


	# create a consolidated trip spend: 8 variables. 
	# nrows =  (7 * 137081) + (1 * 90693) = 1050260
	df_spend_combined <- bind_rows(df_trip_spend_new, df_spend_accomodation, df_spend_transport)

	return(df_spend_combined)

}