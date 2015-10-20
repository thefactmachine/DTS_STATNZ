fn_calc_los <- function(vctNoNights) {
	vctReturn <- vector("character")
	vctReturn <- rep(NA, length(vctNoNights))
	aveDaysPerMonth <- 365.25 / 12

	
	for (i in 1:length(vctNoNights)) {
		int_current_value <- vctNoNights[i]
		
		if (int_current_value <= 14) {
			vctReturn[i] <- paste0(int_current_value, " nights")
		}
		
		else if ((int_current_value >= 15) & 
			(int_current_value < (1 * aveDaysPerMonth))) {
			vctReturn[i] <- "15 nights to less than one month"	
		}

		else if  ((int_current_value >= (1 * aveDaysPerMonth)) & 
			(int_current_value < (2 * aveDaysPerMonth))) {
			vctReturn[i] <- "One month to less than two months"	
		}

		else if  ((int_current_value >= (2 * aveDaysPerMonth)) & 
			(int_current_value < (3 * aveDaysPerMonth))) {
			vctReturn[i] <- "Two months to less than three months"
		}

		else if  ((int_current_value >= (3 * aveDaysPerMonth)) & 
			(int_current_value < (6 * aveDaysPerMonth))) {
			vctReturn[i] <- "Three months to less than six months"
		}


		else if  ((int_current_value >= (6 * aveDaysPerMonth)) & 
			(int_current_value < (12 * aveDaysPerMonth))) {
			vctReturn[i] <- "Six months to less than one year"
		}		

		else {
			vctReturn[i] <- "Error - more than one year"
		}	
	} # for
	
	return(vctReturn)
	

}  # function

