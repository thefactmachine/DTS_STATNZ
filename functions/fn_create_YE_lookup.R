fn_create_YE_lookup <- function(a_vctYE) {
	# this function creates a lookup table for the YE values
	# it receives a vector of year end values
	substrRight <- function(x, n){
	  # function receives a string and returns the n right characters
	  substr(x, nchar(x)-n+1, nchar(x))
	}
	
	# YE values have 9 characters eg "YEMar2003"
	vct_YE <- unique(a_vctYE[nchar(a_vctYE) == 9])
	# extract the year component: "YEMar2003" => 2003
	vct_year <- as.numeric(substrRight(vct_YE,4))
	# extract the month: "YEMar2003" => "Mar"
	vct_month <- substr(vct_YE, 3, 5)
	# convert the month to a quarter: "Mar" => 1 
	vct_quarter <- match(vct_month, c("Mar", "Jun", "Sep", "Dec"))
	
	# construct a data.frame
	df_year <- data.frame(YE = vct_YE, year = vct_year, month =  vct_month, qtr = vct_quarter)
	
	# do some sorting and create some derived values
	df_year <- df_year %>% 
				# sort by year then qtr
				arrange(year, qtr) %>% 
				# create a numbered column
				mutate(Code = 1:n()) %>% 
				mutate(Description = paste0("YE", month, " ", year)) %>%
				select(-c(year, month, qtr)) %>%
				mutate(SortOrder = Code)
	
	nr <- nrow(df_year)
	
	# create a single row data frame
	df_all <- data.frame(YE = "All", Code = nr + 1, Description = "All", SortOrder = nr + 1)
	
	# club two data frames together
	df_return <- bind_rows(df_year, df_all)
	return(df_return)
}

