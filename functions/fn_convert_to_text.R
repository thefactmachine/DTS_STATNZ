fn_convert_to_text <- function(a_flt_number) {
	# converts a floating point number to text
	str_return <- a_flt_number %>% 
					# round to zero decimal places
					round(0) %>% 
					# convert number to string with 0 decimals
					format(nsmall = 0) %>%
					# remove any leading / trailing spaces
					str_trim()
	return(str_return)	
}
