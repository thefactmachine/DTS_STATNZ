fn_quote <- function(a_vct_str) {
	# surrounds the argument vector with quote marks
	paste0(intToUtf8(34), a_vct_str, intToUtf8(34))
}