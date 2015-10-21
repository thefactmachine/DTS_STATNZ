fn_create_column_combinations <- function(vct_names) {
	# takes a vector of names and returns all possible combinations
	# if length(vct_names) = 4, then.. the total combinations is (2^4)-1
	# but this function excludes the final combination..as these have
	# already been calculated...so this function returns (2^4)-2 = 14 combos..
	# for a four element list

	fn_combination <- function(a_vct_names, a_int_n) {
		# this wraps combn() so it returns a list rathern than a matrix
		mat <- combn(a_vct_names, a_int_n)
		col_seq <- seq_len(ncol(mat))
		lst_return <- lapply(col_seq, function(x) mat[,x])
		return(lst_return)	
	}

	lst_combinations <- 
		lapply(seq_len(length(vct_names)-1), function(x) fn_combination(vct_names, x))

	# turn a nested list into a flat structure
	lst_combinations_flat <- unlist(lst_combinations, recursive=FALSE)
	return(lst_combinations_flat)

}