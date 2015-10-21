fn_create_df_with_all <- 
	function(df_sans_all, a_vct_all_dim, vct_col_sort) {
	# receives a data.frame with N dimensions from a total of T dimensions
	# where (N <= T). This function creates the missing dimensions where each
	# row will have the value "All".

	# what are the names of the missing dimensions?
	vct_dim_excluded <- base::setdiff(a_vct_all_dim, names(df_sans_all))
	
	int_rows <- nrow(df_sans_all)
	# number of cells in a matrix = number of columsns * number of rows
	vct_num_cells <- length(vct_dim_excluded) * int_rows
	
	# create a matrix and name the columns
	vct_cell_values <- rep("All", vct_num_cells)
	mat <- matrix(vct_cell_values, nrow = int_rows)
	colnames(mat) <- vct_dim_excluded

	# create a data.frame from the matrix
	df_all <- as.data.frame(mat)

	df_combined <- df_all %>% bind_cols(df_sans_all) %>% 
		select_(.dots = vct_col_sort)

	return(df_combined)
}

