# function performs various group_by aggregations on 
# the data frame "a_df_base_aggregates"
# the columns to group_by are determined by "a_vct_columns"
# the columns to aggregate are determined by "a_sum_clause"
# called by an lapply function with various column combinations
fn_create_comb_aggregates <- 
	function(a_df_base_aggregates, a_vct_columns, a_sum_clause) {

		df_return <- a_df_base_aggregates %>% 
		# group_by various column combinations
		group_by_(.dots = a_vct_columns) %>% 
		# create totals for 4 measure columns
		summarise_(.dots = a_sum_clause) %>% 
		# fill is missing columns with the value "All"
		fn_create_df_with_all(vct_dim_names, vct_col_sort)
		return(df_return)
}