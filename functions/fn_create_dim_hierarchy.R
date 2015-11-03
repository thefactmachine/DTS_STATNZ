fn_create_dim_hierarchy <- function(a_df_lu) {
	#assumes the data frame argument has the format:
	#Code, Description, SortOrder
	int_parent_number <- a_df_lu[a_df_lu$Description == "All", "Code"]
	vct_children <- a_df_lu[a_df_lu$Description != "All", "Code"]
	vct_parent <- rep(int_parent_number, length(vct_children))
	return(data.frame(Parent = vct_parent, Child = vct_children))
	
}