#=============================================================================
# FILE NAME CHANGES
#=============================================================================

Change this  df_file_index  for File.Index.csv


# Convert from names with underscores to camelCase
Insert GSUB into this:
vct_dimension_names <- 
	paste0("DimenLookup", gsub("_","",df_dimension_index$DimensionCode, df_file_index$TableID))



vct_hierarchy_names <- 
	paste0("DimHierarchy", gsub("_", "", df_dimension_index$DimensionCode), df_file_index$TableID)

# CHANGE dim to Dimen
vct_hierarchy_names <- 
	paste0("DimenHierarchy", gsub("_", "", df_dimension_index$DimensionCode), df_file_index$TableID)



# Change to make sure that path is "Table_XXXX"
sub_path_to_output <- paste0("outputs", "/", "Table_", df_file_index$TableID)

# change to ensure that "Data" not "data"
data_name <- paste0("Data", df_file_index$TableID)


# NEED TO REMOVE "s and 
vct_dimension_names <- 
	paste0("DimenLookup", gsub("(\"|_)","",df_dimension_index$DimensionCode), df_file_index$TableID)

vct_hierarchy_names <- 
	paste0("DimenHierarchy", gsub("(\"|_)", "", df_dimension_index$DimensionCode), df_file_index$TableID)






#=============================================================================
# CONTENTS OF 3 STATIC FILE CHANGES
#=============================================================================
df_dimension_index <- 
		data.frame(DimensionCode = fn_quote(vct_dim_names), 
		DimensionTitle = fn_quote(gsub("_", " ", vct_dim_names)))



df_measure_index <-
		data.frame(MeasureCode = fn_quote(vct_measure_names), 
		MeasureTitle = fn_quote(gsub("_", " ", vct_measure_names)))




# (8.3.1) surround character columns with quotes
df_lu_acccom_type$Description <- fn_quote(df_lu_acccom_type$Description)
df_lu_dest_rto$Description <- fn_quote(df_lu_dest_rto$Description)
df_lu_LOS$Description <- fn_quote(df_lu_LOS$Description)
df_lu_YE$Description <- fn_quote(df_lu_YE$Description)



# (8.3.2) Set sort order to blank
df_lu_acccom_type$SortOrder <- "" 
df_lu_dest_rto$SortOrder <- ""
df_lu_LOS$SortOrder <- ""
df_lu_YE$SortOrder <- ""


# (8.3.3) Drop YE columns...we used it for a join but not needed anymore
df_lu_YE$YE <- NULL










