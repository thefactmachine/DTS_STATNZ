#=============================================================================
# FILE NAME CHANGES
#=============================================================================

Change this  df_file_index  for File.Index.csv

# Change to make sure that path is "Table_XXXX"
sub_path_to_output <- paste0("outputs", "/", "Table_", df_file_index$TableID)


# change to ensure that "Data" not "data"
data_name <- paste0("Data", df_file_index$TableID)


#====================================================================
# CHANGE THE FOLLOWING 
vct_dimension_names <- 
	paste0("DimenLookup", df_dimension_index$DimensionCode, df_file_index$TableID)

vct_hierarchy_names <- 
	paste0("DimHierarchy", df_dimension_index$DimensionCode, df_file_index$TableID)


# TO THE FOLLOWING

# NEED TO REMOVE "s and 
vct_dimension_file_names <- 
	paste0("DimenLookup", gsub("(\"|_)","",df_dimension_index$DimensionCode), df_file_index$TableID)

vct_hierarchy_file_names <- 
	paste0("DimenHierarchy", gsub("(\"|_)", "", df_dimension_index$DimensionCode), df_file_index$TableID)



#====================================================================



# assemble the above into a single vector and assign to the list
vct_list_names <- c(data_name, vct_dimension_file_names, vct_hierarchy_file_names, vct_index_names)


rm(data_name, vct_dim_names, vct_hierarchy_file_names , vct_dimension_file_names, 
		vct_index_names, vct_list_names, vct_measure_names)


#COMPILE CODE HERE AND CHECK - SHOULD BE NO ERRORRRSSSSSSSS
#====================================================================






#=============================================================================
# CONTENTS OF 3 STATIC FILE CHANGES
#=============================================================================

# (8.3.1) surround character columns with quotes
ADD THIS ABOVE
# takes a character vector and surrounds it with quotes hi --> "hi"
source('functions/fn_quote.R')




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


#============================================================================================
#============================================================================================
#============================================================================================
#============================================================================================
#============================================================================================


# (8.4) create Dimension and Measure Index df's fn_quote() surounds with "
# (8.4.1) Dimension Index

vct_dim_title <- c("Year ending", "Length of stay", 
		"Regional tourism organisation", "Accommodation type")

vct_dim_title <- fn_quote(vct_dim_title)

df_dimension_index <- 
		data.frame(DimensionCode = fn_quote(vct_dim_names), 
		DimensionTitle = vct_dim_title)

#============================================================================================
#============================================================================================
 


# (8.4.2) Measure Index
vct_measure_title <- c("Total visitors", "Total trips", "Total nights", "Total respondents")
vct_measure_title <- fn_quote(vct_measure_title)


df_measure_index <-
		data.frame(MeasureCode = fn_quote(vct_measure_names), 
		MeasureTitle = vct_measure_title)


#============================================================================================
#============================================================================================
#============================================================================================
#============================================================================================
#============================================================================================



# Get YE to be Year_Ending
#=======================================================================================
#=======================================================================================
#=======================================================================================


# (6.1) get the dimenions names
vct_dim_names <- c("Year_Ending", "LOS_Group", "Destination_RTO", "Accommodation_Type")
names(df_base_aggregates)[1:4] <- vct_dim_names

vct_measure_names <- c("Total_Visitors", "Total_Trips", "Total_Nights", "Total_Respondents")
names(df_base_aggregates)[5:8] <- vct_measure_names


# (6.2) create a "summarise" clause (for multiple use later)
lst_aggregations <- as.list(paste0("sum(", vct_measure_names,")"))


lst_sum_clause <- setNames(lst_aggregations, vct_measure_names)

# (6.3) sort order of the columns
vct_col_sort <- c(vct_dim_names, vct_measure_names)


#=========================================================================='
#=========================================================================='

#line 312

source('functions/fn_create_YE_lookup.R')
df_lu_YE <- fn_create_YE_lookup(df_fin$Year_Ending) %>% as.data.frame()


# NOTE HOW THE COLUMN NAME CHANGE

df_fin_lu <- df_fin %>% 
			inner_join(df_lu_LOS, by = c("LOS_Group" = "Description")) %>% 
			mutate(LOS_Group = Code) %>% 
			select(-c(Code, SortOrder)) %>% 
		
			inner_join(df_lu_dest_rto, by = c("Destination_RTO" = "Description")) %>% 
			mutate(Destination_RTO = Code) %>% 
			select(-c(Code, SortOrder)) %>%
		
			inner_join(df_lu_acccom_type, by = c("Accommodation_Type" = "Description")) %>% 
			mutate(Accommodation_Type = Code) %>% 
			select(-c(Code, SortOrder)) %>%
 		
 			inner_join(df_lu_YE, by = c("Year_Ending" = "YE")) %>% 
			mutate(Year_Ending = Code) %>% 
			select(-c(Code, SortOrder, Description)) %>%   
			as.data.frame()



#=======================================================================================
#============================================================================================
#============================================================================================
#============================================================================================
#============================================================================================
#============================================================================================

#INSERT THE FOLLOWING TO CHECK FOR NAs
# ASSERT: No NAs bave been introduced before converting to text.
stopifnot(sum(is.na(df_consolidated)) == 0)



# (7.1) convert numeric columns to text with 0 decimal places
#df_fin <- sapply(df_consolidated[

# Get YE to be Year_Ending
#=======================================================================================

NAMES NAMES NAMES NAMES

1.       Domestic Travel Survey:  Accommodation  		7578
2.       Domestic Travel Survey:  Trips 				7581
3.       Domestic Travel Survey:  Activities   			7579


4.       Domestic Travel Survey:  Visitor Spend         7580



#========================================================================================


# (6.1) get the dimenions names

vct_dim_names <- c("Year_Ending", "Destination_RTO", "Trip_Type", "POV", "POV_Group")
 names(df_base_aggregates)[1:5] <- vct_dim_names


vct_measure_names <- c("Total_Visitors", "Total_Trips", "Total_Respondents")
names(df_base_aggregates)[6:8] <- vct_measure_names

# (6.2) create a "summarise" clause (for multiple use later)
lst_aggregations <- as.list(paste0("sum(", vct_measure_names,")"))


lst_sum_clause <- setNames(lst_aggregations, vct_measure_names)


















