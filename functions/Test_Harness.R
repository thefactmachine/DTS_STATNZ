# clear everything
rm(list = ls())

# load some libaries 
#library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

options(stringsAsFactors = FALSE)
# do not display in scientific notation
options(scipen=999, digits = 10)

setwd('/Users/zurich/Documents/TEMP-FILES/MBIE/DTS_STATNZ')

expected_file_ID <- "7580"
folder_name <- paste0("Table_", expected_file_ID)
wd <- getwd()
file_index_path <- file.path(wd, "outputs", folder_name, "FileIndex.csv") 
df_file_index <- read.csv(file_index_path, header = TRUE)

# Print and check spelling
df_file_index

# get file ID
file_ID <- df_file_index$TableID

# based on the file_ID load up the data table
data_file_name <- paste0("Data", file_ID, ".csv")
df_data <-  read.csv(file.path(wd, "outputs", folder_name, data_file_name) , header = TRUE)


# print the data of the data table
head(df_data)

#====================================================================================================== 
# load up DimenionIndex and MeasureIndex

df_dimen <-  read.csv(file.path(wd, "outputs", folder_name, "DimensionIndex.csv") , header = TRUE, quote = "")
# check that quotes are surrounding the strings
df_dimen

# read the file again and strip off the quotes
df_dimen <-  read.csv(file.path(wd, "outputs", folder_name, "DimensionIndex.csv") , header = TRUE)


df_measure <- read.csv(file.path(wd, "outputs", folder_name, "MeasureIndex.csv") , header = TRUE, quote = "")
# check that quotes are surrounding the strings
df_measure
# read the file again and strip off the quotes
df_measure <- read.csv(file.path(wd, "outputs", folder_name, "MeasureIndex.csv") , header = TRUE)


#====================================================================================================== 
# Check that column names in the Data file are equal to the names in the dimension and measure files.
# The names need to be the same and they also need to be in the same order

stopifnot(all(names(df_data) == c(df_dimen$DimensionCode, df_measure$MeasureCode)))

 
#====================================================================================================== 
# construct a list of file names for DimensionLookups from df_dimen
dim_lu_file_names <- gsub("(\"|_)", "", df_dimen$DimensionCode)
dim_lu_file_names <- paste0("DimenLookup", dim_lu_file_names, file_ID, ".csv")

lst_dim_lu_quotes 	<- lapply(dim_lu_file_names, function(x)  {
						read.csv(file.path(wd, "outputs", folder_name, x) , header = TRUE, quote = "")
	
})

# EYEBALL OUTPUT...should contain quotes:

lapply(lst_dim_lu_quotes, function(x)  head(x,3))






lst_dim_lu_clean <- lapply(dim_lu_file_names, function(x)  {
						read.csv(file.path(wd, "outputs", folder_name, x) , header = TRUE)
	
})

names(lst_dim_lu_clean) <- df_dimen$DimensionCode











lapply(seq_along(lst_dim_lu_clean), function(x) {	
		col_name <- names(lst_dim_lu_clean)[x]
		
		
	})





df_aggregations <- do.call(bind_rows, lst_aggregations) %>% as.data.frame()























