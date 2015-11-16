fn_get_classification <- function() {
# spreadsheet copies from P:\OTSP\IVS\2.Design and Development\2. Classification
# binary format of the spreadsheet changed from XLS to XLXS

	
	library(xlsx)
	
	df_class <- read.xlsx('inputs/ivs_activities_classification.xlsx', 
						sheetIndex = 3, rowIndex = 2:143, colIndex = 2:5)
	
	 
	for (i in 1:nrow(df_class)) {
		if (is.na(df_class[i,2])) {
			df_class[i, 2] <- df_class[i - 1, 2]
		}
	}
	
	# drop the first and third columsn, these are codes
	df_class <- df_class[, -c(1, 3)]
	
	names(df_class) <- c("Activity_Group", "Activity")
	
	
	
	
	
	
	df_class[df_class$Activity == "Walk in City", "Activity"] <- "Walk In City"
	df_class[df_class$Activity == "Refused To Answer", "Activity"] <- "Refused to Answer"
	df_class[df_class$Activity == "Scenic Flight (heli, plane)", "Activity"] <- "Scenic Flight (Heli, Plane)"
	df_class[df_class$Activity == "Cycling (on-road)", "Activity"] <- "Cycling (On-road)"
	df_class[df_class$Activity == "Arts and Crafts", "Activity"] <- "Arts And Crafts"
	df_class[df_class$Activity == "Mountain Biking (off-road)", "Activity"]  <-
	"Mountain Biking (Off-road)" 
	df_class[df_class$Activity == "Zoos/Wildlife/Marine Parks (e.g. Kelly Tarltons, Orana Park,", "Activity"] <-
	"Zoos/Wildlife/Marine Parks (e.g. Kelly Tarltons, Orana Park, Wellington Zoo)"
	df_class[df_class$Activity == "Personal Business (incl. family help/financial/househunting)" , "Activity"] <-
	"Personal Business (incl. family help/financial/house hunting)"
	df_class[df_class$Activity == "Dont Know" , "Activity"] <- "Dont know" 
	
	return(df_class)

}