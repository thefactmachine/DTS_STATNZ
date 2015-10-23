# QA Steps
## Other steps

### columns with "all" in any of the dimensions

'''
df_consolidated %>% filter(YE == "All" | LOS_Group == "All" |  DestinationRTO == "All" | AccommodationType == "All") %>% summarise(number_rows = n())
'''

Result is 71,115 rows

### total for columns with "all" in any of the dimensions

''' 
lst_aggregations <- list("sum(Total_Visitors)", "sum(Total_Trips)", 
    "sum(Total_Nights)", "sum(Total_Respondents)")

agg_names <- c("Total_Visitors", "Total_Trips", "Total_Nights", "Total_Respondents")

lst_sum_clause <- setNames(lst_aggregations, agg_names)

df_consolidated %>% filter(YE == "All" | LOS_Group == "All" |  
DestinationRTO == "All" | AccommodationType == "All") %>% 
summarise_(.dots = lst_sum_clause)

'''
### results
  Total_Visitors   Total_Trips  Total_Nights Total_Respondents
1  1238909245.05 15372733485.1 38383154404.7           6169710

### Number of rows where YE = "All"  
result is 7656 rows

### Number of rows where LOS_Group == "All"
result is 33050 rows

### Number of rows where DestinationRTO == "All"
result is 15301 rows

### Number of rows where AccommodationType == "All"
result is 21533 rows

### Number of rows where **none** of the dimension variables are "All"

'''
df_consolidated %>% filter(YE != "All" & LOS_Group != "All" &  DestinationRTO != "All" & AccommodationType != "All") %>% summarise(number_rows = n())
'''

result is 86511 rows

### Totals  where **none** of the dimension variables are "All"

Total_Visitors   Total_Trips  Total_Nights   Total_Respondents
1  82593949.6703 1024848899.01 2558876960.31            411314

### Reconcile the above with pre-calculated totals
'''
df_consolidated %>% filter(YE == "All" & LOS_Group == "All" &  DestinationRTO == "All" & AccommodationType == "All") 

'''
same totals as calculated above.























