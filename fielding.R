#  The purpose of this file is to analyze the fielding data 
#  for Major League Baseball Players from 1875 to 2018

#Libraries
library(data.table)

#Read in the fielding data
fielding_data <- read.csv(file="C:\\users\\martia13\\Desktop\\Data Analytics\\Project\\MLB-Data-Analytics-master\\baseballdata\\core\\Fielding.csv", header=T)
View(fielding_data)
num_fielding_data = nrow(fielding_data)

#Remove lines where the Games is 0 
modified_fielding_data <- fielding_data[!(fielding_data$G==0 | is.na(fielding_data$InnOuts)),]
View(modified_fielding_data)
num_modified_fielding_data = nrow(modified_fielding_data)

#Number of rows removed
diff = num_fielding_data - num_modified_fielding_data
print(paste("The number of initial rows: ", num_fielding_data))
print(paste("The number of rows after removing Games = 0 : ", num_modified_fielding_data))
print(paste("Total number of rows removed: ", diff))
print(paste("Percentage of rows removed: ", round(diff/num_fielding_data*100, digits=2), "%"))

# Create a new data table with the following: 
#     Year  Putouts  Assists  Errors InnOuts 
#
# Note: Time in the field is expressed as InnOuts
#
#  These values will represent the "average" fielder for each year. 
new_table = data.frame(matrix(ncol=5, nrow=0))
col_headers = c("Year", "Putouts", "Assists", "Errors", "InnOuts")
colnames(new_table) <- col_headers
View(new_table)

#Go through the data for each year - Split the data by year 
split_by_year <- split(modified_fielding_data, modified_fielding_data$yearID)

for(year in split_by_year) {
  yearId = year$yearID[1]
  #print(nrow(year))

  #get the average putouts per innings in the field 
  avg_putout = sum(year$PO)/sum(year$InnOuts)
  #print(cat("\t", "\t", 'Putouts per inning: ', as.numeric(avg_putout), "\n"))
  
  #get the average assists per innings in the field 
  avg_assists = sum(year$A)/sum(year$InnOuts)
  #print(cat("\t", "\t", 'Assists per inning: ', as.numeric(avg_assists), "\n"))
  
  #get the average errors per innings in the field 
  avg_errors = sum(year$E)/sum(year$InnOuts)
  #print(cat("\t", "\t", 'Errors per inning: ', as.numeric(avg_errors), "\n"))
  
  #get the average number of innings in the field 
  avg_innouts = sum(year$InnOuts)
  #print(cat("\t", "\t", 'Average outs in field: ', as.numeric(avg_innouts), "\n"))
  
  #add the new data to the new data table
  new_table <- rbindlist(list(new_table, as.list(c(yearId, avg_putout, avg_assists, 
                                                 avg_errors, avg_innouts))))
}

#new_table now contains the data that we will be using for the multi-linear regression


























