#  The purpose of this file is to analyze the fielding data 
#  for Major League Baseball Players from 1875 to 2018

#Libraries
library(tidyverse)
library(dplyr)
library(data.table)

#Read in the fielding data
fielding_data <- read.csv(file="C:\\users\\martia13\\Desktop\\Data Analytics\\Project\\MLB-Data-Analytics\\baseballdata\\core\\Fielding.csv", header=T)
num_fielding_data = nrow(fielding_data)

#Remove lines where the Games is 0 
modified_fielding_data <- fielding_data[!(fielding_data$G==0 | is.na(fielding_data$InnOuts) | fielding_data$InnOuts < 675),]
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
  avg_innouts = sum(year$InnOuts)/nrow(year)
  #print(cat("\t", "\t", 'Average outs in field: ', as.numeric(avg_innouts), "\n"))
  
  #add the new data to the new data table
  new_table <- rbindlist(list(new_table, as.list(c(yearId, avg_putout, avg_assists, 
                                                 avg_errors, avg_innouts))))
}

#new_table now contains the data that we will be using for the linear regression

#Putouts Linear Regression
putouts_avg_lm <- lm(Putouts ~ Year, data = new_table)
par(mar=c(7,6,4,2)+0.1, mgp=c(4,1,0))
plot(new_table$Year, new_table$Putouts, main="League Putout Average by Year",
     xlab="Year", ylab="Putout Average (Per Out in the Field)", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(.105, .130, by=.005), las=1)
abline(putouts_avg_lm, col="red", lwd=4)

#Assits Linear Regression
assists_avg_lm <- lm(Assists ~ Year, data = new_table)
plot(new_table$Year, new_table$Assists, main="League Assist Average by Year", 
     xlab="Year", ylab="Assists Average (Per Out in the Field)", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(0.035, 0.060, by=0.005), las=1)
abline(assists_avg_lm, col="red", lwd=4)

#Errors Linear Regression 
errors_avg_lm_original <- lm(Errors ~ Year, data = new_table)
errors_avg_lm <- lm(Errors ~ Year, data = new_table[new_table$Year > 1950])
plot(new_table$Year, new_table$Errors, main="League Error Average by Year", 
     xlab="Year", ylab="Average Errors (Per Out in the Field)", type="b", pch=19, col="blue", 
    yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(0.000, 0.030, by=0.005), las=1)
abline(errors_avg_lm_original, col="grey70", lwd=4)
abline(errors_avg_lm, col="red", lwd=4)
legend("topright", legend=c("Regression For All Years",  
                            "Regression For Years After 1950"), 
                             col=c("grey70", "red"), lty=1, lwd = 4)

#InnOuts Linear Regression
innouts_avg_lm_original <- lm(InnOuts ~ Year, data = new_table)
innouts_avg_lm <- lm(InnOuts ~ Year, data = new_table[new_table$Year > 1950])
plot(new_table$Year, new_table$InnOuts, main="Average InnOuts Played By Year", 
     xlab="Year", ylab="Average InnOuts Played", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(500, 2500, by=500), las=1)
abline(innouts_avg_lm_original, col="grey70", lwd=4)
abline(innouts_avg_lm, col="red", lwd=4)
legend("bottomright", legend = c("Regression For All Years", 
                                "Regression For Years After 1950"), 
                                col=c("grey70", "red"), lty=1, lwd=4)

#Evaluate the models
#Look at the residual standard error for each model. What percent of the average value 
# is the residual error? lower is better
summary(putouts_avg_lm)     # RSE  0.00197 
mean(new_table$Putouts)     #      0.1179359
                            #      % 1.6703
summary(assists_avg_lm)     # RSE  0.002423
mean(new_table$Assists)     #      0.04659756
                            #      % 5.1998
summary(errors_avg_lm)      # RSE  0.0001589
mean(new_table$Errors)      #      0.007461736
                            #      % 2.1295
summary(innouts_avg_lm)     # RSE  90.09
mean(new_table$InnOuts)     #      2023.991
                            #      % 4.4511

#Using the models for prediciton (year = 2020, 2030, 2040, 2050, 2060)
new_years <- data.frame(matrix(ncol=1, nrow=5))
colnames(new_years) <- c("Year")
new_years$Year <- c(2020, 2030, 2040, 2050, 2060)

new_years$pred_putouts <- predict.lm(putouts_avg_lm, newdata = new_years)
new_years$pred_assists <- predict.lm(assists_avg_lm, newdata = new_years)
new_years$pred_errors <- predict.lm(errors_avg_lm, newdata = new_years)
new_years$pred_innouts <- predict.lm(innouts_avg_lm, newdata = new_years)

new_years$total_putouts <- new_years$pred_putouts * new_years$pred_innouts
new_years$total_assists <- new_years$pred_assists * new_years$pred_innouts
new_years$total_errors <- new_years$pred_errors * new_years$pred_innouts

future_fielder_data = new_years %>% select(Year, pred_innouts, total_putouts,
                                           total_assists, total_errors)
colnames(future_fielder_data) <- c("Year", "Number of InnOuts", "Total Putouts", 
                                  "Total Assists", "Total Errors")



















