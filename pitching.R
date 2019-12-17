#  The purpose of this file is to analyze the pitching data 
#  for Major League Baseball Players from 1875 to 2018

#Libraries
library(dplyr)
library(data.table)


#Read in the fielding data
pitching_data <- read.csv(file="C:\\users\\martia13\\Desktop\\Data Analytics\\Project\\MLB-Data-Analytics\\baseballdata\\core\\Pitching.csv", header=T)
num_pitching_data = nrow(pitching_data)

#Remove lines where the Games is 0 
modified_pitching_data <- pitching_data[!(pitching_data$G==0 | is.na(pitching_data$ERA) | pitching_data$IPouts < 108),]
num_modified_pitching_data = nrow(modified_pitching_data)

#Number of rows removed
diff = num_pitching_data - num_modified_pitching_data
print(paste("The number of initial rows: ", num_pitching_data))
print(paste("The number of rows after removing Games = 0 : ", num_modified_pitching_data))
print(paste("Total number of rows removed: ", diff))
print(paste("Percentage of rows removed: ", round(diff/num_pitching_data*100, digits=2), "%"))

# Create a new data table with the following: 
#     Year shutouts_perc avg_num_games
#       avg_ipouts avg_hits avg_era avg_hrs avg_bb avg_so avg_BAopp avg_RA
#
# Note: shutouts_perc  will be based on the number of games played
#       avg_hits avg_era avg_hrs avg_bb avg_so avg_BAopp avg_RA will 
#           be based on the number of outs pitched 
#       
#  These values will represent the "average" pitcher for each year.

new_table = data.frame(matrix(ncol=8, nrow=0))
col_headers = c("Year", "Shutout Percentage", "Avg Outs Pitched", 
                "Average Hits Allowed", "Average ERA", "Average HRs Allowed", "Average Walks Allowed", 
                "Average Strike-outs")
colnames(new_table) <- col_headers
View(new_table)

#Go through the data for each year - Split the data by year 
split_by_year <- split(modified_pitching_data, modified_pitching_data$yearID)

for(year in split_by_year) { 
  yearId = year$yearID[1]
  #print(yearId)
  
  #Get the Shutout percentage 
  shutout_perc = sum(year$SHO)/sum(year$G) * 100
  #print(paste("", "Shutout Percentage: ", as.numeric(shutout_perc), sep="     "))
  
  #Get the average number of outs pitched 
  avg_num_outs_pitched = sum(year$IPouts)/nrow(year)
  #print(paste("", "Average outs pitched: ", as.numeric(avg_num_outs_pitched), sep="     "))
  
  #Get the average number of hits allowed per out
  avg_num_hits = sum(year$H)/sum(year$IPouts)
  #print(paste("", "Average hits per out: ", as.numeric(avg_num_hits), sep="     "))
  
  #Get the average ERA 
  avg_era = sum(year$ER)/sum(year$IPouts) * 27
  #print(paste("", "Average ERA: ", as.numeric(avg_era),sep="     "))
  
  #Get the average number of home runs allowed per out 
  avg_hrs = sum(year$HR)/sum(year$IPouts)
  #print(paste("", "Average HRs per out: ", as.numeric(avg_hrs), sep="     "))
  
  #Get the average number of walks allowed per out
  avg_walks = sum(year$BB)/sum(year$IPouts)
  #print(paste("", "Average BBs per out: ", as.numeric(avg_walks), sep='     '))
  
  #Get the average number of strike outs allowed 
  avg_SO = sum(year$SO)/sum(year$IPouts)
  #print(paste("", "Average SOs per out: ", as.numeric(avg_SO), sep="     "))
  
  #Add the new data to new_table
  new_table <- rbindlist(list(new_table, as.list(c(yearId, shutout_perc, avg_num_outs_pitched, 
                                                   avg_num_hits, avg_era, avg_hrs, avg_walks, avg_SO))))
}

#new_table now contains the data that we will be using for the linear regression

#Average Outs Pitched Linear Regression 
outs_pitched_lm <- lm(`Avg Outs Pitched` ~ Year, data = new_table)
par(mar=c(7,6,4,2)+0.1, mgp=c(4,1,0))
plot(new_table$Year, new_table$`Avg Outs Pitched`, main="League Average Outs Pitched by Year",
     xlab="Year", ylab="Average Outs Pitched", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(0, 1300, by=100), las=1)
abline(outs_pitched_lm, col="red", lwd=4)

#ERA Linear Regression
era_lm <- lm(`Average ERA` ~ Year, data = new_table)
plot(new_table$Year, new_table$`Average ERA`, main = "League Average ERA by Year", 
     xlab="Year", ylab="Average ERA", type="b", pch=19, col="blue",
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(2.0, 5.5, by=0.5), las=1)
abline(era_lm, col="red", lwd=4)

#Shutouts Linear Regression
#Error is too high for all years (~44.44%), so only considering years after 1950
shut_out_perc_lm <- lm(`Shutout Percentage` ~ Year, data = new_table[new_table$Year > 1950])
plot(new_table$Year, new_table$`Shutout Percentage`, main="League Shutout Percentage by Year",
     xlab="Year", ylab="Shutout Percentage", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(0, 10, by=2), las=1)
abline(shut_out_perc_lm, col="red", lwd=4)

#Average Hits Allowed Linear Regression 
avg_hits_allowed_lm <- lm(`Average Hits Allowed` ~ Year, data = new_table)
plot(new_table$Year, new_table$`Average Hits Allowed`, main="League Average Hits Allowed by Year", 
     xlab="Year", ylab="Average Hits Allowed (Per Out Pitched)", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(0.25, 0.50, by=.05), las=1)
abline(avg_hits_allowed_lm, col="red", lwd=4)

#Average HRs allowed Linear Regression 
avg_hrs_allowed_lm <- lm(`Average HRs Allowed` ~ Year, data = new_table)
plot(new_table$Year, new_table$`Average HRs Allowed`, main="League Average HRs Allowed by Year", 
     xlab = "Year", ylab="Average HRs Allowed (Per Out Pitched)", type="b", pch=19, col="blue",
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(0.00, 0.05, by=0.01), las=1)
abline(avg_hrs_allowed_lm, col="red", lwd=4)

#Average Walks Allowed Linear Regression
avg_walks_allowed_lm_original <- lm(`Average Walks Allowed` ~ Year, data = new_table)
avg_walks_allowed_lm <- lm(`Average Walks Allowed` ~ Year, data = new_table[new_table$Year > 1950])
plot(new_table$Year, new_table$`Average Walks Allowed`, main="League Average Walks Allowed by Year", 
     xlab="Year", ylab="Average Walks Allowed (Per Out Pitched)", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(0.00, 0.16, by=0.02), las=1)
abline(avg_walks_allowed_lm_original, col="grey70", lwd=4)
abline(avg_walks_allowed_lm, col="red", lwd=4)

#Average Strike-outs Linear Regression
avg_so_lm <- lm(`Average Strike-outs` ~ Year, data = new_table)
plot(new_table$Year,  new_table$`Average Strike-outs`, main="League Average Strike-Outs by Year", 
     xlab="Year", ylab="Average Strike-Outs (Per Out Pitched)", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(0.00, 0.30, by=0.05), las=1)
abline(avg_so_lm, col="red", lwd=4)


#Evaluate the models
#Look at the residual standard error for each model. What percent of the average value 
# is the residual error? lower is better
summary(outs_pitched_lm)           # RSE  75.94 
mean(new_table$`Avg Outs Pitched`) #      464.3074
                                   #    % 16.356
summary(era_lm)                    # RSE  0.5203
mean(new_table$`Average ERA`)      #      3.63891
                                   #    % 14.298
summary(shut_out_perc_lm)            # RSE 0.4131
mean(new_table$`Shutout Percentage`) #     3.260849   
                                     #   % 12.668
summary(avg_hits_allowed_lm)           # RSE 0.02686  
mean(new_table$`Average Hits Allowed`) #     0.3370276   
                                       #   % 7.970
summary(avg_hrs_allowed_lm)           # RSE 0.004572    
mean(new_table$`Average HRs Allowed`) #     0.02102097
                                      #   % 21.750
summary(avg_walks_allowed_lm)           # RSE 0.007497
mean(new_table$`Average Walks Allowed`) #     0.1084825
                                        #   % 6.911
summary(avg_so_lm)                    #RSE 0.02798 
mean(new_table$`Average Strike-outs`) #    0.1629604
                                      #  % 17.170

#Using the models for prediciton (year = 2020, 2030, 2040, 2050, 2060)
new_years <- data.frame(matrix(ncol=1, nrow=5))
colnames(new_years) <- c("Year")
new_years$Year <- c(2020, 2030, 2040, 2050, 2060)

new_years$pred_outs_pitched <- predict.lm(outs_pitched_lm, newdata = new_years)
new_years$pred_era <- predict.lm(era_lm, newdata = new_years)
new_years$pred_shutout_perc <- predict.lm(shut_out_perc_lm, newdata = new_years)
new_years$pred_hits_allowed <- predict.lm(avg_hits_allowed_lm, newdata = new_years)
new_years$pred_hrs_allowed <- predict.lm(avg_hrs_allowed_lm, newdata = new_years)
new_years$pred_walks_allowed <- predict.lm(avg_walks_allowed_lm, newdata = new_years)
new_years$pred_so <- predict.lm(avg_so_lm, newdata = new_years)

new_years$total_hits <- new_years$pred_outs_pitched * new_years$pred_hits_allowed
new_years$total_hrs <- new_years$pred_outs_pitched * new_years$pred_hrs_allowed
new_years$total_walks <- new_years$pred_outs_pitched * new_years$pred_walks_allowed
new_years$total_so <- new_years$pred_outs_pitched * new_years$pred_so

future_pitcher_data = new_years %>% select(Year, pred_outs_pitched, pred_era,
                                           pred_shutout_perc, total_hits, total_hrs, 
                                           total_walks, total_so)
colnames(future_pitcher_data) <- c("Year", "Number of InnOuts Pitched", "ERA", "Shutout Percentage",
                                   "Hits Allowed", "HRs Allowed", "Walks Allowed", "Strike-Outs")













