#  The purpose of this file is to analyze the batting data 
#  for Major League Baseball Players from 1875 to 2018

#Libraries
library(dplyr)
library(data.table)

#Read in the batting data
batting_data <- read.csv(file="C:\\users\\martia13\\Desktop\\Data Analytics\\Project\\MLB-Data-Analytics\\baseballdata\\core\\Batting.csv", header=T)
num_batting_data = nrow(batting_data)

#Remove lines where the At-Bats is 0 
#RBI had NA values for some entries, so those lines were removed. Increased 
#data removed from 15.44% to 16.55%
modified_batting_data <- batting_data[!(batting_data$AB<100 | batting_data$G==0 | is.na(batting_data$RBI)), ]
num_modified_batting_data = nrow(modified_batting_data)

#Number of rows removed
diff = num_batting_data - num_modified_batting_data
print(paste("The number of initial rows: ", num_batting_data))
print(paste("The number of rows after removing At-Bats = 0 : ", num_modified_batting_data))
print(paste("Total number of rows removed: ", diff))
print(paste("Percentage of rows removed: ", round(diff/num_batting_data*100, digits=2), "%"))

# Create a new data table with the following: 
#     Year  Avg-BatAvg HR-Per-AtBat  RBI-Per-AtBat  Runs-Per-Game  Avg-AtBats
#
#  These values will represent the "average" player that year. 
new_table = data.frame(matrix(ncol=7, nrow=0))
col_headers = c("Year", "Avg-BatAvg", "HR-Per-AtBat", 
                "RBI-Per-AtBat", "Runs-Per-Game", "Avg-AtBats", "Avg-Games")
colnames(new_table) <- col_headers

#Go through the data for each year - Split the data by year 
split_by_year <- split(modified_batting_data, modified_batting_data$yearID)

for(year in split_by_year) {
  yearId = year$yearID[1]
  #Get the average batting-average
  avg_bat_avg = sum(year$H)/sum(year$AB)
  print(paste0('Year: ', yearId, nrow(year)))
  #print(cat("\t", "\t", 'Average average: ', as.numeric(avg_bat_avg), "\n"))
  
  #get the HRs per at-bat 
  avg_hrs_per_at_bat = sum(year$HR) / sum(year$AB)
  #print(cat("\t", "\t", 'Homeruns per at-bat: ', as.numeric(avg_hrs_per_at_bat), "\n"))
  
  #get the RBIs per at-bat 
  avg_rbi_per_at_bat = sum(year$RBI) / sum(year$AB)
  #print(cat("\t", "\t", 'RBIs per at-bat: ', as.numeric(avg_rbi_per_at_bat), "\n"))
  
  #get the runs per game
  avg_runs_per_game = sum(year$R) / sum(year$G)
  #print(cat("\t", "\t", 'Runs per game: ', as.numeric(avg_runs_per_game), "\n"))
  
  #get the average number of at bats 
  avg_num_at_bats = sum(year$AB) / nrow(year)
  #print(cat("\t", "\t", 'Average number of at-bats: ', as.numeric(avg_num_at_bats), "\n"))
  
  #get the average number of games
  avg_num_games = sum(year$G) / nrow(year)
  #print(cat("\t", "\t", 'Average number of games played: ', as.numeric(avg_num_games), "\n"))
  
  #add the newly found data to the table as a new row 
  new_table <- rbindlist(list(new_table, as.list(c(yearId, avg_bat_avg, avg_hrs_per_at_bat, 
                     avg_rbi_per_at_bat, avg_runs_per_game, avg_num_at_bats, avg_num_games))))
}

#new_table now contains the data that we will be using for the linear regression

#Creating the linear regression models

#Batting Average Linear Regression
batting_avg_lm <- lm(`Avg-BatAvg` ~ Year, data = new_table)
par(mar=c(7,6,4,2)+0.1, mgp=c(4,1,0))
plot(new_table$Year, new_table$`Avg-BatAvg`, main="League Batting Average by Year",
     xlab="Year", ylab="Batting Average", type="b", pch=19, col="blue", 
    yaxt="n", xaxt="n")
axis(1, at=seq(1870, 2018, by=20), las=1)
axis(2, at=seq(.220, .330, by=.005), las=1)
abline(batting_avg_lm, col="red", lwd=4)

#HR per At-Bat Average Linear Regression 
hr_per_at_bat_lm <- lm(`HR-Per-AtBat` ~ Year, data = new_table)
plot(new_table$Year, new_table$`HR-Per-AtBat`, main="League Avg Number of HRs Per At-Bat by Year",
     xlab="Year", ylab="Average Number of HR per At-bat", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1871, 2018, by=20), las=1)
axis(2, at=seq(0.000, 0.040, by=0.005), las=1)
abline(hr_per_at_bat_lm, col="red", lwd=4)

#RBI per at bat Average Linear Regression
rbi_per_at_bat_lm <- lm(`RBI-Per-AtBat` ~ Year, data = new_table)
plot(new_table$Year, new_table$`RBI-Per-AtBat`, main = "League Avg Number of RBIs Per At-Bat by Year",
     xlab="Year", ylab="Average Number of RBI per At-bat", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1871, 2018, by=20), las=1)
axis(2, at=seq(0.00, 0.20, by=0.02), las=1)
abline(rbi_per_at_bat_lm, col="red", lwd=4)

#Runs per game Average Linear Regression
runs_per_game_lm <- lm(`Runs-Per-Game` ~ Year, data = new_table)
plot(new_table$Year, new_table$`Runs-Per-Game`, main="League Avg Number of Runs Per Game by Year", 
     xlab="Year", ylab="Average number of Runs per Game", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1871, 2018, by=20), las=1)
axis(2, at=seq(0.0, 1.4, by=0.2), las=1)
abline(runs_per_game_lm, col="red", lwd=4)

#Average number of At-Bats for each player by year
avg_num_at_bats_lm <- lm(`Avg-AtBats` ~ Year, data = new_table)
plot(new_table$Year, new_table$`Avg-AtBats`, main="League Avg Number of At-Bats by Year", 
     xlab="Year", ylab="Average Number of At-Bats", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1871, 2018, by=20), las=1)
axis(2, at=seq(0, 400, by=50), las=1)
abline(avg_num_at_bats_lm, col="red", lwd=4)

#Average number of games for each player by year 
avg_num_games_lm <- lm(`Avg-Games` ~ Year, data = new_table)
plot(new_table$Year, new_table$`Avg-Games`, main="League Avg Number of Games by Year", 
     xlab="Year", ylab="Average Number of Games Played", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1871, 2018, by=20), las=1)
axis(2, at=seq(0, 120, by=20), las=1)
abline(avg_num_games_lm, col="red", lwd=4)

#Need a model for after 1961 because thats when 162 games played began
new_table_1962 = new_table[new_table$Year > 1962,]
avg_num_games_lm_original <- lm(`Avg-Games` ~ Year, data = new_table)
avg_num_games_lm <- lm(`Avg-Games` ~ Year, data = new_table_1962)
plot(new_table$Year, new_table$`Avg-Games`, main="League Avg Number of Games by Year", 
     xlab="Year", ylab="Average Number of Games Played", type="b", pch=19, col="blue", 
     yaxt="n", xaxt="n")
axis(1, at=seq(1871, 2018, by=20), las=1)
axis(2, at=seq(0, 120, by=20), las=1)
abline(avg_num_games_lm_original, col="grey70", lwd=4)
abline(avg_num_games_lm, col="red", lwd=4)
legend("bottomright", legend=c("Regression For All Years",  
                            "Regression For Years After 1961"), 
       col=c("grey70", "red"), lty=1, lwd = 4)

#evaluate the models 
#Look at the residual standard error for each model. What percent of the average value 
# is the residual error? lower is better
summary(batting_avg_lm)     # RSE 0.01355 
mean(new_table$`Avg-BatAvg`)#     0.2707671
                            #     5.0043 %
summary(hr_per_at_bat_lm)     # RSE 0.003904
mean(new_table$`HR-Per-AtBat`)#     0.01786555
                              #     21.852 %
summary(rbi_per_at_bat_lm)     # RSE 0.01522
mean(new_table$`RBI-Per-AtBat`)#     0.1237055
                               #     12.303 %
summary(runs_per_game_lm)      # RSE 0.106  `                                                                                                                                                                                                                                                                                                                                                                                                           AZ`
mean(new_table$`Runs-Per-Game`)#     0.4975056
                               #     21.306 %
summary(avg_num_at_bats_lm) # RSE 32.2
mean(new_table$`Avg-AtBats`)#     335.6908
                            #     9.5922 %

summary(avg_num_games_lm)        # RSE 5.423
mean(new_table_1962$`Avg-Games`) #     105.9963
                                 #     5.116 %


#Using the models for prediciton (year = 2020, 2030, 2040, 2050, 2060)
new_years <- data.frame(matrix(ncol=1, nrow=5))
colnames(new_years) <- c("Year")
new_years$Year <- c(2020, 2030, 2040, 2050, 2060)

new_years$pred_batt_avg <- predict.lm(batting_avg_lm, newdata = new_years)
new_years$pred_hr_per_atbat <- predict.lm(hr_per_at_bat_lm, newdata = new_years)
new_years$pred_rbi_per_atbat <- predict.lm(rbi_per_at_bat_lm, newdata = new_years)
new_years$pred_runs_per_game <- predict.lm(runs_per_game_lm, newdata = new_years)
new_years$pred_num_atbats <- predict.lm(avg_num_at_bats_lm, newdata = new_years)
new_years$pred_num_games <- predict.lm(avg_num_games_lm, newdata = new_years)

new_years$pred_hr_total <- new_years$pred_hr_per_atbat * new_years$pred_num_atbats
new_years$pred_rbi_total <- new_years$pred_rbi_per_atbat * new_years$pred_num_atbats
new_years$pred_runs_total <- new_years$pred_runs_per_game * new_years$pred_num_games

future_batter_data = new_years %>% select(Year, pred_num_games, pred_num_atbats,
               pred_batt_avg, pred_hr_total, pred_rbi_total, pred_runs_total)
colnames(future_batter_data) <- c("Year", "Number of Games", "Number of At-Bats", 
                                  "Batting Average", "Homeruns", "RBIs", 
                                  "Runs")

