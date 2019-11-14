#  The purpose of this file is to analyze the batting data 
#  for Major League Baseball Players from 1875 to 2018

#Libraries
library(data.table)

#Read in the batting data
batting_data <- read.csv(file="C:\\users\\martia13\\Desktop\\Data Analytics\\Project\\MLB-Data-Analytics-master\\baseballdata\\core\\Batting.csv", header=T)
View(batting_data)
num_batting_data = nrow(batting_data)

#Remove lines where the At-Bats is 0 
#RBI had NA values for some entries, so those lines were removed. Increased 
#data removed from 15.44% to 16.55%
modified_batting_data <- batting_data[!(batting_data$AB==0 | batting_data$G==0 | is.na(batting_data$RBI)), ]
View(modified_batting_data)
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
View(new_table)

#Go through the data for each year - Split the data by year 
split_by_year <- split(modified_batting_data, modified_batting_data$yearID)

for(year in split_by_year) {
  yearId = year$yearID[1]
  print(nrow(year))
  
  #Get the average batting-average
  avg_bat_avg = sum(year$H)/sum(year$AB)
  #print(paste0('Year: ', yearId))
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

#new_table now contains the data that we will be using for the multi-linear regression








