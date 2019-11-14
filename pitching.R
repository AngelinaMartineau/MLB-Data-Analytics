#  The purpose of this file is to analyze the pitching data 
#  for Major League Baseball Players from 1875 to 2018

#Libraries


#Read in the fielding data
pitching_data <- read.csv(file="C:\\users\\martia13\\Desktop\\Data Analytics\\Project\\MLB-Data-Analytics-master\\baseballdata\\core\\Pitching.csv", header=T)
View(pitching_data)
num_pitching_data = nrow(pitching_data)

#Remove lines where the Games is 0 
modified_pitching_data <- pitching_data[!(pitching_data$G==0 | is.na(pitching_data$ERA)),]
View(modified_pitching_data)
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
  print(yearId)
  
  #Get the Shutout percentage 
  shutout_perc = sum(year$SHO)/sum(year$G) * 100
  print(paste("", "Shutout Percentage: ", as.numeric(shutout_perc), sep="     "))
  
  #Get the average number of outs pitched 
  avg_num_outs_pitched = sum(year$IPouts)/nrow(year)
  print(paste("", "Average outs pitched: ", as.numeric(avg_num_outs_pitched), sep="     "))
  
  #Get the average number of hits allowed per out
  avg_num_hits = sum(year$H)/sum(year$IPouts)
  print(paste("", "Average hits per out: ", as.numeric(avg_num_hits), sep="     "))
  
  #Get the average ERA 
  avg_era = sum(year$ER)/sum(year$IPouts) * 27
  print(paste("", "Average ERA: ", as.numeric(avg_era),sep="     "))
  
  #Get the average number of home runs allowed per out 
  avg_hrs = sum(year$HR)/sum(year$IPouts)
  print(paste("", "Average HRs per out: ", as.numeric(avg_hrs), sep="     "))
  
  #Get the average number of walks allowed per out
  avg_walks = sum(year$BB)/sum(year$IPouts)
  print(paste("", "Average BBs per out: ", as.numeric(avg_walks), sep='     '))
  
  #Get the average number of strike outs allowed 
  avg_SO = sum(year$SO)/sum(year$IPouts)
  print(paste("", "Average SOs per out: ", as.numeric(avg_SO), sep="     "))
  
  #Add the new data to new_table
  new_table <- rbindlist(list(new_table, as.list(c(yearId, shutout_perc, avg_num_outs_pitched, 
                                                   avg_num_hits, avg_era, avg_hrs, avg_walks, avg_SO))))
}

#new_table now contains the data that we will be using for the multi-linear regression
















