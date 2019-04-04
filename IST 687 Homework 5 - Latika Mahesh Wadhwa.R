#IST 687
#Latika Mahesh Wadhwa
#Homework 5
#Assignment Due Date - September 27, 2018
#Submission Date - September 27,2018

#install package RCurl
install.packages("RCurl")
library(RCurl)

#install package RJSONIO
install.packages("RJSONIO")
library(RJSONIO)

###########################################################################################
#                               Step A: Load the data
###########################################################################################

#Save the URL link in a variable:jsonURL
jsonURL <- "http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
#Save the data to datasetURL
datasetURL <- getURL(jsonURL)  

#Read the data
mydata <- fromJSON(datasetURL, simplify = FALSE, nullValue = NA)
View(mydata)
myList <- mydata[[2]]

#Find no of rows
numRows <- length(myList)                  

#Convert data into table format
dataset <- data.frame(matrix(unlist(myList), nrow=numRows, byrow=T), stringsAsFactors = FALSE)

#view dataframe
View(dataset)                                


###########################################################################################
#                               Step B: Clean the data
###########################################################################################

#Delete first 8 columns of the dataset
dataset <- dataset[,-1:-8]

#view dataframe
View(dataset)       

#Change column names
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME",
                    "ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD",
                    "DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME",
                    "COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST",
                    "INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
colnames(dataset) <- namesOfColumns
#view dataframe
View(dataset)       


###########################################################################################
#                               Step C: Explore the data - using the dataframe you created
###########################################################################################
#import sqldf
install.packages("sqldf")
library(sqldf)

#No of accidents with injury
sqldf("SELECT COUNT(INJURY) AS Count_of_accidents_with_injury
      FROM dataset
      WHERE INJURY LIKE '%YES%'
      ")


#NO of accidents happened on Sunday?
sqldf("SELECT COUNT(DAY_OF_WEEK) AS No_Of_Accidents_on_Sunday
      FROM dataset
      WHERE DAY_OF_WEEK LIKE '%SUNDAY%'
      ")


#NO OF injuries occurred each day of the week
sqldf("SELECT DAY_OF_WEEK, COUNT(DAY_OF_WEEK) AS No_Of_Accidents_on_Sunday
      FROM dataset
      WHERE INJURY LIKE '%YES%'
      GROUP BY DAY_OF_WEEK
      ")


###########################################################################################
#                               Step D: Explore the data - using dplyr
###########################################################################################
#INSTALL DPLYR
install.packages("dplyr")
library(dplyr)
df.GroupBydays <- group_by(dataset, DAY_OF_WEEK)


accidents <- summarize(df.GroupBydays, count = n())

accidents




###########################################################################################
#                               Step E: Explore the distribution of the number of vehicles 
#                                       in accidents
##########################################################################################
# Number of vehicles in accidents on Friday
friday_accident_vehicle_count <-sqldf("SELECT count(VEHICLE_COUNT) AS VEHICLE_COUNT
                                  FROM dataset
                                  WHERE DAY_OF_WEEK LIKE '%FRIDAY%'
                                  AND VEHICLE_COUNT != 'NA'
                                  GROUP BY VEHICLE_COUNT
                                 ")
#Convert in numeric form
num_friday_accident_vehicle_count<- as.numeric(friday_accident_vehicle_count$VEHICLE_COUNT)

# Histogram  of the number of vehicles in accidents on Friday
hist(num_friday_accident_vehicle_count, breaks = 20 )

# QUANTILE  of the number of vehicles in accidents on Friday
quantile(num_friday_accident_vehicle_count)

# Number of vehicles in accidents on SUNDAY
sunday_accident_vehicle_count <-sqldf("SELECT count(VEHICLE_COUNT) AS VEHICLE_COUNT
                                  FROM dataset
                                  WHERE DAY_OF_WEEK LIKE '%SUNDAY%'
                                  AND VEHICLE_COUNT != 'NA'
                                  GROUP BY VEHICLE_COUNT
                                 ")

#Convert in numeric form
num_sunday_accident_vehicle_count<- as.numeric(sunday_accident_vehicle_count$VEHICLE_COUNT)

# Histogram  of the number of vehicles in accidents on Sunday
hist(num_sunday_accident_vehicle_count, breaks = 20 )


# QUANTILE  of the number of vehicles in accidents on Sunday
quantile(num_sunday_accident_vehicle_count)

#Histogram comparision
red <- rgb(1, 0, 0, 0.5) # red
blue <- rgb(0, 0, 1, 0.5) #blue
hist(num_friday_accident_vehicle_count, col = red, breaks = 15, main = "Comparision of car distribution", ylim = c(0, 5), xlim = c(0, 1400),ylab="No of vehicles", xlab = "Distribution")
par(new = TRUE)
hist(num_sunday_accident_vehicle_count, col = blue, breaks = 15, main = "", ylim = c(0, 5), xlim = c(0, 1400), xlab = "", ylab = "")

#Quantile Comparision
boxplot(num_friday_accident_vehicle_count, num_sunday_accident_vehicle_count,
        names = c("Friday Accident Count", "Sunday Accident Count"),
        main = "Distribution Comparision",
        ylab = "Frequency")
