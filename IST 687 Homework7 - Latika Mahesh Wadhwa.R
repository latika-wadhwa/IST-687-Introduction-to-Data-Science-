#IST 687
#Latika Mahesh Wadhwa
#Homework 7
#Assignment Due Date -  October 18, 2018
#Submission Date -  October 18, 2018

#Create function readStatesInfo
readStatesInfo <- function() 
{
  csv_file <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
  States <- read.csv(url(csv_file))
  summary(States)     #summary for States
  str(States)         #structure of States
  
  #Clean the dataframe      
  head(States,5)      #view top 5 rows
  tail(States,5)      #view bottom 5 rows
  
  #Delete the First Row of the dataframe
  nrow(States)        #Display number of rows
  States <- States[-1,]            #Delete the first row
  nrow(States)        #Display number of rows
  
  #	Remove the last Row (for Puerto Rico)
  num_row <- nrow(States)        
  num_row                  
  States <- States[-num_row,]
  
  #Delete the first 4 columns
  States <- States[,-1:-4]
  
  #Rename the columns
  colnames(States) <- c("stateName", "population", "popOver18", "percentOver18")
  View(States)
  
  #Copy USArrests dataset:arrests
  arrests <- USArrests
  View(arrests)
  #Add a new column in arrests named stateName and populate it with rownames of arrests
  arrests$stateName <- rownames(arrests)
  arrests$stateName
  
   Merged dataframe with the attributes from both dataset:mergeDF
  mergeDF <- merge(States, arrests, by ="stateName")
  return(mergeDF)
}

#############################################################################################################
#                                    Step A: Load and Merge datasets
#############################################################################################################

#1)	Read in the census dataset by calling the function readStatesInfo()
mergeDF1 <- readStatesInfo()
View(mergeDF1)

#Describe mergeDF
str(mergeDF1)
summary(mergeDF1)

#2)	 Define area of each state (state.area), and the center of each state (state.center)
stateName <- mergeDF1$stateName
area <- state.area
center <- state.center

#Create a dataframe with column vvalues as stateName, area, x-co-ord and y-co-ord:otherDf
otherDf <- data.frame(stateName,area,center)
View(otherDf)                    #view otherDf

#Merge otherDf and mergeDF1 (by stateName):mergeDF2 
mergeDF2 <- merge(mergeDF1,otherDf, by='stateName')
View(mergeDF2)          #view mergeDF2

#Describe mergeDF2
str(mergeDF2)
summary(mergeDF2)

#############################################################################################################
#                                     Step B: Generate a color coded map
#############################################################################################################

#Install package ggplot2
install.packages("ggplot2")
library(ggplot2)

#Install package ggmap
install.packages("ggmap")
library(ggmap)

us <- map_data("state")
View(us)

#Convert stateName column values to lower case and store it in a new column:state
mergeDF2$state <- tolower(mergeDF2$stateName)
View(mergeDF2)               #view mergeDF2

#3)	Create a color coded map, based on the area of the state 
mapArea <- ggplot(mergeDF2, aes(map_id = state))
mapArea <- mapArea + geom_map(map = us, aes(fill = mergeDF2$area))
mapArea <- mapArea + expand_limits(x = us$long, y = us$lat) + coord_map() +ggtitle("US map based on the area per state")
mapArea

#############################################################################################################
#    Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 
#############################################################################################################

#4)color code the map based on the murder rate of each state.
mapMurder <- ggplot(mergeDF2, aes(map_id = state))
mapMurder <- mapMurder + geom_map(map = us, aes(fill = mergeDF2$Murder))
mapMurder <- mapMurder + expand_limits(x = us$long, y = us$lat) + coord_map() +ggtitle("US map based on the murder per state")
mapMurder

#5)Show the population as a circle per state (the larger the population, 
#the larger the circle), using the location defined by the center of each state
mapMurPop  <- ggplot(mergeDF2, aes(map_id = state))
mapMurPop <- mapMurPop + geom_map(map = us, aes(fill = mergeDF2$Murder))
mapMurPop <- mapMurPop + expand_limits(x = us$long, y = us$lat) + coord_map() +ggtitle("US based on the Murder rate per state")
mapMurPop <- mapMurPop + geom_point(x= mergeDF2$x, y=mergeDF2$y, aes( size = mergeDF2$population)) 
mapMurPop <- mapMurPop +  ggtitle("US based on the Murder rate per state")
mapMurPop

#############################################################################################################
#                                       Step D: Zoom the map
#############################################################################################################


#Define NYC coordinates:newYorkCity 
newYorkCity <-  geocode(source = "dsk", "nyc, new york,ny")
newYorkCity 

#6)	Repeat step C, but only show the states in the north east
NYC_zoomed_map <- ggplot(mergeDF2, aes(map_id = state))
NYC_zoomed_map <- NYC_zoomed_map + geom_map(map = us, aes(fill = mergeDF2$Murder)) 
NYC_zoomed_map <- NYC_zoomed_map + expand_limits(x  = us$long, y = us$lat ) + coord_fixed(xlim = c(newYorkCity$lon -10,newYorkCity$lon +10), ylim = c(newYorkCity$lat +10,newYorkCity$lat -10)) 
NYC_zoomed_map <- NYC_zoomed_map + geom_point(x= mergeDF2$x, y=mergeDF2$y, aes( size = mergeDF2$population)) 
NYC_zoomed_map <- NYC_zoomed_map + ggtitle("North East of US based on the Murder rate per state")
NYC_zoomed_map
