#IST 687
#Latika Mahesh Wadhwa
#Homework 6
#Assignment Due Date - October 11, 2018
#Submission Date - october 09,2018

#__________________________________________________________________________________________#
#                             Step A: Load and Merge datasets                              #
#__________________________________________________________________________________________#

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
  colnames(States) <- c("statename", "population", "popOver18", "percentOver18")
  return(States)
  
}

#1)	Read in the census dataset by calling the function readStatesInfo()
states <- readStatesInfo()


#2) Copy USArrests dataset:arrests
arrests <- USArrests


#Add a new column in arrests named stateName and populate it with rownames of arrests
arrests$stateName <- rownames(arrests)
arrests$stateName

#3) Merged dataframe with the attributes from both dataset:mergeDF
mergeDF <- merge(states, arrests, by ="stateName")
View(mergeDF)

#Des cribe mergeDF
str(mergeDF)
summary(mergeDF)

#__________________________________________________________________________________________#
#                   Step B: Explore the Data - Understanding distributions                 #        
#__________________________________________________________________________________________#

#Install package ggplot2
install.packages("ggplot2")
library(ggplot2)

#4) Histogram for the population 
myPlotPop <- ggplot(mergeDF, aes(x=population))             #Setting up the dataframe as mergeDF and x axis to population
myPlotPop <- myPlotPop + geom_histogram(binwidth = 50000)   #Setting up geom to histogram with binwidth of 50000
myPlotPop <- myPlotPop + ggtitle("Histogram of Population")  # Set the title
myPlotPop

#4) Histogram for the Murder rate 
myPlotMur <- ggplot(mergeDF, aes(x=Murder))             #Setting up the dataframe as mergeDF and x axis to Murder
myPlotMur <- myPlotMur + geom_histogram(binwidth = 1)       #Setting up geom to histogram with binwidth of 1
myPlotMur <- myPlotMur + ggtitle("Histogram of Murder")     # Set the title
myPlotMur

#4) Histogram for the Assault 
myPlotAssault <- ggplot(mergeDF, aes(x=Assault))             #Setting up the dataframe as mergeDF and x axis to Assault
myPlotAssault <- myPlotAssault + geom_histogram(binwidth = 5) #Setting up geom to histogram with binwidth of 5
myPlotAssault <- myPlotAssault + ggtitle("Histogram of Assault")  # Set the title
myPlotAssault

#4) Histogram for the Rape 
myPlotRape <- ggplot(mergeDF, aes(x=Rape))             #Setting up the dataframe as mergeDF and x axis to Rape
myPlotRape <- myPlotRape + geom_histogram(binwidth = 1)   #Setting up geom to histogram with binwidth of 1
myPlotRape <- myPlotRape + ggtitle("Histogram of Rape")  # Set the title
myPlotRape

#4) Histogram for the Urban Population
myPlotUPop <- ggplot(mergeDF, aes(x=UrbanPop))             #Setting up the dataframe as mergeDF and x axis to Urban Population
myPlotUPop <- myPlotUPop + geom_histogram(binwidth = 1)     #Setting up geom to histogram with binwidth of 1
myPlotUPop <- myPlotUPop + ggtitle("Histogram of Urban Population")  # Set the title
myPlotUPop

#5)- Boxplot for the population
myBoxPlotPop <- ggplot(mergeDF, aes(x=factor(0), y=population)) + geom_boxplot()
myBoxPlotPop

#5)- Boxplot for the murder rate
myBoxPlotMur <- ggplot(mergeDF, aes(x=factor(0), y=Murder)) + geom_boxplot()
myBoxPlotMur

#6) I would choose histogram over box plot because histogram show the number of values within the limit of a bin size
# and huge dataset can be visualized easily while boxplot distributes the data based on the minimum, first quartile,
# median, third quartile, and maximum of the data set which makes it difficu to visualize


#__________________________________________________________________________________________#
#                   Step C: Which State had the Most Murders - bar charts                  #        
#__________________________________________________________________________________________#

#7)	Calculate the number of murders per state and add it into a new column of the erdf datadframe: numMurders
mergeDF$numMurders <- mergeDF$population*mergeDF$Murder/10000
View(mergeDF)  #View the datadframe

#8)	Generate a bar chart, with the number of murders per state
#SET    Dataframe = mergeDF        X-axis=stateNum          Y-axis= numMurders         Plot type-Barchart
g <- ggplot(mergeDF, aes(x=stateName, y= numMurders)) + geom_col()  
g

#9) Rotate text on the X axis and add a title named "Total Murders".
g <- g+theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate text on the X axis
g <- g+ ggtitle("Murders by State")                             #Set up the title
g

#10)	 Generate a new bar chart, the same as in the previous step and sort the x-axis by the murder rate
#SET    Dataframe = mergeDF        X-axis=stateNum          Y-axis= numMurders         Plot type-Barchart
g <- ggplot(mergeDF, aes(x= reorder(stateName, numMurders) , y=numMurders))+ geom_col()
g <- g +theme(axis.text.x = element_text(angle = 90))  #Rotate text on the X axis
g

#11) Generate a third bar chart, the same as the previous step and show percentOver18 as the color of the bar
g <- ggplot(mergeDF, aes(x= reorder(stateName, numMurders) , y=numMurders, fill=percentOver18))+ geom_col()
g <- g +theme(axis.text.x = element_text(angle = 90))      #Rotate text on the X axis
g <- g+ ggtitle("Total Murders")                             #Set up the title
g


#__________________________________________________________________________________________#
#                                  Step D: Explore Murders - scatter chart                 #        
#__________________________________________________________________________________________#


#Generate a scatter plot - have population on the X axis, the percent over 18 on the y axis, and the size & color represent the murder rate
scatterplot <- ggplot(mergeDF, aes(x=population, y= percentOver18)) +geom_point(aes(size=Murder, color=Murder)) + scale_color_gradient(low="white", high="red")
scatterplot

