#IST 687
#Latika Mahesh Wadhwa
#Homework 3
#Assignment Due Date - September 20, 2018
#Submission Date - September 20,2018

#                           Step A: Use read.csv( ) and url( ) to read a CSV file form the web into a data frame
#1. Read.csv function reads a file csv_file in table format and create a data frame from it
csv_file <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
dfStates <- read.csv(url(csv_file))

summary(dfStates)     #summary of dfStates
str(dfStates)         #structure of dfStates

#                           Step B: Clean the dataframe

#2.	Use View( ), head( ), and tail( ) to examine the data frame. 
View(dfStates)        #view the dfStates
head(dfStates,5)      #top 5 rows of dfStates
tail(dfStates,5)      #bottom 5 rows of dfStates

#3.    Delete the First Row of the dataframe
nrow(dfStates)
dfStates <- dfStates[-1,]
nrow(dfStates)
View(dfStates)

#4.	Remove the last Row (for Puerto Rico)
num_row <- nrow(dfStates)
num_row
dfStates <- dfStates[-num_row,]
View(dfStates)

#5.	Make sure there are exactly 51 rows 
nrow(dfStates)
View(dfStates)

#6.	Make sure there are precisely 4 columns, with the following names:
# stateName, population, popOver18, percentOver18. 

#Delete unwanted columns
ncol(dfStates)
dfStates <- dfStates[,-1:-4]

#view columns and no of columns
View(dfStates)
ncol(dfStates)

#reset column names
colnames(dfStates) <- c("statename", "population", "popOver18", "percentOver18")
View(dfStates)


#                  Step C: Create a Function

#7.	Create a function that takes no parameters and returns the clean dataframe created in step 6 above.
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

#call the function readStatesInfo()
states <- readStatesInfo()
View(states)

#8.	Calculate the average population of the states
avgpop <-  mean(states$population)
avgpop

#9. State with the highest population  
maxPol.index <- which.max(states$population)
maxPol.index
statewithhighestpop <- dfStates$statename[maxPol.index]
statewithhighestpop


#10.Histogram of the state populations, what do you observe?
hist(states$population, breaks = 20)

#observation
#The vertical axis divides the population into small bins while the horizontal axis states the count of the states that fit in the range of the population
#It is clear that most of the states have population between 0e+00 and 2e+07 and very few exist beyond them.
#the histogram is right skewed 

#11.	Sort the data frame by population 
View(states)
sortState <- states[order(states$population),]
View(sortState)

#12.	Show the 10 states with the lowest populations
head(sortState,10)

sortState
#13.	Use barplot( ) to create a plot of each of the population from the sorted dataframe.  
barplot(sortState$population, xlab= "Frequency", ylab="Population" )

#observation
#The barplot showcases the number of population of each staste arranged in ascending order.
#the vertical axis states the count of the population while the horizontal axis states the states arranged ascending according to the population
#The minimum population is as low as 579315 and the highesh is as high as 39536653


                                                                            

