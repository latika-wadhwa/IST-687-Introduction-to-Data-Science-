#IST 687
#Latika Mahesh Wadhwa
#Homework 4
#Assignment Due Date - September 27, 2018
#Submission Date - September 27,2018

          #Part A: Function to reveal the distribution of a vector of numeric values
#Create a function: printVecInfo
printVecInfo <- function(Vector)
{
#2 Pint the information for the vector supplied in the argument
  cat(" Mean: ",mean(Vector),"\n",
      "Median: ",median(Vector),"\n",
      "Min: ",min(Vector),"\n",
      "Max: ",max(Vector),"\n",
      "Standard Deviation: ",sd(Vector),"\n",
      "Quantile 0.05 is:  ",quantile(Vector,probs = 0.05),"\n",
      "Quantile 0.95 is:  ",quantile(Vector,probs = 0.95),"\n")
}

#3. Create a test function with vector (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 50).
vectest <- 1:10

#Test the function.
printVecInfo(vectest)

#Make histogram
hist(vectest)

#                         Part B: Read the census dataset              
#5.	Read in the Census dataset
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
      
#                        Part C: Sample from the state population data frame
#6 Sample 20 observations from states$population and use printVecInfo( ) to display the characteristics of the resulting sample, and then display the results as a histogram.
samp1 <- sample(states$population,size = 20)
View(samp1)    #view sample dataset
printVecInfo(samp1)     #use the function printVecInfo on the sample just created
hist(samp1)           #make histogram

#7 Repeat step 6
samp2 <- sample(states$population,size = 20)
View(samp2)
printVecInfo(samp2)
hist(samp2)

#repeat step 6
samp3 <- sample(states$population,size = 20)
View(samp3)
printVecInfo(samp3)
hist(samp3)

#8 Each Histogram gives different resuts because the function sample generates random set of 20 values everytime it is called.

#                             Part D: Replicate the sampling
#9 Replicate the sampling 2000 times  
repl1 <- replicate(2000,sample(states$population,size = 20))
#Use printVecInfo( ) to display the characteristics of the resulting replicated sample
printVecInfo(repl1)
#Generate a histogram
hist(repl1)

#10 repeat the replicate step(9) 
repl2 <- replicate(2000,sample(states$population,size = 20))
printVecInfo(repl2)
hist(repl2)

#10 Repeat the replicate step(9)
repl3 <- replicate(2000,sample(states$population,size = 20))
printVecInfo(repl3)
hist(repl3)

#11 Replication re-evaluates its expression for each replication that is created0. 
#Therefore 2000 different samples of size 20 are created 3 times. Each of them being different from each other results in different graph
#For instance samp 1 has 1 sample of 20 while repl1 has 2000*20 i.e 40000 values and the range is also different for both of them depending by the value selected by the sample.
