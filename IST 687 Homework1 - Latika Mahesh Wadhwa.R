#IST 687
#Latika Mahesh Wadhwa
#Homework 1
#Assignment Due Date - September 5, 2018
#Submission Date - August 30, 2018

#                                Step A
#Define vector grades
grades <- c(4.0, 3.3, 3.7)

#Define vector courseName
courseName <- c("Bio", "Math", "History")


#Define variable BetterThanB
BetterThanB <- 3

#                                Step B
#Calculate Average of vector grades
mean(grades)

#Calculate the  number of observations in grades: total.length
total.length <- length(grades)

#Print total.length
total.length

#Calculate the sum of elements in vector grades: total
total <- sum(grades)

#Recompute the average of all the grades by dividing total by total.length
total/total.length

#                                Step C
#Compute the max Grade : maxG
maxG <- max(grades)

#Compute the min Grade : minG
minG <- min(grades)

#                                Step D
#Define vector betterGrades 
betterGrades <- grades + 0.3

#compute average of betterGrades
mean(betterGrades)

#                                Step E
#Check if maxG is greater than 3.5
if(maxG > 3.5)
{
  "Yes"
} else {
  "No"
}

#Check if minG is greater than BetterThanB
if(minG > BetterThanB)
{
  "Yes"
} else {
  "No"
}

#                                Step F
#Display the name of the second class in vector courseName
courseName[2]

