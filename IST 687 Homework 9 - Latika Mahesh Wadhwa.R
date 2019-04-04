#Course number IST687
#Name : Latika Wadhwa
#HW 8 -  Association Rules Mining  
#Due Date :-11/08/2018
#Date the assignment is submitted:- 11/08/2018

#install packages
install.packages("RJSONIO")
install.packages("ggplot2")
library(RJSONIO)
library(ggplot2)

################################################################################################################
#                                           Part A: Explore Data Set
################################################################################################################

#1)	Load the dataset: hotelSurveyBarriot.json 
data<-'hotelSurveySherison.json'

#2)	Name the dataframe hotelSurvey
hotelSurvey<-fromJSON(data,simplify = TRUE,nullValue = NA)
hotelSurvey<-data.frame(hotelSurvey)

#View the dataframe
View(hotelSurvey)


################################################################################################################
#                                           Part B: Explore Data Set
################################################################################################################

#3)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command
class(hotelSurvey)
str(hotelSurvey)


#Look at the summary 
summary(hotelSurvey)



#4)	Map each numeric attribute to a category by creating a function

surveyBucket <-  function(vectorvar)
{
  vBuckets <- replicate(length(vectorvar), "Average")
  vBuckets[vectorvar > 7] <- "High"
  vBuckets[vectorvar < 7] <- "Low"
  return(vBuckets)
  
}

otherBucket <-  function(vectorvar)
{
  q <- quantile(vectorvar, prob= seq(0, 1, 0.6))
  vBuckets <- replicate(length(vectorvar), "Average")
  vBuckets[vectorvar <= q[1]] <- "Low"
  vBuckets[vectorvar > q[2]] <- "High"
  View(vBuckets)
  return(vBuckets)
}

happyCust <- surveyBucket(hotelSurvey$overallCustSat)
hotelSurvey$overallCustSat <- happyCust
View(hotelSurvey)

hotelcleaned <- surveyBucket(hotelSurvey$hotelClean)
hotelSurvey$hotelClean <- hotelcleaned
View(hotelSurvey)


StatusCheck <- surveyBucket(hotelSurvey$checkInSat)
hotelSurvey$checkInSat <- StatusCheck
View(hotelSurvey)


StayLength <-  otherBucket(hotelSurvey$lengthOfStay)
hotelSurvey$lengthOfStay <- StayLength
View(hotelSurvey)

#5)	Count the people in each category of for the age and friendliness attributes
#age
age <- otherBucket(hotelSurvey$guestAge)
hotelSurvey$guestAge <- age
t1 <- table(age)
t1

#friendliness
friendliness <- otherBucket(hotelSurvey$hotelFriendly)
hotelSurvey$hotelFriendly <- friendliness
t2 <- table(friendliness)
t2

#6)	Express the results of problem 3 as percentages by sending the results of the table( ) 
prop.table(t1)
prop.table(t1)*100
prop.table(t2)
prop.table(t2)*100

#7)	Show a "contingency table" of percentages for the age and the overall satisfaction variables together. 
t3 <- table(age, happyCust)
t3
prop.table(t3)
prop.table(t3)*100


#This results in a table having rows as age and columns as happyCust and the value suggests the number for the intersecting row and column.
# for example there are 2748 people wit average and average happyCust rating

################################################################################################################
#                                           Part C: Coerce the data frame into transactions
################################################################################################################

#8)	Install and library two packages: arules and arulesViz.

#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)


hotelSurvey[1:3]<-lapply(hotelSurvey[1:3], as.factor)
hotelSurvey[5:6]<-lapply(hotelSurvey[5:6], as.factor)
hotelSurvey[8:10]<-lapply(hotelSurvey[8:10], as.factor)


hotelSurveyX <- as(hotelSurvey,"transactions")  
str(hotelSurveyX)


inspect(hotelSurveyX[1:10,])
itemFrequency(hotelSurveyX[,c(1:9)])
itemFrequencyPlot(hotelSurveyX[,c(1:4)])


################################################################################################################
#                                           Part D: Use arules to discover patterns
################################################################################################################
#11)	Run the apriori command to try and predict happy customers (as defined by 
#their overall satisfaction being high - above 7).

ruleset<-apriori(hotelSurveyX,parameter = list(support=0.1,confidence=0.5))


#12)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset. 
inspect(ruleset)

#13)	 If you had to provide two rules to the hotel owner 
#(in terms of what helps drive high overall customer satisfaction,
#what would those two rules be?  
#Use a block comment to explain your answer.

ruleset2<-ruleset[quality(ruleset)$lift>2]
inspect(ruleset2)

# FOllowing are the two rules that I would want to suggest:

#hotelClean=High,guestAge=High,whenBookedTrip=High}=>{overallCustSat=High} 
#  0.1331  0.9568656 2.014030  1331

#If hotelcean = high,guestage = high and whenbookedtrip = high also,(->)
# then the overall satisfaction is high

#{hotelFriendly=Average,whenBookedTrip=High} => {overallCustSat=High}   
# 0.1202  0.9860541 2.075467  1202

#If  the hotelfriendliness = low, whenbookedtrip = high, (=>) then the overalcustsat is high

