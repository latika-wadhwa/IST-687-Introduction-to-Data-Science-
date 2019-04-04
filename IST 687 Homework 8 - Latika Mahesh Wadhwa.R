#IST 687
#Latika Mahesh Wadhwa
#Homework 7
#Assignment Due Date -  October 25, 2018
#Submission Date -  October 25, 2018


install.packages("RJSONIO")
library(RJSONIO)

install.packages("RCurl")
library(RCurl)
################################################################################################################
#                                     Step A: Load and condition the data  
###############################################################################################################
#Load the hotelSurveySherison.json 
jsonURL <- "hotelSurveySherison.json"


#Read the data
mydata <- fromJSON(jsonURL, simplify = TRUE, nullValue = NA)
hotel_survey <- data.frame(mydata)
typeof(hotel_survey)
View(hotel_survey)


#2.	Use the str command to make sure you  see the required attributes
str(hotel_survey)
hotel_survey <- hotel_survey[,-11]
str(hotel_survey)

################################################################################################################
#                                           Step B: Explore the data    
###############################################################################################################
#3.	Create bivariate plots for each of the attributes.

#Hotel size vs Overall Customer Satisfaction
hotel_size<-ggplot(data = hotel_survey,aes(x=hotelSize,y=overallCustSat))+geom_point()
hotel_size +geom_jitter()+labs(x="Hotel Size",y="Overall Customer Satisfaction")

#Check_in satisfaction vs Customer Satisfaction
check_in_sat<-ggplot(data = hotel_survey,aes(x=checkInSat,y=overallCustSat))+geom_point()
check_in_sat +geom_jitter()+labs(x="Check-In Satisfacton",y="Overall Customer Satisfaction")

#Hotel State vs Customer Satisfaction
hotel_state<-ggplot(data = hotel_survey,aes(x=hotelState,y=overallCustSat))+geom_point()
hotel_state+geom_jitter() +labs(x="Hotel State",y="Overall Customer Satisfaction")


#Hotel cleanliness vs Customer Satisfaction
hotel_clean<-ggplot(data = hotel_survey,aes(x=hotelClean,y=overallCustSat))+geom_point()
hotel_clean +geom_jitter()+labs(x="Hotel State",y="Overall Customer Satisfaction")

#Hotel Friendliness vs Custome Satisfaction
hotel_friendly<-ggplot(data = hotel_survey,aes(x=hotelFriendly,y=overallCustSat))+geom_point()
hotel_friendly +geom_jitter()+labs(x="hotel Friendliness",y="Overall Customer Satisfaction")

#Gender vs Custome Satisfaction
gender_plot<-ggplot(data = hotel_survey,aes(x=gender,y=overallCustSat))+geom_point()
gender_plot +geom_jitter()+labs(x="hotelFriendliness",y="Overall Customer Satisfaction")


#Guest Age vs Custome Satisfaction
hotel_age<-ggplot(data = hotel_survey,aes(x=guestAge,y=overallCustSat))+geom_point()
hotel_age +geom_jitter()+labs(x="Guest Age",y="Overall Customer Satisfaction")

#Length of stay vs Custome Satisfaction
length_of_stay<-ggplot(data = hotel_survey,aes(x=lengthOfStay,y=overallCustSat))+geom_point()
length_of_stay +geom_jitter()+labs(x="Length of stay",y="Overall Customer Satisfaction")

#When booked Trip vs Custome Satisfaction
when_booked_trip<-ggplot(data = hotel_survey,aes(x=whenBookedTrip,y=overallCustSat))+geom_point()
when_booked_trip +geom_jitter()+labs(x="When Booked",y="Overall Customer Satisfaction")

################################################################################################################
#                                           Step B: Explore the data    
###############################################################################################################

#5.	Next, create one regression model predicting the overall customer satisfaction from the other variables (but not the freeText response). Refer to page 202 in the text for syntax and explanations of lm( ). Make sure to include all predictors in one model - NOT different models each with one predictor.
model1<-lm(formula =overallCustSat~.,data=hotel_survey)
summary(model1)

#6.	Report the R-Squared in a comment. Which of the predictors are statistically significant in the model? 
#In a comment, report the coefficients (AKA slopes or B-weights) for each predictor that is statistically significant. 

#     checkInSat   -  -2.381e-01  Significance 0 -> high significance
#     hotelClean  -    4.042e-02  Significance 0 -> high significance
#    hotelFriendly-    1.122e+00  Significance 0 -> high significance
#    guestAge   -     -1.205e-01  Significance 0 -> high significance
#    lengthOfStay-    -3.284e-1   Significance 0 ->high significance
#    whenBookedTrip-   6.421e-03  Significance 0 -> high significance


#7.	Write a block comment that explains in a narrative your overall
# interpretation of the model. Make sure to refer to each variable 
#(one dependent and three independent) by a descriptive name 
#(i.e., not X1, X2, etc.).

model2 <- lm(formula= overallCustSat ~ checkInSat,data= hotel_survey)
summary(model2)
#Multiple R-squared:  0.001325,	Adjusted R-squared:  0.001225 

model3 <- lm(formula= overallCustSat ~ hotelClean,data= hotel_survey)
summary(model3)
#Multiple R-squared:  0.125,	Adjusted R-squared:  0.1249 


model4 <- lm(formula= overallCustSat ~ hotelFriendly,data= hotel_survey)
summary(model4)
#Multiple R-squared:  0.3785, Adjusted R-squared:  0.3784 

model5 <- lm(formula= overallCustSat ~ guestAge,data= hotel_survey)
summary(model5)
#Multiple R-squared:  0.003735,	Adjusted R-squared:  0.003635 

model6 <- lm(formula= overallCustSat ~ lengthOfStay,data= hotel_survey)
summary(model6)
#Multiple R-squared:  2.089e-05,	Adjusted R-squared:  -7.913e-05 

model7 <- lm(formula= overallCustSat ~ whenBookedTrip,data= hotel_survey)
summary(model7)
#Multiple R-squared:  4.482e-05,	Adjusted R-squared:  -5.519e-05 


# The closer the value of R-square is to 1 the significant the mode is. 
# in our case guestAge, hotelFriendly and hotelClean have values closer to 1

#hotelClean    0.125
#guestAge      0.003735
#hotelFriendly 0.3785

##8.	Next, create a different regression model predicting the overall 
#customer satisfaction from the one variable you think is best.
#Then create another using two variables.

# HotelFriendly give out the best R-Squared value of 0.3785 compared to guestAge  and hotelClean   
model8 <- lm(formula= overallCustSat ~ hotelFriendly,data= hotel_survey)
summary(model8)
#Multiple R-squared:  0.3785,	Adjusted R-squared:  0.3784 

#Creating model with hotelFriendly and hotelClean as variables
model9<-lm(formula = overallCustSat ~ hotelFriendly + hotelClean,data=hotel_survey)
summary(model9)
#Multiple R-squared:  0.3919,	Adjusted R-squared:  0.3918 

#Creating model with hotelFriendly and guestAge as variables
model10<-lm(formula = overallCustSat ~ hotelFriendly + guestAge,data=hotel_survey)
summary(model10)
#Multiple R-squared:  0.5936,	Adjusted R-squared:  0.5936 


#9.	Write a block comment comparing the two lm models in #8	
#The resulted Multiple R-squared value makes it pretty obvious that hotelFriendly along with guestAge have better predicton and are the potential factors that improves customer happiness


