#Course number IST687
#Name : Latika Wadhwa
#HW 10 -  Support Vector Machine  
#Due Date :-11/15/2018
#Date the assignment is submitted:- 11/15/2018

#install RJSONIO packages
install.packages("RJSONIO")
install.packages("ggplot2")

#install( ) kernlab R package
install.packages("kernlab") 
library(kernlab)
library(RJSONIO)
library(ggplot2)


################################################################################################################
#                                           Part A: Load and condition the data  
################################################################################################################

#1)	Load the dataset: hotelSurveyBarriot.json 
data<-'hotelSurveySherison.json'

#2)	Name the dataframe hotelSurvey
hotelSurvey<-fromJSON(data,simplify = TRUE,nullValue = NA)
hotelSurvey<-data.frame(hotelSurvey)

#View the dataframe
View(hotelSurvey)
  
################################################################################################################
#                                          Part B: Create a happy customer variable 
################################################################################################################

#Create a new column happy_customer that sets value to happy if overallCustSat is 8 or higher else unhappy
hotelSurvey$happy_customer <- ifelse(hotelSurvey$overallCustSat>=8,"happy","not happy")
table(hotelSurvey$happy_customer)

################################################################################################################
#                                          Part C: Create training and test data sets
################################################################################################################

#create two datasets - one for training, one for testing.
#create random indices data frame from hotelSurvey
random_index <- sample(1:dim(hotelSurvey)[1]) 
summary(random_index)

#Breaking the data set in 2parts- one 2/3(training) and other of 1/3rd(test)
cut_Point <- floor(2 * dim(hotelSurvey)[1]/3) 
cut_Point

# Create training dataset with 2/3rd of hotelSurvey data
training_Data <- hotelSurvey[randIndex[1:cut_Point],]

# Create test_data with 1/3rd of hotelSurvey data 
test_data <- hotelSurvey[randIndex[(cut_Point+1):dim(hotelSurvey)[1]],] 
test_data
#4.	Use the dim( ) function
dim(hotelSurvey)
dim(training_Data)
dim(test_data)

################################################################################################################
#                                          Part D: Build a Model using ksvm( ) 
################################################################################################################

#apply ksvm on hotelClean,hotelFriendly, lengthOfStay withh parameters kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE
svm_output <- ksvm(happy_customer ~ hotelClean+hotelFriendly+lengthOfStay, data=training_Data,kernel ="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
#Cost of the constraint defines the leve of overfitting and underfitting done.
#cross = 3, defines that it is build on 3-fold cross-validation and that the model is not biased towards one model
#kernel = rbfdot, defines that model used radial basis function
#kpar='autmatic', The kernel generating functions are used to initialize a kernel function which calculates the dot (inner) product between two feature vectors in a Hilbert Space.
#echo variable svm_output to the console
print(svm_output)

################################################################################################################
#                     Part E: Predict Values in the Test Data and Create a Confusion Matrix
################################################################################################################

# Create a variable 'svm_prediction' based on number of votes
svm_prediction <- predict(svm_output, test_data, type ="votes") 
#Review the contents using str( ) and head( ).
str(svm_prediction)
head(svm_prediction)

#based on happy_customer and svm_prediction, define a composite table 
composite_table<-data.frame(test_data$happy_customer,svm_prediction[2,]) 

# Create a confusion matrix:con_matrix
con_matrix<-table(composite_table) 
con_matrix 
# Define a DF containing sum of errors:error_sum
error_sum<-con_matrix[1,2]+con_matrix[2,1]
# define percentage of error rate:error_rate
error_rate<-error_sum/sum(con_matrix)*100 
error_rate

################################################################################################################
#                                      # Part F: Find a good prediction
################################################################################################################


#apply ksvm on hotelClean,whenBookedTrip with parameters kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE
svm_output<-ksvm(happy_customer~hotelClean+whenBookedTrip, data = training_Data, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)

# Create a variable 'svm_prediction' based on number of votes
svm_prediction2 <- predict(svm_output, test_data, type = "votes")

#based on happy_customer and svm_prediction, define a composite table 
computation_table<-data.frame(test_data$happy_customer,svm_prediction2[2,])

# Create the confusion matrix
confusion_matrix<-table(computation_table)
confusion_matrix

# Define dataframe that contains sum of errors
error_sum<-confusion_matrix[1,2]+confusion_matrix[2,1]

# Define error rate
error_rate<-error_sum/sum(confusion_matrix)*100
error_rate


#It is important to have a test data set becasue that helps in measuring the reliability of the model just used.
# various parameters makes an overfitting model but when it is checked with the test data set actual reliablity can be checked 









    