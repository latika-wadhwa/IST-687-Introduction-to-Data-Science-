#IST 687
#Latika Mahesh Wadhwa
#Homework 2
#Assignment Due Date - September 12, 2018
#Submission Date - September 07,2018

#                  Step A:Initialize an 'arrests' dataframe

#Copy USArrests dataset:arrests
arrests <- USArrests

#View arrests
View(arrests)                              #using just 'arrests' will print the dataset in the console

#View the structure of the dataset
str(arrests)

#View the summary of the dataset 
summary(arrests)

#                 Step B: Explore the assault rate

#Write a comment: Is a higher or lower assault rate best?
#lower
arrests[which.min(arrests$Assault),]

#higher
arrests[which.max(arrests$Assault),]

#                 Step C: Explore the murder rate  
#Find the state with highest murder rate
max.murder.index <- which.max(arrests$Murder)
max.murder.index    #10
rownames(arrests[max.murder.index,])

#Create a dataframe sorted descendingly wrt murder rate:sortedMurder
sortedMurder <- arrests[order(-arrests$Murder),]


#Display top 10 states with highest murder
head(sortedMurder,10)

#Display the value of the 20th row, third column in sortedMurder
sortedMurder[20,3]

#                 Step D: Which state is the least safe? Explain your logic

#calculate Weighted average of arrests dataframe :weight.Arrests
weight.Arrests <- (arrests$Murder*0.4)+(arrests$Rape*0.4)+(arrests$Assault*.2)+arrests$UrbanPop*0.1
weight.Arrests

#Find the observation with highest weight in weight.Arrests
highest_weight <- which.max(weight.Arrests)
highest_weight
arrests[highest_weight,]


#Logic :- 
#Each crime is weighed according to the subjetive level of importance,
#Murder and Rape being the hightest followed by assault and then urban population.
#Summing up all the values leads to Florida being the most unsafe state to live in.

