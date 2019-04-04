#Course number IST687
#Name : Latika Wadhwa
#HW 11 -  Wielding the tm package and counting words  
#Due Date :-11/29/2018
#Date the assignment is submitted:- 11/29/2018

#install RJSONIO packages
install.packages("RJSONIO")
install.packages("ggplot2")

#install( ) kernlab R package
install.packages("kernlab") 
install.packages("tm") 
install.packages("wordcloud") 
library(kernlab)
library(RJSONIO)
library(ggplot2)
library(tm)
library(wordcloud)

################################################################################################################
#                                           Part A: Load and condition the data     
################################################################################################################

#	Load the dataset: hotelSurveyBarriot.json 
data<-'hotelSurveySherison.json'

#	Name the dataframe hotelSurvey
hotelSurvey<-fromJSON(data,simplify = TRUE,nullValue = NA)
hotelSurvey<-data.frame(hotelSurvey)

#View the dataframe
View(hotelSurvey)
str(hotelSurvey)
#positive file
pos <- "positive-words.txt"
View(pos)

p <- scan(pos, character(0), sep = "\n")
str(p)
summary(p)
View(p)


#negative file
neg <- "negative-words.txt"
View(neg)

n <- scan(neg, character(0), sep = "\n")
str(n)
summary(n)
View(n)

#clean data
p <- p[-c(1:34)]
n <- n[-c(1:34)]

head(p,50)
head(n,50)


#functions

createWordCounts <- function(vFreeText)
{

  words.vec <- VectorSource(vFreeText)
#Corpus metadata contains corpus specific metadata in form of tag-value pairs. 
  words.corpus <- Corpus(words.vec)
#Convert everything to lower case
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))
  #remove punctuations
    words.corpus <- tm_map(words.corpus, removePunctuation)
  #remove numbers
  words.corpus <- tm_map(words.corpus,removeNumbers)
  #final step transformation:take out the "stop" words, such as "the", "a" and "at"
  words.corpus <- tm_map(words.corpus,removeWords, stopwords("english"))
  #Create a term document matrix
  tdm <- TermDocumentMatrix(words.corpus)
  tdm
  
  m<- as.matrix(tdm)
  wordCounts<-  rowSums(m)
  wordCounts <- sort(wordCounts, decreasing = TRUE)
  return(wordCounts)

}

#Match positive/ negative words with the words in the dataset
getMatched <- function(wordCounts, PosOrNeg )
{

    words <- names(wordCounts)
  matched <-  match(words, PosOrNeg, nomatch=0)
  return(matched)

}

#4 Calculate percent positive words and negative words.

CalcPosNeg <- function(wordCounts, matchedP, matchedN)
{
  
  totalWords <- sum(wordCounts)
  pTotal <- sum(wordCounts[which(matchedP!= 0)])
  pTotal
  pPer <- (pTotal/totalWords)*100
  nTotal <- sum(wordCounts[which(matchedN!= 0)])
  nTotal
  nPer<- (nTotal/totalWords)*100
  retunValue1 <-  c(pPer, nPer)
  return(retunValue1)  
  
}

#generate Word Cloud
genWordCloud <- function(wordCounts)
{

    cloudFrame <- data.frame(word= names(wordCounts), freq = wordCounts)
  wordcloud(names(wordCounts), wordCounts, min.freq = 2, max.words = 30, rat.per=0.35, colors=brewer.pal(8, "Dark2"))
  
}

#generate bar chart

genBarCloud <- function(wordCounts, matched)
{
  
  sortedWords <- sort(wordCounts[matched>1])
  barplot(sortedWords, las=2, cex.names=0.75)
  
}


################################################################################################################
#                                Part B: Create a list of word counts from the speech    
################################################################################################################

#Transform the free text into a term document matrix
wordCounts <- createWordCounts(hotelSurvey$freeText)
View(wordCounts)
#Determine positive and negative word matches.
matchedP <- getMatched(wordCounts, p)
matchedN <- getMatched(wordCounts, n)
View(matchedP)
View(matchedN)
wordCounts

#4.	Calculate the percent positive words and negative words.
ret <- CalcPosNeg(wordCounts, matchedP, matchedN)
cat("num pos words (%)", ret[1], " num neg words (%) ", ret[2], "")#...display

#5.	Write a block comment that summarizes what you learned from ratioPos and ratioNeg.
# The ratioPos and ratioNeg suggests that the document has  more of positive words over negative words
# which in turn suggests that the ratio of happy customers is more than the unhappy one's

################################################################################################################
#                                    Part C: Visualize the results    
################################################################################################################

#6.	Create a word cloud
genWordCloud(wordCounts)

#7.	Create a barplot of the positive and negative words that matched (at least twice)
genBarCloud(wordCounts, matchedP)
genBarCloud(wordCounts, matchedN)

#8.	Write a block comment on what you observe from these two barplots and the wordcloud.
#The gen cloud suggests that room and hotel followed by staff are the most used words in the customers satisfaction 
#The barchart suggests friendly is the word used max number of time in positive barcloud and bad is the word used max number of times in the  negative bar cloud

#9.	Does these results make sense to you in terms of the kinds of emotions you see?
#           Which do you think is more informative - barplot or the wordcloud?
# Yes, they do make sense to me when compared to the emotions in the dataset.
#I find barplot to be a better, because it viisualizes with the exact number of times the word appeared in the dataset

#10.	Create two subset of the text vectors: one for happy customers and one for not happy customers (based on overall customer satisfaction).
hotelSurveylow <- hotelSurvey[ hotelSurvey$overallCustSat < 7,]
hotelSurveyHigh <- hotelSurvey[ hotelSurvey$overallCustSat >= 7,]

################################################################################################################
#                                    Part C: Visualize the results    
################################################################################################################

#11.	Redo Steps B, C & D, for these two subsets of the text strings.
wordCounts <- createWordCounts(hotelSurveylow$freeText)
View(wordCounts)
matchedP <- getMatched(wordCounts, p)
matchedN <- getMatched(wordCounts, n)
View(matchedP)
View(matchedN)
wordCounts
ret <- CalcPosNeg(wordCounts, matchedP, matchedN)
cat("num pos words (%)", ret[1], " num neg words (%)", ret[2], "")#...display
#num pos words (%) 6.102362  num neg words (%) 10.03937 
genWordCloud(wordCounts)
genBarCloud(wordCounts, matchedP)
genBarCloud(wordCounts, matchedN)


wordCounts <- createWordCounts(hotelSurveyHigh$freeText)
View(wordCounts)

matchedP <- getMatched(wordCounts, p)
matchedN <- getMatched(wordCounts, n)
View(matchedP)
View(matchedN)
wordCounts
ret <- CalcPosNeg(wordCounts, matchedP, matchedN)
cat("num pos words (%)", ret[1], " num neg words (%)", ret[2], "")#...display
#num pos words (%) 27.15789  num neg words (%) 1.052632 

genWordCloud(wordCounts)
genBarCloud(wordCounts, matchedP)
genBarCloud(wordCounts, matchedN)


#12.	Compare the positive and negative ratios for these two differ
#   After dividing the dataset in customers with 7 or gretter satisfaction and less than that, the posRation suggests that there were 6% of positive words used in the customers with low satisfaction and 
#10% of negative words used in the same, while in the dataset with the happy customers there appro 27% of positive words and only 1% of negative words used.

