#4TH YEAR DATA ANALYTICS PROJECT--EPL ANALYSIS
#Sentiment Analysis
#FASIAL BASHAR - X13358851-NATIONAL COLLEGE OF IRELAND

#seting the Working Directory 
setwd("G:\\Data")

#Install packages
install.packages("gsubfn")
install.packages("plyr")
install.packages("dplyr")
install.packages("sentimentr")
install.packages("ggplot")
install.packages("wordcloud")
install.packages("stringr")
install.packages("sentiment")
install.packages("Rstem")
install.packages("tm")
install.packages("RColorBrewer")
#loading packages
library("gsubfn")
library("plyr")
library("dplyr")
library("sentimentr")
library("ggplot2")
library("wordcloud")
library("stringr")
library("sentiment")
library("Rstem")
library("tm")
library("RColorBrewer")
library("gridExtra")


#Loading the positive-words and negative-words dictionary (will be used for all the Dataset)
hu.liu.pos = scan('G:\\Data\\positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('G:\\Data\\negative-words.txt', what='character', comment.char=';')

##########################################################################################################
#       Loading each of the datasets that conatints gathered tweets - 5 Dataset will be used             #                                                             #
##########################################################################################################

#DATASET:1 : Manchester City vs Liverpool Match (1-1)
MCILIV <- read.csv(file = "MCILIV.csv", header = TRUE, sep = ",")

#Cleaning the Datset using gsub() function
MCILIV$Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", MCILIV$Text)  # First we will remove retweet entities from
MCILIV$Text = gsub("@\\w+", " ", MCILIV$Text)  # Then remove all "@people"
MCILIV$Text = gsub("http\\w+", "", MCILIV$Text) # removing all the html-links
MCILIV$Text = gsub("[[:punct:]]", " ", MCILIV$Text)  # removing all the punctuations
MCILIV$Text = gsub("[[:digit:]]", " ", MCILIV$Text)  # removing any numbers, only text can analysed 
MCILIV$Text = gsub("[ \t]{2,}", " ", MCILIV$Text)  # removing any unwanted spaces
MCILIV$Text = gsub("^\\s+|\\s+$", " ", MCILIV$Text)

#Removing the Duplicate data/tweets
#Unique_MCILIV <- unique(MCILIV)
#write.csv(Unique_MCILIV, file='G:\\Data\\Unique_MCILIV.csv', row.names = F)

#loading the Unique Dataset
#Unique_MCILIV <- read.csv("G:\\Data\\Unique_MCILIV.csv")

# take a random sample of 1000 from a dataset
#MCILIV.Sample <- Unique_MCILIV[sample(nrow(Unique_MCILIV), 1000),]
#write.csv(MCILIV.Sample, file='G:\\Data\\MCILIV_Sample.csv', row.names = F)

#Reading the 1000 Sample tweets
MCILIV.Sample <- read.csv(file = "MCILIV_Sample.csv", header = TRUE, sep = ",")



# selectig the Text/tweet only from the Dataset
MCILIV_Text = MCILIV.Sample$Text

#score.sentiment Algorithm:
#creating a score.sentiment function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
{  
  require(plyr)  
  require(stringr)       
  # we got a vector of sentences. plyr will handle a list  
  # or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    # cleaning up the sentences using gsub():  
    sentence = gsub('[[:punct:]]', '', sentence)  
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    sentence = gsub('\\d+', '', sentence)  
    # converting the tweets to lower case:  
    sentence = tolower(sentence)  
    #str_split used for to split the sentences into words 
    word.list = str_split(sentence, '\\s+')  
    # sometimes a list() is one level of hierarchy too much  
    words = unlist(word.list)  
    # comparing the words from the tweets with the positive & negative word dictionaries
    # match() returns the position of the matched words or NA  
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    # we are just looking for TRUE/FALSE:  
    pos.matches = !is.na(pos.matches)  
    neg.matches = !is.na(neg.matches)  
    # TRUE or FALSE will be treated as 1 or 0 by sum():  
    score = sum(pos.matches) - sum(neg.matches)  
    return(score)  
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
}

#using the score.sentiment to score the tweets
MCILIV_scores = score.sentiment(MCILIV_Text, hu.liu.pos, hu.liu.neg, .progress='text')
#saving the scored tweets to csv file 
write.csv(MCILIV_scores, file='G:\\Data\\MCILIV_scores.csv', row.names=F)


sc1<- qplot(factor(score), data=MCILIV_scores, geom="bar", 
      fill=factor(score))+xlab("Sentiment_Scores") + ylab("Tweet_Count") + ggtitle("MCLIV Sentiment Scores")
grid.arrange(sc1, nrow=1)

######

#Naive Bayes Algorithms

# create missing value
try.error = function(x)
{
  
  y = NA #Creating a missing value
  try_error = tryCatch(tolower(x), error=function(e)e) #tryCatch to handle errors 
  if(!inherits(try_error, "error")) #If there are no error
    y=tolower(x)
  return(y) # then the result is fine
}

#using the try.error to convert MCILIV_Text to lowercase
MCILIV_Text = sapply(MCILIV_Text, try.error)

# removing all the NAs in Text data if there are any
MCILIV_Text = MCILIV_Text[!is.na(MCILIV_Text)]
names(MCILIV_Text) = NULL


#classify_emotion using the Naive_Bayes Algorithm
class_emo = classify_emotion(MCILIV_Text, algorithm="bayes", prior=1.0)
#get the emotions that gives the best result
emotion = class_emo[,7]
# Replacing all the NA's with "unknown"
emotion[is.na(emotion)] = "unknown"

#saving the Emotion analysis in a CSV file
write.csv(class_emo, file='G:\\Data\\MCILIV_Emo.csv', row.names=F)

# classify_polarity using the Naive_Bayes Algorithm
class_pol = classify_polarity(MCILIV_Text, algorithm="bayes")
# get the polarity that gives best result
polarity = class_pol[,4]

#saving the polarity Analysis in a CSV file
write.csv(class_pol, file='G:\\Data\\MCILIV_Pol.csv', row.names=F)


# Creating a data.frame with results from emotions and polarity
MCILIV_df = data.frame(text=MCILIV_Text, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sorting the data.frame(df)
MCILIV_df = within(MCILIV_df, 
  emotion <- factor(emotion, levels=names(sort(table(emotion),decreasing=TRUE))))

#write csv of results
write.csv(MCILIV_df, file='G:\\Data\\MCILIV_Emo+Pol.csv', row.names=F)

#Vewi the Data.frame
#View(MCILIV_df)

# plot distribution of emotions_categories
em1 <- ggplot(MCILIV_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion))+
  xlab("Emo_Cat") + ylab("Number of Tweets")+
  ggtitle("MCILIV Tweets by Emotion")

grid.arrange(em1, nrow=1)

# plot distribution of Polarity_categories
po1<- ggplot(MCILIV_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity))+
  xlab("Polarity_Cate") + ylab("Number of Tweets")+
  ggtitle("MCILIV Tweets by Polarity")

grid.arrange(po1, nrow=1)


##############################################################################################
#DATASET:2: West Brom Albion vs Arsenal Match (3-1)
WBAARS <- read.csv(file = "WBAARS.csv", header = TRUE, sep = ",")


#Cleaning the Datset using gsub() function
WBAARS$Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", WBAARS$Text)  # First we will remove retweet entities from
WBAARS$Text = gsub("@\\w+", " ", WBAARS$Text)  # Then remove all "@people"
WBAARS$Text = gsub("http\\w+", "", WBAARS$Text) # removing all the html-links
WBAARS$Text = gsub("[[:punct:]]", " ", WBAARS$Text)  # removing all the punctuations
WBAARS$Text = gsub("[[:digit:]]", " ", WBAARS$Text)  # removing any numbers, only text can analysed 
WBAARS$Text = gsub("[ \t]{2,}", " ", WBAARS$Text)  # removing any unwanted spaces
WBAARS$Text = gsub("^\\s+|\\s+$", " ", WBAARS$Text)



#Removing the Duplicate data/tweets
Unique_WBAARS <- unique(WBAARS)
write.csv(Unique_WBAARS, file='G:\\Data\\Unique_WBAARS.csv', row.names = F)

#loading the Unique Dataset
Unique_WBAARS <- read.csv("G:\\Data\\Unique_WBAARS.csv")

# take a random sample of 1000 from a dataset
#WBAARS.Sample <- Unique_WBAARS[sample(nrow(Unique_WBAARS), 1000),]
#write.csv(WBAARS.Sample, file='G:\\Data\\WBAARS_Sample.csv', row.names = F)

#Reading the 1000 Sample tweets
WBAARS.Sample <- read.csv(file = "WBAARS_Sample.csv", header = TRUE, sep = ",")



# selectig the Text/tweet only from the Dataset
WBAARS_Text = WBAARS.Sample$Text

#score.sentiment Algorithm:
#creating a score.sentiment function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
{  
  require(plyr)  
  require(stringr)       
  # we got a vector of sentences. plyr will handle a list  
  # or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    # cleaning up the sentences using gsub():  
    sentence = gsub('[[:punct:]]', '', sentence)  
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    sentence = gsub('\\d+', '', sentence)  
    # converting the tweets to lower case:  
    sentence = tolower(sentence)  
    #str_split used for to split the sentences into words 
    word.list = str_split(sentence, '\\s+')  
    # sometimes a list() is one level of hierarchy too much  
    words = unlist(word.list)  
    # comparing the words from the tweets with the positive & negative word dictionaries
    # match() returns the position of the matched words or NA  
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    # we are just looking for TRUE/FALSE:  
    pos.matches = !is.na(pos.matches)  
    neg.matches = !is.na(neg.matches)  
    # TRUE or FALSE will be treated as 1 or 0 by sum():  
    score = sum(pos.matches) - sum(neg.matches)  
    return(score)  
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
}

#using the score.sentiment to score the tweets
WBAARS_scores = score.sentiment(WBAARS_Text, hu.liu.pos, hu.liu.neg, .progress='text')
#saving the scored tweets to csv file 
write.csv(WBAARS_scores, file='G:\\Data\\WBAARS_scores.csv', row.names=F)


qplot(factor(score), data=WBAARS_scores, geom="bar", 
      fill=factor(score))+xlab("Sentiment_Scores") + ylab("Tweet_Count") + ggtitle("WBAARS Sentiment Scores")

######

#Naive Bayes Algorithm
# create missing value
try.error = function(x)
{
  
  y = NA #Creating a missing value
  try_error = tryCatch(tolower(x), error=function(e)e) #tryCatch to handle errors 
  if(!inherits(try_error, "error")) #If there are no error
    y=tolower(x)
  return(y) # then the result is fine
}

#using the try.error to convert WBAARS_Text to lowercase
WBAARS_Text = sapply(WBAARS_Text, try.error)

# removing all the NAs in Text data if there are any
WBAARS_Text = WBAARS_Text[!is.na(WBAARS_Text)]
names(WBAARS_Text) = NULL


#classify_emotion using the Naive_Bayes Algorithm
class_emo = classify_emotion(WBAARS_Text, algorithm="bayes", prior=1.0)
#get the emotions that gives the best result
emotion = class_emo[,7]
# Replacing all the NA's with "unknown"
emotion[is.na(emotion)] = "unknown"

#saving the Emotion analysis in a CSV file
write.csv(class_emo, file='G:\\Data\\WBAARS_Emo.csv', row.names=F)

# classify_polarity using the Naive_Bayes Algorithm
class_pol = classify_polarity(WBAARS_Text, algorithm="bayes")
# get the polarity that gives best result
polarity = class_pol[,4]

#saving the polarity Analysis in a CSV file
write.csv(class_pol, file='G:\\Data\\WBAARS_Pol.csv', row.names=F)


# Creating a data.frame with results from emotions and polarity
WBAARS_df = data.frame(text=WBAARS_Text, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)

# sorting the data.frame(df)
WBAARS_df = within(WBAARS_df, 
                   emotion <- factor(emotion, levels=names(sort(table(emotion),decreasing=TRUE))))

#write csv of results
write.csv(WBAARS_df, file='G:\\Data\\WBAARS_Emo+Pol.csv', row.names=F)

#Vewi the Data.frame
#View(WBAARS_df)


# plot distribution of emotions_categories
em2 <- ggplot(WBAARS_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion))+
  xlab("Emo_Cat") + ylab("Number of Tweets")+
  ggtitle("WBAARS Tweets by Emotion")

grid.arrange(em2, nrow=1)

# plot distribution of Polarity_categories
po2<- ggplot(WBAARS_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity))+
  xlab("Polarity_Cate") + ylab("Number of Tweets")+
  ggtitle("WBAARS Tweets by Polarity")

grid.arrange(po2, nrow=1)


##############################################################################################
#DATASET:3: Chelsea vs Crystal Palace Match (1-2)
CHECRY <- read.csv(file = "CHECRY.csv", header = TRUE, sep = ",")


#Cleaning the Datset using gsub() function
CHECRY$Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", CHECRY$Text)  # First we will remove retweet entities from
CHECRY$Text = gsub("@\\w+", " ", CHECRY$Text)  # Then remove all "@people"
CHECRY$Text = gsub("http\\w+", "", CHECRY$Text) # removing all the html-links
CHECRY$Text = gsub("[[:punct:]]", " ", CHECRY$Text)  # removing all the punctuations
CHECRY$Text = gsub("[[:digit:]]", " ", CHECRY$Text)  # removing any numbers, only text can analysed 
CHECRY$Text = gsub("[ \t]{2,}", " ", CHECRY$Text)  # removing any unwanted spaces
CHECRY$Text = gsub("^\\s+|\\s+$", " ", CHECRY$Text)



#Removing the Duplicate data/tweets
Unique_CHECRY <- unique(CHECRY)
write.csv(Unique_CHECRY, file='G:\\Data\\Unique_CHECRY.csv', row.names = F)

#loading the Unique Dataset
Unique_CHECRY <- read.csv("G:\\Data\\Unique_CHECRY.csv")

# take a random sample of 1000 from a dataset
#CHECRY.Sample <- Unique_CHECRY[sample(nrow(Unique_CHECRY), 1000),]
#write.csv(CHECRY.Sample, file='G:\\Data\\CHECRY_Sample.csv', row.names = F)

#Reading the 1000 Sample tweets
CHECRY.Sample <- read.csv(file = "CHECRY_Sample.csv", header = TRUE, sep = ",")



# selectig the Text/tweet only from the Dataset
CHECRY_Text = CHECRY.Sample$Text


#score.sentiment Algorithm:
#creating a score.sentiment function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
{  
  require(plyr)  
  require(stringr)       
  # we got a vector of sentences. plyr will handle a list  
  # or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    # cleaning up the sentences using gsub():  
    sentence = gsub('[[:punct:]]', '', sentence)  
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    sentence = gsub('\\d+', '', sentence)  
    # converting the tweets to lower case:  
    sentence = tolower(sentence)  
    #str_split used for to split the sentences into words 
    word.list = str_split(sentence, '\\s+')  
    # sometimes a list() is one level of hierarchy too much  
    words = unlist(word.list)  
    # comparing the words from the tweets with the positive & negative word dictionaries
    # match() returns the position of the matched words or NA  
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    # we are just looking for TRUE/FALSE:  
    pos.matches = !is.na(pos.matches)  
    neg.matches = !is.na(neg.matches)  
    # TRUE or FALSE will be treated as 1 or 0 by sum():  
    score = sum(pos.matches) - sum(neg.matches)  
    return(score)  
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
}

#using the score.sentiment to score the tweets
CHECRY_scores = score.sentiment(CHECRY_Text, hu.liu.pos, hu.liu.neg, .progress='text')
#saving the scored tweets to csv file 
write.csv(CHECRY_scores, file='G:\\Data\\CHECRY_scores.csv', row.names=F)


qplot(factor(score), data=CHECRY_scores, geom="bar", 
      fill=factor(score))+xlab("Sentiment_Scores") + ylab("Tweet_Count") + ggtitle("CHECRY Sentiment Scores")

######

#Naive Bayes Algorithm
# create missing value
try.error = function(x)
{
  
  y = NA #Creating a missing value
  try_error = tryCatch(tolower(x), error=function(e)e) #tryCatch to handle errors 
  if(!inherits(try_error, "error")) #If there are no error
    y=tolower(x)
  return(y) # then the result is fine
}

#using the try.error to convert CHECRY_Text to lowercase
CHECRY_Text = sapply(CHECRY_Text, try.error)

# removing all the NAs in Text data if there are any
CHECRY_Text = CHECRY_Text[!is.na(CHECRY_Text)]
names(CHECRY_Text) = NULL


#classify_emotion using the Naive_Bayes Algorithm
class_emo = classify_emotion(CHECRY_Text, algorithm="bayes", prior=1.0)
#get the emotions that gives the best result
emotion = class_emo[,7]
# Replacing all the NA's with "unknown"
emotion[is.na(emotion)] = "unknown"

#saving the Emotion analysis in a CSV file
write.csv(class_emo, file='G:\\Data\\CHECRY_Emo.csv', row.names=F)

# classify_polarity using the Naive_Bayes Algorithm
class_pol = classify_polarity(CHECRY_Text, algorithm="bayes")
# get the polarity that gives best result
polarity = class_pol[,4]

#saving the polarity Analysis in a CSV file
write.csv(class_pol, file='G:\\Data\\CHECRY_Pol.csv', row.names=F)


# Creating a data.frame with results from emotions and polarity
CHECRY_df = data.frame(text=CHECRY_Text, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)

# sorting the data.frame(df)
CHECRY_df = within(CHECRY_df, 
                   emotion <- factor(emotion, levels=names(sort(table(emotion),decreasing=TRUE))))

#write csv of results
write.csv(CHECRY_df, file='G:\\Data\\CHECRY_Emo+Pol.csv', row.names=F)

#Vewi the Data.frame
#View(CHECRY_df)


# plot distribution of emotions_categories
em3 <- ggplot(CHECRY_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion))+
  xlab("Emo_Cat") + ylab("Number of Tweets")+
  ggtitle("CHECRY Tweets by Emotion")

grid.arrange(em3, nrow=1)

# plot distribution of Polarity_categories
po3<- ggplot(CHECRY_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity))+
  xlab("Polarity_Cate") + ylab("Number of Tweets")+
  ggtitle("CHECRY Tweets by Polarity")

grid.arrange(po3, nrow=1)


##############################################################################################

#Comparing the histograms of Emotion
grid.arrange(em1, em2, em3, ncol=3 )
#Comparing the histograms of Polarity
grid.arrange(po1, po2, po3, ncol=3)
##############################################################################################

