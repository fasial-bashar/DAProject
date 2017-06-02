
##2: Top 4 Predictive analysis: 
#Loading the Required Library 
library("twitteR")
library("plyr")
library("ROAuth")
library("stringr")
library("ggplot2")

#Conttecting to twitter API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

#my API Keys
api_key <- "msjng3EcYPGdL19gt2npzMnZn"
api_secret <- "Kx4DyXylBXnGkdHOJ2Szmm2KDyWYSA6ohVm7MgY2qw49wH7sFt"
access_token <- "313278116-BOQBiDPukATJAjxbWP4BHGGgXAaf3huy1icaxgCP"
access_token_secret <- "eoBSIy91NUVtDLQu0djYZvaPZt9ZiKoKNlfDBFIxMu758"

#setting twitter authentication details
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#Searching for Tweets related to the Keywords. 
ManUtd.list <- searchTwitter('#MANUTD',num= 1000, resultType = 'recent') 
ManUtd.df = twListToDF(ManUtd.list) 
View(ManUtd.df)

Chelsea.list <- searchTwitter('#Chelsea #premierleague',num= 1000, resultType = 'recent') 
Chelsea.df = twListToDF(Chelsea.list) 
View(Chelsea.df)

Liverpool.list <- searchTwitter('#LFC',num= 1000, resultType = 'recent') 
Liverpool.df = twListToDF(Liverpool.list) 
View(Liverpool.df)

Tottenham.list <- searchTwitter('#tottenham',num= 1000, resultType = 'recent') 
Tottenham.df = twListToDF(Tottenham.list) 
View(Tottenham.df)

ManCity.list <- searchTwitter('#mancity',num= 1000, resultType = 'recent') 
ManCity.df = twListToDF(ManCity.list) 
View(ManCity.df)

#Generating  the function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
{ 
  #requires plyr and stringr package
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
    sentence = gsub("[[:digit:]]", " ", sentence)
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
    # TRUE or FALSE will be counted as 1 or 0 by sum() function:  
    score = sum(pos.matches) - sum(neg.matches)  
    return(score)  
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
}

#Load sentiment word lists
pos.words = scan('G:\\Data\\positive-words.txt', what='character', comment.char=';')
neg.words = scan('G:\\Data\\negative-words.txt', what='character', comment.char=';')

#converting the text to factor
#ManUtd.df$text<-as.factor(ManUtd.df$text)
#Chelsea.df$text<-as.factor(Chelsea.df$text)
#Liverpool.df$text<-as.factor(Liverpool.df$text)
#Tottenham.df$text<-as.factor(Tottenham.df$text)
#ManCity.df$text<-as.factor(ManCity.df$text)

#scoring each dataset by using the score.sentiment algorithm 
ManUtd.scores = score.sentiment(ManUtd.df$text, pos.words,neg.words, .progress='text')

Chelsea.scores = score.sentiment(Chelsea.df$text, pos.words,neg.words, .progress='text')

Liverpool.scores = score.sentiment(Liverpool.df$text,pos.words,neg.words, .progress='text')

Tottenham.scores = score.sentiment(Tottenham.df$text,pos.words,neg.words, .progress='text')

ManCity.scores = score.sentiment(ManCity.df$text, pos.words, neg.words, .progress='text')

#Adding extra attribute (Team) to each dataset
ManUtd.scores$Team = 'ManUtd'
Chelsea.scores$Team = 'Chelsea'
Liverpool.scores$Team = 'Liverpool'
Tottenham.scores$Team = 'Tottenham'
ManCity.scores$Team = 'ManCity'

# histogram of Final outputs
hist(ManUtd.scores$score)
hist(Chelsea.scores$score)
hist(Liverpool.scores$score)
hist(Tottenham.scores$score)
hist(ManCity.scores$score)

#creating a table for score
table(ManUtd.scores$score)
table(Chelsea.scores$score)
table(Liverpool.scores$score)
table(Tottenham.scores$score)
table(ManCity.scores$score)


#Combing All the Teams.Score together for the Table using rbind() fucntion 
all.scores = rbind(ManUtd.scores, Chelsea.scores, Liverpool.scores, Tottenham.scores, ManCity.scores)

#creating a table with all the teams and the sentiment score.
table(all.scores$score,all.scores$Team)

#Creating a ggplot based on the score for each team
ggplot(data=all.scores) + geom_histogram(mapping=aes(x=score, fill=Team), binwidth=1) +
  facet_grid(Team~.) +theme_bw() + scale_fill_brewer(palette="Set1") # using "Set1" as colors

