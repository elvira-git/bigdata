# Global setups

library(twitteR)
library(tm)
library(rjson)
library(wordcloud)
library(dplyr)
library(caret)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(syuzhet) # for sentiment analysis
library(scales)
library(rbokeh)
library(base64enc) # fix for twitter oauth in shinyapps.io
library(SnowballC) # fix for stemming issue in tm
library(sqldf)
library(rtweet)#only for streaming
library(magrittr)

runOnline = T


catch.error = function(x){
  
  y = NA
  
  catch_error = tryCatch(tolower(x), error=function(e) e)
  
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  
  return(y)
}

cleanTweets<- function(tweet){
 
  
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  
  
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  
  
  tweet = gsub("#\\w+", " ", tweet)
  
  
  tweet = gsub("@\\w+", " ", tweet)
  
 
  tweet = gsub("[[:punct:]]", " ", tweet)
 
  tweet = gsub("[[:digit:]]", " ", tweet)
  
 
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  tweet = gsub("[ \n]{2,}", " ", tweet)
  
  tweet = catch.error(tweet)
  tweet
}


cleanTweetsAndRemoveNAs   = function(Tweets) {
  
  TweetsCleaned = sapply(Tweets, cleanTweets)

  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  
  names(TweetsCleaned) = NULL
  
TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}


# Load twitter authorization
if(runOnline){
    secrets <- fromJSON(file='twitter_secrets.json.nogit')
    
    setup_twitter_oauth(secrets$api_key,
                        secrets$api_secret,
                        secrets$access_token,
                        secrets$access_token_secret)

    
}


if(runOnline){
appname <- "rtweet_token"

key <- "MyK52CDBJRQi4iTRNCtHI9Ng5"

secret <- "L99HTTIjCbPSQ4dctADu3BqTfPUpQ4u1JqW3VuSnJjxTrPcnqV"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)
}


# Grab tweets
getTweets <- function(searchString, numTweets, date1, date2,adress,radius,language,resultType){
    
    if(runOnline){
        
      
      a=geocode(adress, source="google")
    
      la=as.character(a$lat)
      lo=as.character(a$lon)
      
      x=paste(a$lat,',',a$lon,',',radius,'km')
      x_=gsub(" ", "", x) 
      
      st= searchTwitter(searchString, since=as.character(date1), until= as.character(date2), resultType= resultType ,geocode =x_, lang = language, n = numTweets)
        
        statuses <- data.frame(text=sapply(st, function(x) x$getText()),
                               user=sapply(st, function(x) x$getScreenName()),
                               RT=sapply(st, function(x) x$isRetweet),
                               latitude=sapply(st, function(x) as.numeric(x$latitude[1])),
                               longitude=sapply(st, function(x) as.numeric(x$longitude[1])),
                               time=sapply(st, function(x) format(x$created, format='%F %T'))
        )
    }
    
   
    
    return(statuses)
}

######################### user timeline
# get timeline
getTimeline <- function(searchString2, numTweets2){
  
  if(runOnline){
    
    tw = userTimeline(searchString2, n = numTweets2)
    tw = twListToDF(tw)
  
    statuses2 = data.frame(tw$text,tw$created,tw$favoriteCount,tw$retweetCount)
    colnames(statuses2) <- c("Text","Creation Date","FavoriteCount" ,"RetweetCount")
    
    }
  
  return(statuses2)
}

get_user <- function(searchString2){
  
  if(runOnline){
    
    userTable <- getUser(searchString2)
    
    df_ut=as.data.frame(userTable)
    df_ut_filtered =data.frame (df_ut$name, df_ut$description,df_ut$statusesCount , df_ut$followersCount ,df_ut$favoritesCount ,df_ut$friendsCount ,df_ut$location  )
    colnames(df_ut_filtered) <- c("Name","Description","Number of Tweets" ,"Followers", "Favourites", "Following", "Location" )
    
    df_ut_filtered
     
  }
  
  return(df_ut_filtered)
}





getStream <- function(searchString4,searchString5,seconds){
  
  if(runOnline){
    
    filename <- "test.json"
    
    ## No upfront-parse save as json file instead method
    stream_tweets(q = searchString4, parse = FALSE,
                  timeout = seconds,
                  file_name = filename)
    ## Parse from json file
    rt <- parse_stream(filename)
    
    rt2=rt[, c(1,3,5,10,16)]
    
  }
  
  return(rt2)
}

#######################################
#### Grab text data
getTextData <- function(statuses) {
    
  
  textdata = cleanTweetsAndRemoveNAs(statuses$text)
  

}


########## Grab user timeline data
getTextData2 <- function(statuses2) {
  
    textdata2 = cleanTweetsAndRemoveNAs(statuses2$Text)
}





############## Get sentiment data
getSentiments <- function(textdata){
    sentiments <- sapply(textdata, function(x) get_nrc_sentiment(as.character(x)))
    
    sentiments <- as.data.frame(aperm(sentiments)) # transpose and save as dataframe
    sentiments <- as.data.frame(lapply(sentiments, as.numeric)) 
    sentiments <-
        sentiments %>%
        mutate(positivity = positive - negative)
}


# Get sentiment data for user timeline
getSentiments2 <- function(textdata2){
  sentiments2 <- sapply(textdata2, function(x) get_nrc_sentiment(as.character(x)))
  
  sentiments2 <- as.data.frame(aperm(sentiments2)) # transpose and save as dataframe
  sentiments2 <- as.data.frame(lapply(sentiments2, as.numeric)) 
  sentiments2 <-
    sentiments2 %>%
    mutate(positivity = positive - negative)
}
