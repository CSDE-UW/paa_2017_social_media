### PAA 2017
### Workshop on Social Media for Demographic Research  
### April 2017
### Zagheni-Polimis-Fiorio
### Collect and Analyze Tweets
### Part 2 Collect Tweets using the streaming API and get descriptive statistics
### This part relies on packages and some functions written by Pablo Barbera

#check the directory where you are  
getwd()
## If you are not in the the folder "Collect_Tweets", move to that folder
## using
## setwd("YOUR_PATH_TO_THE_FOLDER")

##Install the packages below and their dependecies (if you have not installed them yet)
## e.g. 
##install.packages(streamR, dependencies=TRUE) 
library(streamR)
library(ROAuth)
source('functions_by_pablobarbera.R')


load("credentials/twitCred.Rdata")


####
## COLLECTING SAMPLES OF TWEETS 
####

## Get a random sample of tweets that are being posted
sampleStream("data/tweets_random.json", timeout = 60, 
             oauth = twitCred)

## Read the json file and parse it into a data frame
tweets <- parseTweets("data/tweets_random.json", verbose = TRUE)
## you may see a warning related to tweets that have been deleted

names(tweets)
head(tweets)

##language of tweets
table(tweets$lang)


##Which of these tweets was sent by the person with more followers?
tweets$followers_count
max(tweets$followers_count)

## "which" would give us the index for the tweet that satisfies our request
which(tweets$followers_count == max(tweets$followers_count))

##Text of the tweet from the person with the highest number of followers
tweets$text[which(tweets$followers_count == max(tweets$followers_count))]

##name of the user who posted the tweet
tweets$screen_name[which(tweets$followers_count == max(tweets$followers_count))]
## when the tweet was posted
tweets$created_at[which(tweets$followers_count == max(tweets$followers_count))]


## find the most common hashtags 
getCommonHashtags(tweets$text, n=50)

####
## See package documentation here: https://cran.r-project.org/web/packages/streamR/streamR.pdf
###


## As an example, imagine we want to collect tweets posted in the area of Chicago.
## The way to do it is to find two pairs of coordinates (longitude and latitude)
## that indicate the southwest corner AND the northeast corner of the rectangle/box that
## we are considering.
## (NOTE THE REVERSE ORDER, IT'S NOT LAT, LONG BUT LONG, LAT)
## In the case of Chicago, it would be approx. (-88.31,41.36) and (-87.13,42.31)
## How to find the coordinates? you can use: http://itouchmap.com/latlong.html
## information about the API parameters can be found here:
## https://dev.twitter.com/streaming/overview/request-parameters

filterStream(file.name="data/tweets_chicago.json", locations=c(-88.31,41.36,-87.13,42.31), 
             timeout=60, oauth=twitCred)

tweets_chicago<- parseTweets("data/tweets_chicago.json", verbose = TRUE)

head(tweets_chicago)


#Tweets from Mexico or in Spanish
filterStream(file.name="data/tweets_mexico_spanish.json", locations=c(-117.6,13.1,-85.26,33.74), 
             timeout=60, language="es", oauth=twitCred)

tweets_mexico_spanish<- parseTweets("data/tweets_mexico_spanish.json", verbose = TRUE)


#Tweets that match a predefined keyword
filterStream(file.name="data/tweets_demog.json", track = c("fertility","mortality","migration","population","demography"), 
             timeout=20, oauth=twitCred)

tweets_demog <- parseTweets("data/tweets_demog.json", verbose = TRUE)



#####
## COLLECTING A RECENT SET OF TWEETS FROM A USER
#####


getTimeline(filename="data/tweets_Bil.json", screen_name="FCBillari", n=1000, oauth_folder="./credentials")
tweets_Bil <- parseTweets("data/tweets_Bil.json")

names(tweets_Bil)
head(tweets_Bil)


## obtain a list of followers for a given user
getFollowers(screen_name = "FCBillari",oauth_folder="./credentials")


getTimeline(filename="data/example_timeline_follower.json", id="235225127", n=1000, oauth_folder="./credentials")
tweets_timeline_follower <- parseTweets("data/example_timeline_follower.json")

# get a list of accounts that the user is following
getFriends(screen_name = "ezagheni", oauth_folder="./credentials")


# to convert from IDs to usernames and viceversa, see: 
# https://tweeterid.com/








