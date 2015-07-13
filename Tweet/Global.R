# global for JAEG Tweet

# ----- Need More Emelie Solution! JAEG -----
# To Do:
# 1 - Find more secure way of twitter authentication
# 2 - Add opiton to change party in common and dif word clouds
# 3 - Optimize code for speed
# 4 - Scale the profile image
# 5 - Add sentiment analysis
# 6 - Better treemap
# 7 - Clean up the environment 
# 8 - Tidy the code

# Issue:
# 1 - Sometime the authorization doesn't work, don't know why
# 2 - Retweets introduce errors

# ----- Load Packages ------

library(curl)
library(plyr)
library(dplyr) # load dplyr last
library(DT)
library(geosphere)
library(ggplot2)
library(ggthemes)
library(httr)
library(leaflet)
library(lubridate)
library(maps)
library(RCurl)
library(rmarkdown)
library(markdown)
library(reshape2)
library(ROAuth)
library(scales)
library(shiny)
library(shinydashboard)
library(sortableR)
library(tidyr)
library(tm)
library(treemapify)
library(twitteR)
library(wordcloud)

# Need to fix authentication
# http://blog.dataweave.in/post/96618078833/building-a-twitter-sentiment-analysis-app-using-r
# https://dipanjan.shinyapps.io/twitter-analysis/
# https://github.com/dipanjanS/MyShinyApps/tree/master/twitter-analysis

# Cool twitter map stuff
# source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")
# twitterMap("simplystats", plotType = "both")

# Connect to Twitter
# Need another more secure way to authenticate to work on shiny
source("www/twitterconnect.R")

# ---- Create warpper functions ----

# Set up repeatble wordcloud function
# not using atm
wordcloud_rep <- repeatable(wordcloud)

# Clean tweet
clean_tweet <- function(tweet) {
  # Ensure it is a dataframe
  if (!is.data.frame(tweet)) stop("input must be a dataframe")
  
  # This line fix some but not all encoding issues
  # Use this on mac
  # tweet$text <- iconv(tweet$text,to="utf-8-mac")
  # Use this on shinyapps
  tweet$text <- iconv(tweet$text, 'UTF-8', 'ASCII')
  
  # Clean up punctuation marks
  tweet$text <- gsub("&amp\\;", "&", tweet$text)
  tweet$text <- gsub(",", "", tweet$text)
  tweet$text <- gsub("\\.", "", tweet$text)
  
  # Convert into corpus
  tweet_text_corpus <- Corpus(DataframeSource(tweet["text"]))
  
  # More clean up
  tweet_text_corpus <- tm_map(tweet_text_corpus, stripWhitespace)
  tweet_text_corpus <- tm_map(tweet_text_corpus, removeWords, stopwords("english"))
  tweet_text_corpus <<- tm_map(tweet_text_corpus, removeWords, stopwords("french"))
  
  # Assign object to global env
  assign(paste0(unique(tweet$screenName), "_text_corpus"), tweet_text_corpus, envir = globalenv())
}

# Create new variables for user calender plotting
create_cal2_df <- function(df) {
  # Create the various time variables
  main_df <- mutate(df, date = floor_date(created, "day"),
                    year = year(created),
                    month = month(created, label = TRUE),
                    mday = mday(created),
                    wday = wday(created, label=TRUE),
                    hour = hour(created),
                    m_first_wday = wday(ymd(paste(year,month,1,sep='-'))),
                    m_week = factor(floor((mday + m_first_wday - 2) / 7) +1, levels = c(1:6))
  )
  
  assign("user_df", main_df, envir = globalenv())
  
  main_day_df <- main_df %>% 
    mutate(date = floor_date(df$created, "day")) %>%
    group_by(date) %>%
    dplyr::summarize(tweetCount = sum(tweetCount),
                     retweetCount = sum(retweetCount),
                     favoriteCount = sum(favoriteCount)) %>%
    mutate(year = year(date),
           month = month(date, label = TRUE),
           mday = mday(date),
           wday = wday(date, label=TRUE),
           m_first_wday = wday(ymd(paste(year,month,1,sep='-'))),
           m_week = factor(floor((mday + m_first_wday - 2) / 7) +1, levels=c(1:6)))
  
  assign("user_day_df", main_day_df, envir = globalenv())
  
  # Create shadow dataframe for empties
  date.start <- min(main_df$date)
  date.end <- max(main_df$date)
  shadow_df <- data.frame(mydate = seq(as.Date(date.start),
                                       as.Date(date.end),
                                       by = 'day'))
  shadow_df <- mutate(shadow_df,
                      year = year(mydate),
                      month = month(mydate, label = TRUE),
                      mday = mday(mydate),
                      wday = wday(mydate, label = TRUE),
                      hour = hour(mydate),
                      m_first_wday = wday(ymd(paste(year,month,1,sep='-'))),
                      m_week = factor(floor((mday + m_first_wday - 2) / 7) +1, levels = c(1:6)),
                      count = 1
  )
  
  assign("user_shadow_df", shadow_df, envir = globalenv())
}

# Create new variables for political party calender plotting
create_cal_df <- function(df) {
  # Create the various time variables
  main_df <- mutate(df, date = floor_date(created, "day"),
         year = year(created),
         month = month(created, label = TRUE),
         mday = mday(created),
         wday = wday(created, label=TRUE),
         hour = hour(created),
         m_first_wday = wday(ymd(paste(year,month,1,sep='-'))),
         m_week = factor(floor((mday + m_first_wday - 2) / 7) +1, levels = c(1:6))
  )
  
  assign(paste0(unique(main_df$screenName), "_df"), main_df, envir = globalenv())
  
  main_day_df <- main_df %>% 
    mutate(date = floor_date(df$created, "day")) %>%
    group_by(date) %>%
    dplyr::summarize(tweetCount = sum(tweetCount),
                     retweetCount = sum(retweetCount),
                     favoriteCount = sum(favoriteCount)) %>%
    mutate(year = year(date),
          month = month(date, label = TRUE),
          mday = mday(date),
          wday = wday(date, label=TRUE),
          m_first_wday = wday(ymd(paste(year,month,1,sep='-'))),
          m_week = factor(floor((mday + m_first_wday - 2) / 7) +1, levels = c(1:6)))
  
  assign(paste0(unique(main_df$screenName), "_day_df"), main_day_df, envir = globalenv())

  # Create shadow dataframe for empties
  date.start <- min(main_df$date)
  date.end <- max(main_df$date)
  shadow_df <- data.frame(mydate = seq(as.Date(date.start),
                                  as.Date(date.end),
                                  by = 'day'))
  shadow_df <- mutate(shadow_df,
    year = year(mydate),
    month = month(mydate, label = TRUE),
    mday = mday(mydate),
    wday = wday(mydate, label = TRUE),
    hour = hour(mydate),
    m_first_wday = wday(ymd(paste(year,month,1,sep='-'))),
    m_week = factor(floor((mday + m_first_wday - 2) / 7) +1, levels=c(1:6)),
    count = 1
  )
  
  assign(paste0(unique(main_df$screenName), "_shadow_df"), shadow_df, envir = globalenv())
}

# Get any user tweets
get_user_tweet <- function(user = "taylorswift13", num_t = 10, rt = FALSE) {
  
  # Get user profile
  tweet_profile <- as.data.frame(getUser(user))
  assign("user_profile", tweet_profile,  envir = globalenv())
  
  # Get tweet
  tweet_text <- userTimeline(user, n = num_t, includeRts = rt)
  tweet_df <- twListToDF(tweet_text) # convert to dataframe
  tweet_df$tweetCount <- 1
  clean_tweet(tweet_df) # clean up the tweet using the wrapper function
  assign("text_corpus", tweet_text_corpus, envir = globalenv())
  assign(paste0(user,"_df"), tweet_df, envir = globalenv())
  
  # Return them as list
  list(tweet_text = tweet_text_corpus,
       tweet_df = tweet_df,
       user_profile = tweet_profile)
}  

# Get tweets from the five Canadian federal political party leaders
get_cdnpoli_tweet <- function(num_twt = 100, r_twt = FALSE) {
  
  # Get user info from major political party leader
  con_user <- as.data.frame(getUser("pmharper"))
  lib_user <- as.data.frame(getUser("JustinTrudeau"))
  ndp_user <- as.data.frame(getUser("ThomasMulcair"))
  green_user <- as.data.frame(getUser("ElizabethMay"))
  bloc_user <- as.data.frame(getUser("GillesDuceppe"))
  
  cdn_party_user_profile <- rbind(con_user, lib_user, ndp_user, green_user, bloc_user)
  
  assign("cdn_party_user_profile", cdn_party_user_profile, envir = globalenv())

  # Get tweets and retweets from major political party leader
  con <- userTimeline("pmharper", num_twt, includeRts = r_twt)
  lib <- userTimeline("JustinTrudeau", num_twt, includeRts = r_twt)
  ndp <- userTimeline("ThomasMulcair", num_twt, includeRts = r_twt)
  green <- userTimeline("ElizabethMay", num_twt, includeRts = r_twt)
  bloc <- userTimeline("GillesDuceppe", num_twt, includeRts = r_twt)
  
  # Create dataframe for other purposes
  con_df <- twListToDF(con)
  lib_df <- twListToDF(lib)
  ndp_df <- twListToDF(ndp)
  green_df <- twListToDF(green)
  bloc_df <- twListToDF(bloc)
  
  # Create a dummy count
  con_df$tweetCount <- 1
  lib_df$tweetCount <- 1
  ndp_df$tweetCount <- 1
  green_df$tweetCount <- 1
  bloc_df$tweetCount <- 1
  
  # Create individual cleaned corpous for simple cloud
  clean_tweet(con_df)
  clean_tweet(lib_df)
  clean_tweet(ndp_df)
  clean_tweet(green_df)
  clean_tweet(bloc_df)
  
  assign("con_df", con_df, envir = globalenv())
  assign("lib_df", lib_df, envir = globalenv())
  assign("ndp_df", ndp_df, envir = globalenv())
  assign("green_df", green_df, envir = globalenv())
  assign("bloc_df", bloc_df, envir = globalenv())
  
  # Create combined corpus for comparison and common cloud
  cdn_party_df <- rbind(con_df, lib_df, ndp_df, green_df, bloc_df)
  
  # Create combined cloud
  # May run into encoding issue with retweet
  
  # Extract the text
  con_t <- sapply(con, function(x) x$getText())
  lib_t <- sapply(lib, function(x) x$getText())
  ndp_t <- sapply(ndp, function(x) x$getText())
  green_t <- sapply(green, function(x) x$getText())
  bloc_t <- sapply(bloc, function(x) x$getText())
  
  # Collapse and combine them
  con_t <- paste(con_t, collapse = " ")
  lib_t <- paste(lib_t, collapse = " ")
  ndp_t <- paste(ndp_t, collapse = " ")
  green_t <- paste(green_t, collapse = " ")
  bloc_t <- paste(bloc_t, collapse = " ")
  cdn_party_text <- c(con_t, lib_t, ndp_t, green_t, bloc_t)
  
  # Remove common words in English and French
  cdn_party_text <- removeWords(cdn_party_text, c(stopwords("english"), stopwords("french")))
  
  # Replace the Ampersand symbol
  cdn_party_text <-  gsub("&amp\\;", "&", cdn_party_text)
  
  # Remove comma and period
  cdn_party_text <-  gsub(",", "", cdn_party_text)
  cdn_party_text <-  gsub("\\.", "", cdn_party_text)
  
  # Create corpus from vector source
  cdn_party_corpus <- Corpus(VectorSource(cdn_party_text))
  
  # Trim whitespace
  cdn_party_corpus <- tm_map(cdn_party_corpus, stripWhitespace)
  
  # Create term-document matrix
  cdn_party_tdm <- TermDocumentMatrix(cdn_party_corpus)
  
  # Convert as matrix
  cdn_party_tdm <- as.matrix(cdn_party_tdm)
  
  # Add column names
  colnames(cdn_party_tdm) <- c("Conservative", "Liberal", "NDP", "Green", "Bloc Québécois")
  
  # Assign to global env
  assign("cdn_party_tdm", cdn_party_tdm, envir = globalenv())
  
  list(corpous_for_cloud = cdn_party_tdm,
       green_df = green_df,
       lib_df = lib_df,
       con_df = con_df,
       ndp_df = ndp_df,
       bloc_df = bloc_df,
       cdn_party_user_profile = cdn_party_user_profile)
}

# Fn modified from https://github.com/wilkox/treemapify
ggtify <- function (treeMap, label.groups = TRUE) {
  if (missing(treeMap) || is.data.frame(treeMap) == FALSE) {
    stop("Must provide a data frame")
  }
  xlim <- c(min(treeMap["xmin"]), max(treeMap["xmax"]))
  ylim <- c(min(treeMap["ymin"]), max(treeMap["ymax"]))
  Plot <- ggplot(treeMap)
  Plot <- Plot + coord_cartesian(xlim = xlim, ylim = ylim)
  Plot <- Plot + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                               ymax = ymax, fill = fill))
  Plot <- Plot + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                               ymax = ymax), fill = NA, colour = "grey", size = 0.2)
  Plot <- Plot + theme(axis.ticks = element_blank(), axis.title = element_blank(), 
                       axis.text = element_blank())
  Plot <- Plot + guides(fill = guide_legend(title = attributes(treeMap)$fillName))
  if ("group" %in% colnames(treeMap)) {
    groupRects <- ddply(treeMap, .(group), plyr::summarise, xmin <- min(xmin), 
                        xmax <- max(xmax), ymin <- min(ymin), ymax <- max(ymax))
    names(groupRects) <- c("group", "xmin", "xmax", "ymin", 
                           "ymax")
    Plot <- Plot + geom_rect(data = groupRects, mapping = aes(xmin = xmin, 
                                                              xmax = xmax, ymin = ymin, ymax = ymax), colour = "grey", 
                             fill = NA, size = 1.2)
    Plot <- Plot + theme(panel.border = element_rect(size = 2, 
                                                     fill = NA, colour = "grey"))
  }
  if (label.groups == TRUE && "group" %in% colnames(treeMap)) {
    if ("label" %in% colnames(treeMap)) {
      groupLabels <- ddply(treeMap, c("group"), plyr::summarise, 
                           x <- max(xmax) - ((max(xmax) - min(xmin)) * 0.5), 
                           y <- min(ymin) + 2, size <- (max(xmax) - min(xmin))/nchar(as.character(group[1])))
    }
    else {
      groupLabels <- ddply(treeMap, c("group"), plyr::summarise, 
                           x <- max(xmax) - ((max(xmax) - min(xmin)) * 0.5), 
                           y <- max(ymax) - ((max(ymax) - min(ymin)) * 0.5), 
                           size <- (max(xmax) - min(xmin))/(nchar(as.character(group[1]))))
    }
    names(groupLabels) <- c("group", "x", "y", "size")
    Plot <- Plot + annotate("text", x = groupLabels$x, y = groupLabels$y, 
                            label = groupLabels$group, size = groupLabels$size, 
                            colour = "darkgrey", fontface = "bold", hjust = 0.5, 
                            vjust = 0)
  }
  if ("label" %in% colnames(treeMap)) {
    treeMap <- ddply(treeMap, "label", plyr::mutate, labelx = xmin + 
                       1, labely = ymax - 1, labelsize = 12
    )
    Plot <- Plot + geom_text(data = treeMap, aes(label = label, 
                                                 x = labelx, y = labely, size = labelsize), hjust = 0, 
                             vjust = 1, colour = "black")
    Plot <- Plot + scale_size(range = c(1, 8), guide = FALSE)
  }
  return(Plot)
}

# Fn to get locatin of twitter followers
# Adapted from http://simplystatistics.org/2011/12/21/an-r-function-to-map-your-twitter-followers/
twitterMap <- function(userName, nMax = 1000) {
  
  tmp = getUser(userName)
  
  # Use Vancouver as user location to by-pass issue
  # Could use any location since we are not drawing the great circle paths
  userLocation <- "Vancouver, Canada"
  
  if(is.null(userLocation)){
    userLocation = location(tmp)
    userLocation = trim(userLocation)
    if(nchar(userLocation) < 2){stop("We can not find your location from Twitter")}
  }
  
  # Get followers and following data
  followers=tmp$getFollowers(n=nMax)
  followersLocation = sapply(followers,function(x){location(x)})
  followersScreeName = sapply(followers,function(x){screenName(x)})
  followers_df <- data.frame(followersLocation, followersScreeName)
  
  # Lets skip following for now
#  following = tmp$getFriends(n=nMax)
#   followingLocation = sapply(following,function(x){location(x)})
#   followingScreeName = sapply(following,function(x){screenName(x)})
#   following_df <- data.frame(followingLocation, followingScreeName)
  
  # Load the geographic data
  data(world.cities)
  data(us.cities)
  data(canada.cities)
  
  # Find the latitude and longitude of the user
  cat("Getting geographic (latitude/longitude) of Twitter users.\n")
  userLL <- findLatLon(userLocation)$latlon
  if(any(is.na(userLL))){stop("We can't find the latitude and longitude of your location from Twitter")}
  
  # Find the latitude and longitude of each of the followers/following
  # and calcualte the distance to the user
  
  followersLL = matrix(NA,nrow=length(followers),ncol=4)
  # followingLL = matrix(NA,nrow=length(following),ncol=4)
  
  ## Try pre-clean the extra whitespace in the middle, don't seem to matter
  followersLocation <- sapply(followersLocation, function(x) gsub("\\s{2,}", " ", x))
#   followingLocation <- sapply(followingLocation, function(x) gsub("\\s{2,}", " ", x))
#   
  # Geocode via lookup
  for(i in 1:length(followers)){
    if(length(followersLocation[[i]]) > 0){
      tmpLL = findLatLon(trim(followersLocation[[i]]))
      if(any(!is.na(tmpLL$latlon))){
        followersLL[i,] = c(unlist(tmpLL$latlon),distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
      }
    }
  }
  
  # Geocode via lookup
#   for(i in 1:length(following)){
#     if(length(followingLocation[[i]]) > 0){
#       tmpLL = findLatLon(trim(followingLocation[[i]]))
#       if(any(!is.na(tmpLL$latlon))){
#         followingLL[i,] =  c(unlist(tmpLL$latlon),distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
#       }
#     }
#   }
  
  # Combine them before getting rid of it
  followers_coded_df <- data.frame(followersLL, followers_df)
  names(followers_coded_df) <- c("long", "lat", "somenumber", "anothernumber", "location", "Handle")
  followers_coded_df <- filter(followers_coded_df, !is.na(long))
  
#   following_coded_df <- data.frame(followingLL, following_df)
#   names(following_coded_df) <- c("long", "lat", "somenumber", "anothernumber", "location", "Handle")
#   following_coded_df <- filter(following_coded_df, !is.na(long))
#   
  assign(paste0("user_f_coded_df"), followers_coded_df, envir = globalenv())
#   assign(paste0("user_ff_coded_df"), following_coded_df, envir = globalenv())
#   
  #   followingLL = followingLL[order(-followingLL[,3]),]
  #   followersLL = followersLL[order(-followersLL[,3]),]
  #   
  #   followingLL = followingLL[!is.na(followingLL[,1]),]
  #   followersLL = followersLL[!is.na(followersLL[,1]),]
  
  followers_coded_df
}

# fn to lookup lat and lon
findLatLon <- function(loc){
  latlon = NA
  cont = NA
  
  # Asia = 1, Africa = 2, North America = 3, South America = 4, Australia/New Zealand = 5, Europe = 6
  continents = matrix(NA,nrow=length(unique(world.cities[,2])),ncol=2)
  continents[,1] = unique(world.cities[,2])
  continents[1:10,2] = c(1,1,1,2,1,1,1,1,1,1)
  continents[11:20,2]= c(1,1,2,1,1,2,1,2,2,2)
  continents[21:30,2] = c(2,1,6,6,6,6,6,6,6,6)
  continents[31:40,2] = c(6,6,6,6,2,4,4,1,2,1)
  continents[41:50,2] = c(4,6,1,4,6,1,3,1,6,6)
  continents[51:60,2] = c(3,2,4,2,6,1,6,1,3,2)
  continents[61:70,2] = c(1,2,2,2,3,6,3,3,6,6)
  continents[71:80,2] = c(1,1,2,6,3,4,3,4,6,1)
  continents[81:90,2] = c(3,3,3,2,2,6,6,6,6,4)
  continents[91:100,2] = c(2,5,2,2,3,1,1,1,1,1)
  continents[101:110,2] = c(1,2,1,1,1,3,2,5,1,6)
  continents[111:120,2] = c(1,6,1,1,2,6,1,1,6,2)
  continents[121:130,2] = c(6,6,6,1,1,3,4,3,4,2)
  continents[131:140,2] = c(6,6,2,2,1,1,1,4,1,1)
  continents[141:150,2] = c(1,2,2,1,1,1,4,6,6,2)
  continents[151:160,2] = c(4,1,1,1,1,2,4,6,2,2)
  continents[161:170,2] = c(1,2,2,1,6,2,1,1,6,1)
  continents[171:180,2] = c(1,1,1,2,6,2,2,6,1,1)
  continents[181:190,2] = c(2,6,2,1,6,6,3,3,3,3)
  continents[191:200,2] = c(2,2,2,2,3,2,3,2,3,1)
  continents[201:210,2] = c(3,2,2,2,2,2,2,1,6,2)
  continents[211:220,2] = c(1,3,1,6,2,4,3,6,3,4)
  continents[221:230,2] = c(1,1,1,3,2,3,3,6,1,6)
  continents[231:232,2] = c(2,1)
  
  
  # Get the first element of the location
  # The first line is already commented out, not sure why
  # firstElement = strsplit(loc,"[^[:alnum:]]")[[1]][1]
  firstElement = strsplit(loc,",")[[1]][1]
  if(is.na(firstElement)){firstElement="zzzzzzzzz"}
  
  # See if it is a city
  tmp = grep(firstElement,world.cities[,1],fixed=TRUE)
  tmp2 = grep(firstElement,state.name,fixed=TRUE)
  tmp3 = grep(firstElement,world.cities[,2],fixed=TRUE)
  
  if(length(tmp) == 1){
    latlon = world.cities[tmp,c(5,4)]
    cont = continents[which(world.cities[tmp,2]==continents[,1]),2]
  }else if(length(tmp) > 1){
    tmpCities = world.cities[tmp,]
    latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
    cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
  }else if(length(tmp2) == 1){
    latlon = c(state.center$x[tmp2],state.center$y[tmp2])
    cont = 3
  }else if(length(tmp3) > 0){
    tmpCities = world.cities[tmp3,]
    latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
    cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
  }
  
  return(list(latlon=latlon,cont=as.numeric(cont)))
  
}

# Find great circle path, not using atm
getGreatCircle = function(userLL,relationLL){
  tmpCircle = greatCircle(userLL,relationLL)
  start = which.min(abs(tmpCircle[,1] - userLL[1,1]))
  end = which.min(abs(tmpCircle[,1] - relationLL[1]))
  greatC = tmpCircle[start:end,]
  return(greatC)
}

# Special trim fn
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Other way to authenticate twitteR doesn't work though

# library(twitteR)
# library(ROAuth)
# library(RCurl)
# 
# download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

# requestURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
# consumerKey <- ""
# consumerSecret <- ""
# 
# twitCred <- OAuthFactory$new(consumerKey=consumerKey,
#                              consumerSecret=consumerSecret,
#                              requestURL=requestURL,
#                              accessURL=accessURL,
#                              authURL=authURL)
# 
# twitCred$handshake(cainfo="cacert.pem")
# 
# registerTwitterOAuth(twitCred)
# Error in registerTwitterOAuth(twitCred) : 
#   ROAuth is no longer used in favor of httr, please see ?setup_twitter_oauth

# save(list="twitCred", file="twitteR_credentials")

# load("twitteR_credentials")
# registerTwitterOAuth(twitCred)
