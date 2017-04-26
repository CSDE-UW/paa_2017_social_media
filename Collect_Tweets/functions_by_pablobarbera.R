
##from this repository:
## https://github.com/SMAPPNYU/smappR/tree/master/R


## function to find the N most common hashtags in a string vector
getCommonHashtags <- function(text, n=20){

    hashtags <- regmatches(text,gregexpr("#(\\d|\\w)+",text))

    hashtags <- unlist(hashtags)

    tab <- table(hashtags)

    return(head(sort(tab, dec=TRUE), n=n))

}




## function to obatin the timeline of a user
# getTimeline <- function(file.name, screen_name=NULL, n=1000, oauth){
# 
#     require(rjson); require(ROAuth)
# 
#     ## url to call
#     url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
# 
#     ## first API call
#     params <- list(screen_name = screen_name, count=200, trim_user="false")
#    
#     url.data <- oauth$OAuthRequest(URL=url, params=params, method="GET", 
#     cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
# 
#     ## trying to parse JSON data
#     json.data <- fromJSON(url.data, unexpected.escape = "skip")
#     if (length(json.data$error)!=0){
#         cat(url.data)
#         stop("error parsing tweets!")
#     }
#     ## writing to disk
#     conn <- file(file.name, "w")
#     invisible(lapply(json.data, function(x) writeLines(toJSON(x), con=conn)))
#     close(conn)
#     ## max_id
#     tweets <- length(json.data)
#     max_id <- json.data[[tweets]]$id_str
#     max_id_old <- "none"
#     cat(tweets, "tweets.\n")
# 
#     while (tweets < n & max_id != max_id_old){
#         max_id_old <- max_id
#         params <- list(screen_name = screen_name, count=200, max_id=max_id,
#                 trim_user="false")
#         url.data <- oauth$OAuthRequest(URL=url, params=params, method="GET", 
#         cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
#         ## trying to parse JSON data
#         json.data <- fromJSON(url.data, unexpected.escape = "skip")
#         if (length(json.data$error)!=0){
#             cat(url.data)
#             stop("error! Last cursor: ", cursor)
#         }
#         ## writing to disk
#         conn <- file(file.name, "a")
#         invisible(lapply(json.data, function(x) writeLines(toJSON(x), con=conn)))
#         close(conn)
#         ## max_id
#         tweets <- tweets + length(json.data)
#         max_id <- json.data[[length(json.data)]]$id_str
#         cat(tweets, "tweets.\n")
#     }
# }


getTimeline <- function(filename, n=3200, oauth_folder="~/credentials", screen_name=NULL, 
                        id=NULL, since_id=NULL, trim_user="true", sleep=.5, verbose=FALSE){
  
  ## create list of credentials
  creds <- list.files(oauth_folder, full.names=T)
  ## open a random credential
  cr <- sample(creds, 1)
  if (verbose) message(cr)
  load(cr)
  ## while rate limit is 0, open a new one
  limit <- getLimitTimeline(twitCred)
  if (verbose) message(limit, " hits left")
  while (limit==0){
    cr <- sample(creds, 1)
    if (verbose) message(cr)
    load(cr)
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(twitCred)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitTimeline(twitCred)
    if (verbose) message(limit, " hits left")
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
  
  ## first API call
  if (!is.null(screen_name)){
    params <- list(screen_name = screen_name, count=200, trim_user=trim_user)
  }
  if (!is.null(id)){
    params <- list(id=id, count=200, trim_user=trim_user)   
  }
  if (!is.null(since_id)){
    params[["since_id"]] <- since_id
  }
  query <- lapply(params, function(x) URLencode(as.character(x)))
  
  # preparing OAuth token for httr
  options("httr_oauth_cache"=FALSE)
  app <- httr::oauth_app("twitter", key = twitCred$consumerKey, 
                         secret = twitCred$consumerSecret)
  credentials <- list(oauth_token = twitCred$oauthKey, oauth_token_secret = twitCred$oauthSecret)
  twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
                                      app = app, credentials = credentials)
  
  # first query
  url.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
  Sys.sleep(sleep)
  ## one API call less
  limit <- limit - 1
  ## changing oauth token if we hit the limit
  if (verbose) message(limit, " hits left")
  cr_old <- cr
  while (limit==0){
    cr <- sample(creds, 1)
    if (verbose) message(cr)
    load(cr)
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(twitCred)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitTimeline(twitCred)
    if (verbose) message(limit, " hits left")
  }
  if (cr != cr_old) {
    app <- httr::oauth_app("twitter", key = twitCred$consumerKey, 
                           secret = twitCred$consumerSecret)
    credentials <- list(oauth_token = twitCred$oauthKey, oauth_token_secret = twitCred$oauthSecret)
    twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
                                        app = app, credentials = credentials)
  }
  ## trying to parse JSON data
  ## json.data <- fromJSON(url.data, unexpected.escape = "skip")
  json.data <- httr::content(url.data)
  if (length(json.data$error)!=0){
    message(url.data)
    stop("error! Last cursor: ", cursor)
  }
  ## writing to disk
  conn <- file(filename, "a")
  ret <- lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn))
  close(conn)
  ## max_id
  tweets <- length(json.data)
  max_id <- json.data[[tweets]]$id_str
  message(tweets, " tweets. Max id: ", max_id)
  max_id_old <- "none"
  if (is.null(since_id)) {since_id <- 1}
  
  while (tweets < n & max_id != max_id_old & 
         as.numeric(max_id) > as.numeric(since_id)){
    max_id_old <- max_id
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, count=200, max_id=max_id,
                     trim_user=trim_user)
    }
    if (!is.null(id)){
      params <- list(id=id, count=200, max_id=max_id, trim_user=trim_user)
    }
    if (!is.null(since_id) && since_id != 1 ){
      params[['since_id']] <- since_id
    }
    query <- lapply(params, function(x) URLencode(as.character(x)))
    url.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
    Sys.sleep(sleep)
    ## one API call less
    limit <- limit - 1
    ## changing oauth token if we hit the limit
    message(limit, " hits left")
    cr_old <- cr
    while (limit==0){
      cr <- sample(creds, 1)
      message(cr)
      load(cr)
      Sys.sleep(sleep)
      # sleep for 5 minutes if limit rate is less than 100
      rate.limit <- getLimitRate(twitCred)
      if (rate.limit<100){
        Sys.sleep(300)
      }
      limit <- getLimitTimeline(twitCred)
      message(limit, " hits left")
    }
    if (cr != cr_old) {
      app <- httr::oauth_app("twitter", key = twitCred$consumerKey, 
                             secret = twitCred$consumerSecret)
      credentials <- list(oauth_token = twitCred$oauthKey, oauth_token_secret = twitCred$oauthSecret)
      twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
                                          app = app, credentials = credentials)
    }
    ## trying to parse JSON data
    ## json.data <- fromJSON(url.data, unexpected.escape = "skip")
    json.data <- httr::content(url.data)
    if (length(json.data$error)!=0){
      message(url.data)
      stop("error! Last cursor: ", cursor)
    }
    ## writing to disk
    conn <- file(filename, "a")
    ret <- lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn))
    close(conn)
    ## max_id
    tweets <- tweets + length(json.data)
    max_id <- json.data[[length(json.data)]]$id_str
    message(tweets, " tweets. Max id: ", max_id)
  }
}


getLimitTimeline <- function(twitCred){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "statuses,application")
  response <- twitCred$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(rjson::fromJSON(response)$resources$statuses$`/statuses/user_timeline`[['remaining']]))
  
}








getFollowers <- function(screen_name=NULL, oauth_folder, cursor=-1, user_id=NULL, verbose=TRUE, sleep=1){
  
  ## create list of credentials
  creds <- list.files(oauth_folder, full.names=T)
  ## open a random credential
  cr <- sample(creds, 1)
  if (verbose) {message(cr)}
  load(cr)
  ## while rate limit is 0, open a new one
  limit <- getLimitFollowers(twitCred)
  if (verbose) {message(limit, " API calls left")}
  while (limit==0){
    cr <- sample(creds, 1)
    if (verbose){message(cr)}
    load(cr)
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(twitCred)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitFollowers(twitCred)
    if (verbose){message(limit, " API calls left")}
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/followers/ids.json"
  ## empty list for followers
  followers <- c()
  ## while there's more data to download...
  while (cursor!=0){
    ## making API call
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, cursor = cursor, stringify_ids="true")
    }
    if (!is.null(user_id)){
      params <- list(user_id = user_id, cursor = cursor, stringify_ids="true")
    }
    url.data <- twitCred$OAuthRequest(URL=url, params=params, method="GET", 
                                      cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    Sys.sleep(sleep)
    ## one API call less
    limit <- limit - 1
    ## trying to parse JSON data
    json.data <- rjson::fromJSON(url.data)
    if (length(json.data$error)!=0){
      if(verbose){message(url.data)}
      stop("error! Last cursor: ", cursor)
    }
    ## adding new IDS
    followers <- c(followers, as.character(json.data$ids))
    
    ## previous cursor
    prev_cursor <- json.data$previous_cursor_str
    ## next cursor
    cursor <- json.data$next_cursor_str
    ## giving info
    message(length(followers), " followers. Next cursor: ", cursor)
    
    ## changing oauth token if we hit the limit
    if (verbose){message(limit, " API calls left")}
    while (limit==0){
      cr <- sample(creds, 1)
      if (verbose){message(cr)}
      load(cr)
      Sys.sleep(sleep)
      # sleep for 5 minutes if limit rate is less than 100
      rate.limit <- getLimitRate(twitCred)
      if (rate.limit<100){
        Sys.sleep(300)
      }
      limit <- getLimitFollowers(twitCred)
      if (verbose){message(limit, " API calls left")}
    }
  }
  return(followers)
}

getLimitFollowers <- function(twitCred){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- twitCred$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(rjson::fromJSON(response)$resources$followers$`/followers/ids`[['remaining']]))
}





getFriends <- function(screen_name=NULL, oauth_folder, cursor=-1, user_id=NULL, verbose=TRUE, sleep=1){
  
  ## create list of credentials
  creds <- list.files(oauth_folder, full.names=T)
  ## open a random credential
  cr <- sample(creds, 1)
  if (verbose){message(cr)}
  load(cr)
  ## while rate limit is 0, open a new one
  limit <- getLimitFriends(twitCred)
  if (verbose){message(limit, " API calls left")}
  while (limit==0){
    cr <- sample(creds, 1)
    if (verbose){message(cr)}
    load(cr)
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(twitCred)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitFriends(twitCred)
    if (verbose){message(limit, " API calls left")}
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/friends/ids.json"
  ## empty list for friends
  friends <- c()
  ## while there's more data to download...
  while (cursor!=0){
    ## making API call
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, cursor = cursor, stringify_ids="true")
    }
    if (!is.null(user_id)){
      params <- list(user_id = user_id, cursor = cursor, stringify_ids="true")
    }
    url.data <- twitCred$OAuthRequest(URL=url, params=params, method="GET", 
                                      cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    Sys.sleep(sleep)
    ## one API call less
    limit <- limit - 1
    ## trying to parse JSON data
    json.data <- rjson::fromJSON(url.data, unexpected.escape = "skip")
    if (length(json.data$error)!=0){
      if (verbose){message(url.data)}
      stop("error! Last cursor: ", cursor)
    }
    ## adding new IDS
    friends <- c(friends, as.character(json.data$ids))
    
    ## previous cursor
    prev_cursor <- json.data$previous_cursor_str
    ## next cursor
    cursor <- json.data$next_cursor_str
    ## giving info
    message(length(friends), " friends. Next cursor: ", cursor)
    
    ## changing oauth token if we hit the limit
    if (verbose){message(limit, " API calls left")}
    while (limit==0){
      cr <- sample(creds, 1)
      if (verbose){message(cr)}
      load(cr)
      Sys.sleep(sleep)
      # sleep for 5 minutes if limit rate is less than 100
      rate.limit <- getLimitRate(twitCred)
      if (rate.limit<100){
        Sys.sleep(300)
      }
      limit <- getLimitFriends(twitCred)
      if (verbose){message(limit, " API calls left")}
    }
  }
  return(friends)
}

getLimitFriends <- function(twitCred){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "friends,application")
  response <- twitCred$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(rjson::fromJSON(response)$resources$friends$`/friends/ids`['remaining']))
}

getLimitRate <- function(twitCred){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- twitCred$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(rjson::fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}

#Updated parseTweets => parseTweets2, readTweets => readTweets2 functions
#Adjusts for encoding problems
#Includes unlistWithNA dependency
#Lee Fiorio - 2017.04.24 - fiorio@uw.edu

readTweets2 <- function(tweets, verbose=TRUE){
  ## checking input is correct
  if (is.null(tweets)){
    stop("Error: you need to specify file or object where tweets text was stored.")
  }
  
  ## Read the text file and save it in memory as a list           
  if (length(tweets)==1 && file.exists(tweets)){
    lines <- readLines(tweets)
  }       
  else {
    lines <- tweets
  }
  ## Converting to UTF-8
  ## Encoding lines added by Lee Fiorio fiorio@uw.edu 2017-04-24
  
  if (Sys.info()["sysname"]=="Windows"){
    lines <- iconv(lines, "UTF-8", "Latin1", sub="")     
  }
  else {
    lines <- iconv(lines, "ASCII", "UTF-8", sub="")
  }
  
  results.list <- lapply(lines[nchar(lines)>0], function(x) tryCatch(fromJSON(x), error=function(e) e))
  
  ## check if JSON file is coming from search endpoint instead of API
  search <- 'search_metadata' %in% names(results.list[[1]])
  if (search) results.list <- results.list[[1]]$statuses
  
  ## removing lines that do not contain tweets or were not properly parsed
  #errors <- which(unlist(lapply(results.list, length))<18)
  if (!search){
    errors <- which(unlist(lapply(results.list, function(x) 'id' %in% names(x) == FALSE)))
    if (length(errors)>0){
      results.list <- results.list[-errors]
    }        
  }
  
  # information message
  if (verbose==TRUE) message(length(results.list), " tweets have been parsed.")
  return(results.list)
}

parseTweets2 <- function (tweets, simplify = FALSE, verbose = TRUE) 
{
  results.list <- readTweets2(tweets, verbose = FALSE)
  if (length(results.list) == 0) {
    stop(deparse(substitute(tweets)), " did not contain any tweets. ", 
         "See ?parseTweets for more details.")
  }
  df <- data.frame(text = unlistWithNA(results.list, "text"), 
                   retweet_count = unlistWithNA(results.list, "retweet_count"), 
                   favorite_count = unlistWithNA(results.list, "favorite_count"), 
                   favorited = unlistWithNA(results.list, "favorited"), 
                   truncated = unlistWithNA(results.list, "truncated"), 
                   id_str = unlistWithNA(results.list, "id_str"), in_reply_to_screen_name = unlistWithNA(results.list, 
                                                                                                         "in_reply_to_screen_name"), source = unlistWithNA(results.list, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        c("user", "screen_name")), stringsAsFactors = F)
  if (simplify == FALSE) {
    df$country_code <- unlistWithNA(results.list, c("place", 
                                                    "country_code"))
    df$country <- unlistWithNA(results.list, c("place", "country"))
    df$place_type <- unlistWithNA(results.list, c("place", 
                                                  "place_type"))
    df$full_name <- unlistWithNA(results.list, c("place", 
                                                 "full_name"))
    df$place_name <- unlistWithNA(results.list, c("place", 
                                                  "name"))
    df$place_id <- unlistWithNA(results.list, c("place", 
                                                "id"))
    place_lat_1 <- unlistWithNA(results.list, c("place", 
                                                "bounding_box", "coordinates", 1, 1, 2))
    place_lat_2 <- unlistWithNA(results.list, c("place", 
                                                "bounding_box", "coordinates", 1, 2, 2))
    df$place_lat <- sapply(1:length(results.list), function(x) mean(c(place_lat_1[x], 
                                                                      place_lat_2[x]), na.rm = TRUE))
    place_lon_1 <- unlistWithNA(results.list, c("place", 
                                                "bounding_box", "coordinates", 1, 1, 1))
    place_lon_2 <- unlistWithNA(results.list, c("place", 
                                                "bounding_box", "coordinates", 1, 3, 1))
    df$place_lon <- sapply(1:length(results.list), function(x) mean(c(place_lon_1[x], 
                                                                      place_lon_2[x]), na.rm = TRUE))
    df$lat <- unlistWithNA(results.list, c("geo", "coordinates", 
                                           1))
    df$lon <- unlistWithNA(results.list, c("geo", "coordinates", 
                                           2))
    df$expanded_url <- unlistWithNA(results.list, c("entities", 
                                                    "urls", 1, "expanded_url"))
    df$url <- unlistWithNA(results.list, c("entities", "urls", 
                                           1, "url"))
  }
  if (verbose == TRUE) 
    message(length(df$text), " tweets have been parsed.")
  return(df)
}

unlistWithNA <- function(lst, field){
  if (length(field)==1){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], '[[', field))
  }
  if (length(field)==2){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]]))
  }
  if (length(field)==3 & field[1]!="geo"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  if (field[1]=="geo"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]]))
  }
  
  if (length(field)==4 && field[2]!="urls"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]][[field[3]]][[field[4]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]][[field[4]]]))
  }
  if (length(field)==4 && field[2]=="urls"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
  }
  if (length(field)==6 && field[2]=="bounding_box"){
    notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) 
      x[[field[1]]][[field[2]]][[field[3]]][[as.numeric(field[4])]][[as.numeric(field[5])]][[as.numeric(field[6])]]))
  }
  return(vect)
}
