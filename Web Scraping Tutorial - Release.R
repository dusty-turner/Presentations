# install.packages("tidyverse")
library(tidyverse)
# install.packages("rvest")
library(rvest)

#########################################
## Just a refresher on the '%>%' operator
#########################################

multiplyfunction = function(x,y) {
  z=x*y
  return(z)
}

multiplyfunction(3,4)

3 %>% multiplyfunction(4)

## Why do this?

subtractfunction = function(i,j) {
  k=i-j
  return(k)
}

3 %>% multiplyfunction(4) %>% subtractfunction(2)

subtractfunction(multiplyfunction(3,4),2)

####################
## Lets Get Scraping
####################

url = "http://www.espn.com/mens-college-basketball/bpi/_/view/overview/page/1"

stuff = url %>% read_html() %>% html_nodes(".bpi__table td") %>% html_text()

espnBPI = url %>%
  read_html() %>%
  html_nodes(".bpi__table td") %>%
  html_text() %>%
  matrix(ncol = 8, byrow = TRUE)

####################
## Lets Extend This
####################

url = c("http://www.espn.com/mens-college-basketball/bpi/_/view/overview/page/1",
        "http://www.espn.com/mens-college-basketball/bpi/_/view/overview/page/2")

espnBPITotal = NA

for (i in 1:length(url)) {
  espnBPI = url[i] %>%
    read_html() %>%
    html_nodes(".bpi__table td") %>%
    html_text() %>%
    matrix(ncol = 8, byrow = TRUE)
  espnBPITotal = rbind(espnBPITotal,espnBPI)
}

espnBPITotal

########################
## Lets Extend This More
########################


for (i in 1:15) {
  url[i]=paste0("http://www.espn.com/mens-college-basketball/bpi/_/view/overview/page/", i)
}

espnBPITotal = NA

for (i in 1:length(url)) {
  espnBPI = url[i] %>%
    read_html() %>%
    html_nodes(".bpi__table td") %>%
    html_text() %>%
    matrix(ncol = 8, byrow = TRUE)
  espnBPITotal = rbind(espnBPITotal,espnBPI)
}

espnBPITotal

#############################
## One More Extention: Ethics
#############################

for (i in 1:15) {
  url[i]=paste0("http://www.espn.com/mens-college-basketball/bpi/_/view/overview/page/", i)
}

paste0("http://www.espn.com/mens-college-basketball/bpi/_/view/overview/page/", i)

espnBPITotal = NA

for (i in 1:length(url)) {
  espnBPI = url[i] %>%
    read_html() %>%
    html_nodes(".bpi__table td") %>%
    html_text() %>%
    matrix(ncol = 8, byrow = TRUE)
  espnBPITotal = rbind(espnBPITotal,espnBPI)
  Sys.sleep(runif(1,.001,.01))
}

espnBPITotal

espnBPITotal = espnBPITotal[-1,]

colnames(espnBPITotal) = c("Rank", "Team", "Conf", "W-L", "BPI", "SOS", "SOR", "RPI")

espnBPITotal

dim(espnBPITotal)

write.csv(espnBPITotal, "ESPNBPI.csv")

espnBPITotal %>%
  as.data.frame() %>%
  filter(Team=="UMBCUMBC"|Team=="VirginiaUVA")

############################
## Now for a note about APIs
############################

# ESPN Fantasy API
# Google Places, Geocoding, Email, Books, Calendar, many others
# Yahoo Answers, Flicr, Maps
# Yelp
# Zillow
# Facebook and

# TWITTER!!

########################
## ESPN FANTASY FOOTBALL
########################

# install.packages("twitteR")
library(twitteR)

# Must get the keys at the below website
# https://developer.twitter.com/en/docs

# Set API Keys
api_key <- "xxx"
api_secret <- "xxx"
access_token <- "xxx"
access_token_secret <- "xxx"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  
  
tweets <- searchTwitter(searchString =  "West Point", 
  n = 100
  # since = since,
  # until = until
  , geocode = '41.3915,-73.956,10mi'
  )
  
cleantweets = tweets %>%
  twListToDF()

cleantweets

cleantweets$created

names(cleantweets)

cleantweets$text

######################
## Back to Power Point
######################

  