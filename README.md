---
title: "Web Scraping Tutorial"
author: "Dusty Turner"
date: "April 27, 2018"
output:
  html_document:
    theme: united
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)
```


# Load / Install Packages
```{r}

# install.packages("tidyverse")
library(tidyverse)
# install.packages("rvest")
library(rvest)
```

# Just a refresher on the '%>%' operator

```{r}
multiplyfunction = function(x,y) {
  z=x*y
  return(z)
}

multiplyfunction(3,4)

3 %>% multiplyfunction(4)
```

# Why do this?


```{r}
subtractfunction = function(i,j) {
  k=i-j
  return(k)
}

3 %>% multiplyfunction(4) %>% subtractfunction(2)

subtractfunction(multiplyfunction(3,4),2)
```

It provides the same answer but is more 'readable'.  To quote Hadley Wickham:

"R is optimized for human performance not computer performance."

# Lets Get Scraping

To find the css selector, you'll need to download [Inspector Gadget](http://selectorgadget.com/).  Follow the instructions on the website to select the elements you want to 'scrape' and paste that css selection into the "html_nodes" part of the command below.  

```{r}
url = "http://www.espn.com/mens-college-basketball/bpi/_/view/overview/page/1"

stuff = url %>% read_html() %>% html_nodes(".bpi__table td") %>% html_text()

espnBPI = url %>%
  read_html() %>%
  html_nodes(".bpi__table td") %>%
  html_text() %>%
  matrix(ncol = 8, byrow = TRUE)

as.tibble(espnBPI)
```


# Lets Extend This


```{r}
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

as.tibble(espnBPITotal)
```


# Lets Extend This More

```{r}
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

as.tibble(espnBPITotal)
```


# One More Extention: Ethics

Build in a pause to not frustrate the site owners.


```{r}
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

as.tibble(espnBPITotal)
```

Clean up the output:

```{r}
espnBPITotal = espnBPITotal[-1,]

colnames(espnBPITotal) = c("Rank", "Team", "Conf", "W-L", "BPI", "SOS", "SOR", "RPI")

as.tibble(espnBPITotal)
```

Write the file to a CSV if you like

```{r}
write.csv(espnBPITotal, "ESPNBPI.csv")
```

Filter to look at your data in specific ways:

```{r}
espnBPITotal %>%
  as.tibble() %>%
  filter(Team=="UMBCUMBC"|Team=="VirginiaUVA")

```


##Now for a note about APIs

ESPN Fantasy API
Google Places, Geocoding, Email, Books, Calendar, many others
Yahoo Answers, Flicr, Maps
Yelp
Zillow
Facebook and

TWITTER!!

# Load twitteR Package

**Note, since I gave this presentation, I've found the `rtweet` packages is much better.  

```{r}
# install.packages("twitteR")
library(twitteR)
```

```{r include=FALSE}
# Set API Keys
api_key <- "xxx"
api_secret <- "xxx"
access_token <- "xxx"
access_token_secret <- "xxx"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
```

My API key is hidden for privacy.  You can get this through the [Twitter Developer API]( https://developer.twitter.com/)

```{r echo=TRUE, eval=FALSE}
# Set API Keys
api_key <- "key"
api_secret <- "secret"
access_token <- "token"
access_token_secret <- "token secret"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
```


```{r}  
tweets <- searchTwitter(searchString =  "West Point", 
  n = 100
  # since = since,
  # until = until
  , geocode = '41.3915,-73.956,10mi'
  )
  
cleantweets = tweets %>%
  twListToDF()

```

See All Data

```{r}
as.tibble(cleantweets)
```

When were they created?

```{r}
head(cleantweets$created)
```

What is the text?
```{r}
head(cleantweets$text)
```

