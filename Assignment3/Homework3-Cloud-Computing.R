rm(list=ls())
getwd()
setwd("/Users/maryamkhan/ceu-cloud-class/serverless")
library(rvest)
library(xml2)
library(dplyr)
library(data.table)
library(httr)
library("aws.comprehend")
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tidyverse)
library(ggthemes)
library(tm)
install.packages('tm')
install.packages("coolbutuseless/threed")
install.packages("coolbutuseless/ggthreed")
install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))
install.packages("wordcloud2")
install.packages("wordcloud")
install.packages("RColorBrewer")


# *Installation*
#   Latest Stable Version:

# On Mac/Linux:
if (Sys.info()["sysname"] == 'Darwin'){
  Sys.setenv(LDFLAGS="-L/usr/local/opt/openssl@1.1/lib",
             CPPFLAGS="-I/usr/local/opt/openssl@1.1/include",
             PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig",
             LIBRARY_PATH=paste(Sys.getenv("LIBRARY_PATH"),
                                "/usr/local/opt/openssl@1.1/lib",
                                sep=""))
  dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
  install.packages("xml2", configure.vars='INCLUDE_DIR=/usr/local/opt/libxml2/include/libxml2 LIB_DIR=/usr/local/opt/libxml2/lib/')
  install.packages('curl', lib = Sys.getenv("R_LIBS_USER"))
  install.packages('httr')
  install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
} else { # On Windows
  install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"), INSTALL_opts = "--no-multiarch")
  # if not working use:
  # install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
}

# Set up your R w/ AWS

keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
  stop("ERROR: AWS key file not found")
} 
keyTable <- read.csv(keyfile, header = T) # *accessKeys.csv == the CSV downloaded from AWS containing your Access & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)
#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 



# Dawn - Pakistan News
t<- read_html("https://www.dawn.com/news/1649887")
description1 <- t %>% html_nodes('p:nth-child(22) , p:nth-child(8) , p:nth-child(3) , p:nth-child(4) , p:nth-child(9) , p:nth-child(10) , p:nth-child(14) , p:nth-child(15) , p:nth-child(16) , p:nth-child(17) , p:nth-child(18) , p:nth-child(19) , p:nth-child(20) , p:nth-child(21) , .mt-1 p:nth-child(2) , .mt-1 > p:nth-child(1) , .border-b-grey-default .story__link') %>% html_text()

detect_language(description1)
paknews <- detect_sentiment(description1)
paknews 
paknews$Sentiment <- NULL
pak1 <- paknews %>% gather("sentiment", "rating", -1)



# BBC - International News
tt<- read_html("https://www.bbc.com/news/world-asia-58443839")
description2 <- tt %>% html_nodes('#main-heading , .e1xue1i85:nth-child(21) .eq5iqo00 , .e1xue1i85:nth-child(15) .eq5iqo00 , .e1xue1i85:nth-child(17) .eq5iqo00 , .e1xue1i85:nth-child(18) .eq5iqo00 , .e1xue1i85:nth-child(14) .eq5iqo00 , .e1xue1i85:nth-child(12) .eq5iqo00 , #piano-inline1+ .e1xue1i85 .eq5iqo00 , .e1xue1i85:nth-child(9) .eq5iqo00 , .e1xue1i85:nth-child(7) .eq5iqo00 , .e1xue1i85:nth-child(6) .eq5iqo00 , .e1xue1i85:nth-child(4) .eq5iqo00 , .e5tfeyi3') %>% html_text()

detect_language(description2)
internationalnews <- detect_sentiment(description2)
internationalnews
internationalnews$Sentiment <- NULL
int1 <- internationalnews %>% gather("sentiment", "rating", -1)



## Bar chart for sentiment analysis
#PAK
ggplot(pak1, aes(x=Index, y= rating, fill=sentiment)) +
  geom_bar(stat = "identity", position="fill")+
  theme_stata()
#INT
ggplot(int1, aes(x=Index, y= rating, fill=sentiment)) +
  geom_bar(stat = "identity", position="fill")+
  theme_stata()

## Word cloud
#PAK
docs1 <- Corpus(VectorSource(paste(description1, collapse = '')))
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- d[-c(7, 12, 13, 15, 17, 19),]
d
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=120, random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#INT

docs <- Corpus(VectorSource(paste(description2, collapse = '')))
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- d[-c(1,2,5,6,7,9,11,12,15,18,19,20),]
d
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=180, random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



#Imran Khan speech on taliban
pakspeech<- read_html("https://www.aljazeera.com/news/2021/9/22/pakistan-imran-khan-civil-war-afghanistan-taliban")
description3 <- pakspeech %>% html_nodes('p:nth-child(31) , #aljazeera_incontent_dynamic1+ p , p:nth-child(2)') %>% html_text()
pakspeech_sum <- paste(description3, collapse = "")
detect_language(description3)
pakspeech_sentiment <- detect_sentiment(pakspeech_sum)
pakspeech_sentiment
pakspeech_sentiment$Sentiment <- NULL
pss <- pakspeech_sentiment %>% gather("sentiment", "rating", -1)

#Taliban spokeperson speech
talibanspeech<- read_html("https://www.business-standard.com/article/international/pakistan-is-like-a-second-home-for-the-taliban-says-zabihullah-mujahid-121082700086_1.html")
description4 <- talibanspeech %>% html_nodes('p:nth-child(8) , .p-content p:nth-child(3)') %>% html_text()
talibanspeech_sum <- paste(description4, collapse = "")
detect_language(description4)
talibanspeech_sentiment <- detect_sentiment(talibanspeech_sum)
talibanspeech_sentiment
talibanspeech_sentiment$Sentiment <- NULL
tss <- talibanspeech_sentiment %>% gather("sentiment", "rating", -1)

#Pie Chart
#Pakistan statement
library(ggplot2)
pss
pss$rating1 <- round(pss$rating *100,3)
ggplot(pss, aes(x = "", y = rating, fill = sentiment)) +
  geom_col(color = "black") +
  # geom_text(aes(label = rating1),
  #           position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_update()

#taliban statement
library(ggplot2)
tss
tss$rating1 <- round(tss$rating *100,3)
ggplot(tss, aes(x = "", y = rating, fill = sentiment)) +
  geom_col(color = "black") +
  geom_text(aes(label = ""),
          position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_update()
