
# Airplane Crashes - Data Visualization Project ####
## Importing Libraries ####
library(ggplot2)
library(socviz)
library(tidyverse)
library(dplyr)

## Importing Data ####
MainData <- read_csv("Data/Airplane_Crashes_and_Fatalities_Since_1908_1.csv") # import  data


## Data Preparation ####
MainData$Date<-MainData$Date %>% parse_datetime("%m/%d/%Y")


## Data Description ####
dim(MainData)
summary(MainData)
names(MainData)
str(MainData)

## Insight # 2 Crashes by Month and Hour ####
figu2_data <- data.frame(table(format(MainData$Date,"%m"),
                          as.POSIXlt(MainData$Time,  
                          format ="%H:%M:%OS", 
                          optional = FALSE)$hour))

figu2_data
names(figu2_data)[1] <- "month"
names(figu2_data)[2] <- "hour"
names(figu2_data)[3] <- "count"

fig2<- ggplot(data = figu2_data, mapping = aes(
  y= hour, 
  x= month,
  fill = count))+
  geom_tile() +
  scale_fill_gradient(low="light blue", high="dark red") +
  geom_text(aes(label=count))+
  theme(text = element_text(size = 18, face = "bold") , plot.title = element_text(hjust = 0.5))+
  labs(x="Month", y="Hour", title="Crashes by time by month")
fig2

## Insight # 4 - Wordcloud Crashes summary ####


install.packages("remotes")
remotes::install_github("kwanjeeraw/radiant.wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("tm")
library(tm)
library("NLP")
#Create a vector containing only the text
text <- MainData$Summary

# remove none ascii characters
text <- iconv(text, "latin1", "ASCII", sub="")

# Create a corpus  
docs <- Corpus(VectorSource(text))

dtm <- TermDocumentMatrix(docs,control = list(removePunctuation = TRUE,
                                              stopwords = TRUE)) 
?TermDocumentMatrix
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df = df[-3,]
df = df[-2,]
df = df[-1,]
head(df,30)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=30, random.order=TRUE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


