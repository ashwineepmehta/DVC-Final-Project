# Airplane Crashes - Data Visualization Project #####
# Importing Libraries #####
library(ggplot2)
library(socviz)
library(tidyverse)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(NLP)
library(tm)

# Importing Data#####
MainData <- read_csv("Data/Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

#Visualization 2#####
MainData$Date <- MainData$Date %>% parse_datetime("%m/%d/%Y")

figu2_data <- data.frame(table(format(MainData$Date,"%m"), as.POSIXlt(MainData$Time, format ="%H:%M", optional = FALSE)$hour))

figu2_data

names(figu2_data)[1] <- "month"
names(figu2_data)[2] <- "hour"
names(figu2_data)[3] <- "count"

fig2<- ggplot(data = figu2_data, mapping = aes(
  y= hour, 
  x= month,
  fill = count))+
  geom_tile() +
  scale_fill_gradient(low="light blue", high="dark blue") +
  geom_text(aes(label=count))+
  theme(text = element_text(size = 18, face = "bold") , plot.title = element_text(hjust = 0.5))+
  labs(x="Month", y="Hour", title="Crashes by time by month")

fig2

#Visualization 1#####
#MainData$Date <- lubridate::mdy(MainData$Date)

Year <- MainData$Date <- as.numeric(format(MainData$Date,'%Y'))

as.data.frame(Year)

ggplot(data = MainData, mapping = aes(x = Year)) + geom_bar() + 
  theme(text = element_text(size = 12, face = "bold"), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA)) + ggtitle("Number of crashes per year from 1908 to 2021" ) + xlab("Year") + ylab("Number of Crashes") + geom_text(size = 3, stat='count', aes(label=..count..), vjust=-1) + scale_x_continuous(breaks = seq(from = 1908, to = 2022, by = 10)) + scale_y_continuous(breaks = seq(from = 0, to = 120, by = 10))

#Visualization 4#####

text <- MainData$Summary

text <- iconv(text, "latin1", "ASCII", sub="")

docs <- Corpus(VectorSource(text))

dtm <- TermDocumentMatrix(docs,control = list(removePunctuation = TRUE,
                                              stopwords = TRUE))
matrix <- as.matrix(dtm)

words <- sort(rowSums(matrix),decreasing=TRUE)

df <- data.frame(word = names(words),freq=words)

df = df[-3,]
df = df[-2,]
df = df[-1,]

head(df,100)

set.seed(1234)

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=30, random.order=TRUE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
