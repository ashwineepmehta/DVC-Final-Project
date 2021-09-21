
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



