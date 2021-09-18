
# Airplane Crashes - Data Visualization Project ####
## Importing Libraries ####
library(ggplot2)
library(socviz)
library(tidyverse)
library(dplyr)

## Importing Data ####
MainData <- read_csv("Data/Airplane_Crashes_and_Fatalities_Since_1908_1.csv") # import  data
dim(MainData)
summary(MainData)

MainData$Date<-MainData$Date %>% parse_datetime("%m/%d/%Y")
summary(MainData)

table1 <- data.frame(table(format(MainData$Date, format = "%Y")) )
table1

names(table1)[1] <- "date"
names(table1)[2] <- "crashes"
table1
str(table1)


table1 <- table1 %>%
  mutate( paste(substr(date, 1, 3), "0", sep=""))
table1
names(table1)[2] <- "crashes"
names(table1)[3] <- "date2"
table1

xx<-aggregate(crashes ~ date2, table1, sum)
names(xx)[1] <- "date"
xx

ggplot(data=xx, 
       mapping = aes(x=date, y=crashes, group=1)) +
  geom_line()

MainData
names(MainData)

figu2 <- data.frame(table(format(MainData$Date, format = "%m"),
                          format(format(MainData$Time, format = "%h:%m"), format="%h")))
figu2

figu2 <- data.frame(table(format(MainData$Date,"%m"),
                          as.POSIXlt(MainData$Time,  
                          format ="%H:%M:%OS", 
                          optional = FALSE)$hour))
figu2

names(figu2)[1] <- "month"
names(figu2)[2] <- "hour"
names(figu2)[3] <- "count"

figu2<-aggregate(count ~ . , figu2, sum)
?aggregate
figu2

ggplot(data = figu2, mapping = aes(
  y= hour, 
  x= month,
  fill = count))+
  geom_tile() +
  scale_fill_gradient(low="light blue", high="dark blue") +
  theme_classic()+
  labs(x="Month", y="Time", title="Crashes by time by month")


fig3<-aggregate(count~hour,figu2,sum)
fig3
ggplot(data = fig3, 
       mapping = aes(x=hour, 
                    y=count,
                    group=1
                          ))+
  geom_line(color = 'dark blue')

fig4<-aggregate(count~month,figu2,sum)
fig4
ggplot(data = fig4, 
       mapping = aes(x=month, 
                     y=count,
                     group=1
       ))+
  geom_line(color = 'dark blue')


mydate = as.POSIXlt('1981-4-19 7:01:00')

mydate

?as.POSIXlt
as.POSIXlt(MainData$Date)

as.POSIXlt(MainData$Time,  
           format ="%H:%M:%OS", optional = FALSE)$hour
           
  
ggplot(data = MainData, mapping = aes(
  y= format(Time, format = "%h"), 
  x=format(MainData$Date, format = "%m")),
  group_by(format(Time, format = "%h")),
  color = "count")+
  geom_tile() +
  labs(x="Month", y="Time", title="Crashes by time by month")

unique(MainData$Time)  

MainData
table1 <- data.frame(table(format(MainData$Date, format = "%Y")) )

  