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
library(data.table)
library(treemapify)
library(ggrepel)

#Visualization 1#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

MainData$Date <- MainData$Date %>% parse_datetime("%m/%d/%Y")

Viz2df <- data.frame(table(format(MainData$Date,"%b"), as.POSIXlt(MainData$Time, format ="%H:%M", optional = FALSE)$hour))

names(Viz2df)[1] <- "month"
names(Viz2df)[2] <- "hour"
names(Viz2df)[3] <- "count"

Viz2df$month <- factor(Viz2df$month , levels = month.abb)

Viz2a<- ggplot(data = Viz2df, mapping = aes(y= hour, x= month, fill = count)) + geom_tile() +
  scale_fill_gradient(low="pink", high="dark red") + geom_text(aes(label=count)) + theme(text = element_text(size = 18, face = "bold") , plot.title = element_text(hjust = 0.5)) + labs(x="Month", y="Hour", title="Crashes by time by month", fill="Crashes")

Viz2a

Viz2dfb <- Viz2df %>% group_by(month) %>% summarise(count=sum(count))
Viz2b<- ggplot(data = Viz2dfb, mapping = aes(y= count, x= month, group =1)) + geom_line(color="dark red", size=0.7) + geom_text(aes(label=count), hjust = -0.3) + ggplot2::annotate(geom = "rect", xmin = "Jul", xmax = "Dec", ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2) + ggplot2::annotate(geom = "text", x = "Sep", y = 340, label = "Crashes are higher \n 2nd half of the year") + labs(x="Month", y="Crashes", title="Crashes by month")

Viz2b

Viz2dfc <- Viz2df %>% group_by(hour) %>% summarise(count=sum(count))

Viz2c <- ggplot(data = Viz2dfc, mapping = aes(y= count, x= hour, group =1)) + geom_line(color="dark red") + geom_text_repel(aes(label=count)) + ggplot2::annotate(geom = "rect", xmin = 8, xmax = 20, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2) + ggplot2::annotate(geom = "text", x = 14, y = 100, label = "Crashes are higher in \n busy hours of the day") + labs(x="Time", y="Crashes", title="Crashes by Time")

Viz2c

#Visualization 2#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

MainData$Date <- lubridate::mdy(MainData$Date)

Year <- MainData$Date <- as.numeric(format(MainData$Date,'%Y'))

vizpart1 <- ggplot(data = MainData, mapping = aes(x = Year)) + geom_bar() + theme(text = element_text(size = 12, face = "bold"), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA)) + ggtitle("Number of crashes per year from 1908 to 2021" ) + xlab("Year") + ylab("Number of Crashes") + geom_text(size = 3, stat='count', aes(label=..count..), vjust=-1) + scale_x_continuous(breaks = seq(from = 1908, to = 2022, by = 10)) + coord_cartesian(clip = "off")

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

MainData$Date <- lubridate::mdy(MainData$Date)

Year <- MainData$Date <- as.numeric(format(MainData$Date,'%Y'))

Year <- as.data.frame(Year)

line_data <- Year %>% group_by(Year) %>% summarise(Crashes = n())

vizpart2 <- ggplot(data = line_data, aes(x = Year, y = Crashes, group = 1)) + geom_line() +
  theme(text = element_text(size = 12, face = "bold"), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA)) + ggtitle("Trend of crashes from 1908 to 2021" ) + xlab("Year") + ylab("Number of Crashes") + scale_x_continuous(breaks = seq(from = 1908, to = 2022, by = 10))

plot_grid(vizpart1, vizpart2, ncol = 1, nrow = 2)

#Visualization 3#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

AC_Type_Data <- fread("Airplane_Crashes_and_Fatalities_Since_1908_final.csv", select = c("AC_Type_Edited", "Fatalities"))

grouped_AC_Type <- AC_Type_Data %>% group_by(AC_Type_Edited) %>% summarize(types = n(), fatalities = sum(Fatalities, na.rm = TRUE))

counts <- grouped_AC_Type[order(-grouped_AC_Type$types),]

top_n <- counts[1:10,]

p <- ggplot(data = top_n, mapping = aes(x = reorder(AC_Type_Edited, types), y = types, fill = fatalities))

p + geom_col() + coord_flip() + theme(text = element_text(size = 15, face = "bold"), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent", colour = NA)) + ggtitle("Frequent Aircraft Types" ) + xlab("Aircraft Type") + ylab("Frequency") + scale_fill_gradient(low = "pink", high = "dark red")

#Visualization 4#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

Viz5df <- MainData %>% separate(Location , c("City", "Country"), sep=',')

World <- map_data("world")

US <- map_data("state")
US <- select(US,region)
names(US)[1] <- "state"

Viz5df$region <- str_trim(tolower(Viz5df$Country))

World$region <- tolower(World$region)

US.states.DQ <- data.frame(c('Wyoming', 'Wisconson', 'Wisconsin', 'West Virginia', 'Washington DC', 'Washington', 'Washingon', 'Virginia.', 'Virginia', 'US Virgin Islands', 'United States', 'Texas', 'Tennessee', 'Tennesee', 'South Dekota', 'South Dakota', 'South Carolina', 'Rhode Island', 'Pennsylvania', 'Oregon', 'Oklahoma', 'Ohio', 'North Carolina', 'New York (Idlewild)', 'New York', 'New Mexico', 'New Jersey', 'New Hampshire', 'Nevada', 'Nebraska', 'Montana', 'Missouri', 'Mississippi', 'Mississipi', 'Minnisota', 'Minnesota', 'Michigan', 'Massachutes', 'Massachusetts', ': massachusetts', 'Maryland', 'Maine', 'Louisiana', 'Kentucky', 'Kansas', 'Iowa', 'Indiana', 'Illinois', 'Ilinois', 'Idaho', 'Georgia', 'Florida', 'Deleware', 'Delaware', 'Connecticut', 'Coloado', 'Calilfornia', 'Californiia', 'California', 'Arkansas', 'Arizona', 'Alaska', 'Alakska', 'Alaksa', 'Alabama', 'Airzona'))

names(US.states.DQ)[1] <- "state"
US.states.DQ$state <- tolower(US.states.DQ$state)

Viz5df$region[Viz5df$region %in% unique(US$state)] <- "usa"
Viz5df$region[Viz5df$region %in% US.states.DQ$state] <- "usa"

Viz5df1 <- Viz5df %>% group_by(region) %>% summarise(Crashes = n())

Viz5df1 <- left_join(World, Viz5df1)

Viz5a <- ggplot(data = Viz5df1, mapping = aes(x = long, y = lat, group = group))

Viz5a <- Viz5a + geom_polygon(data = Viz5df1, mapping = aes(fill = Crashes)) + scale_fill_gradient(low = "pink", high = "dark red") + labs(x="", y="", title="Crashes by Country")

Viz5a

Viz5df$region[Viz5df$region %in% US$state] <- "usa"
Viz5df2 <- Viz5df %>% group_by(region) %>% summarise(Crashes = n(), Fatalities = sum(Fatalities))
Viz5df2 <- top_n(Viz5df2, 10, Crashes) %>% drop_na()
Viz5df2$region <- str_to_title(Viz5df2$region)

Viz5b <- ggplot(data = Viz5df2, mapping = aes(x = reorder(region,Crashes, na.rm=TRUE), y = Crashes, fill = Fatalities)) + coord_flip() + geom_col() + labs(x="Country", title = "Crashes: Top 10 Countries") + scale_fill_gradient(low = "pink", high = "dark red")

Viz5b

#Visualization 5#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

text <- MainData$Summary

text <- iconv(text, "latin1", "ASCII", sub = "")

docs <- Corpus(VectorSource(text))

dtm <- TermDocumentMatrix(docs,control = list(removePunctuation = TRUE, stopwords = TRUE))
matrix <- as.matrix(dtm)

words <- sort(rowSums(matrix), decreasing=TRUE)

df <- data.frame(word = names(words), freq=words)

#df = df[-3,]
#df = df[-2,]
#df = df[-1,]

head(df,30)

set.seed(1234)

wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=30, random.order=TRUE, rot.per=0.35,
colors=brewer.pal(8, "Dark2"))

#Excel Automation#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

MainData$Summary <- iconv(MainData$Summary, "latin1", "ASCII", sub="")

MainData<- MainData %>%  mutate(
  CrashClass = case_when(
    #Main Reasons - This part will override anything
    str_detect(tolower(Summary), "pilot error") ~ "Human",
    str_detect(tolower(Summary), "crew error") ~ "Human",
    str_detect(tolower(Summary), "judge") ~ "Human",
    str_detect(tolower(Summary), "unknown circumstances") ~ "Unknown",
    str_detect(tolower(Summary), "unknown reason") ~ "Unknown",
    str_detect(tolower(Summary), "undetermined reason") ~ "Unknown",
    str_detect(tolower(Summary), "cause undetermined") ~ "Unknown",
    str_detect(tolower(Summary), "undetermined cause") ~ "Unknown",
    str_detect(tolower(Summary), "unknown") ~ "Unknown", 
    is.na(Summary) ~ "Unknown",
    str_detect(tolower(Summary), "disappeared") ~ "Unknown",
    
    #Weather section
    str_detect(tolower(Summary), "weather") | 
      str_detect(tolower(Summary), "visibility") | # poor / low
      str_detect(tolower(Summary), "thunderstorm") |
      str_detect(tolower(Summary), "rain") |
      str_detect(tolower(Summary), "snow") |
      str_detect(tolower(Summary), "icing") |
      str_detect(tolower(Summary), "ice ") |
      str_detect(tolower(Summary), "foggy") |
      str_detect(tolower(Summary), "fog") |
      str_detect(tolower(Summary), "severe turbulence") |
      str_detect(tolower(Summary), "headwinds") |
      str_detect(tolower(Summary), "wind") |
      str_detect(tolower(Summary), "lightning") |
      str_detect(tolower(Summary), "rainstorm") |
      str_detect(tolower(Summary), "overcast") |
      str_detect(tolower(Summary), "snowstorm") |
      str_detect(tolower(Summary), "storm") |
      str_detect(tolower(Summary), "sandstorm") |
      str_detect(tolower(Summary), "cloud") | 
      str_detect(tolower(Summary), "smoke and haze") |
      str_detect(tolower(Summary), "downdrafts") |
      str_detect(tolower(Summary), "turbulence") 
    ~ "Weather",
    
    #Shooting section
    str_detect(tolower(Summary), "shot down") | 
      str_detect(tolower(Summary), "missile") |
      str_detect(tolower(Summary), "flight fire") |
      str_detect(tolower(Summary), "aircraft fire") |
      str_detect(tolower(Summary), "shot down") |
      str_detect(tolower(Summary), "shooting")
    ~ "Shooting",
    
    #Mechanical Section
    str_detect(tolower(Summary), "failure") | 
      str_detect(tolower(Summary), "mechanical") |
      str_detect(tolower(Summary), "engine") |
      str_detect(tolower(Summary), "loss of power") | 
      str_detect(tolower(Summary), "lost power") | 
      str_detect(tolower(Summary), "fuel") |
      str_detect(tolower(Summary), "wing") |
      str_detect(tolower(Summary), "lose of control") |
      str_detect(tolower(Summary), "loss of control") |
      str_detect(tolower(Summary), "control") |
      str_detect(tolower(Summary), "jambed") | 
      str_detect(tolower(Summary), "gear") |
      str_detect(tolower(Summary), "collapsed") |
      str_detect(tolower(Summary), "caught fire") |
      str_detect(tolower(Summary), "flame") |
      str_detect(tolower(Summary), "stall") | 
      str_detect(tolower(Summary), "explo") | # for exploded / explosion
      str_detect(tolower(Summary), "loss") | 
      str_detect(tolower(Summary), "elevator") |
      str_detect(tolower(Summary), "emergency")
    ~ "Mechanical",
    
    #Human Section
    str_detect(tolower(Summary), "error") |
      str_detect(tolower(Summary), "heavily loaded") |
      str_detect(tolower(Summary), "improperly loaded") |
      str_detect(tolower(Summary), "explosive") |  
      str_detect(tolower(Summary), "bomb ") |
      str_detect(tolower(Summary), "bomb,") |
      str_detect(tolower(Summary), "pilot") | 
      str_detect(tolower(Summary), "hijack") |
      str_detect(tolower(Summary), "crew") |
      str_detect(Summary, "ATC ") |
      str_detect(Summary, "ATC.") |
      str_detect(tolower(Summary), "overloaded")
    ~ "Human",
    
    #Collision Section
    str_detect(tolower(Summary), "collision") |
      str_detect(tolower(Summary), "power lines") |
      str_detect(tolower(Summary), "power cables") | 
      str_detect(tolower(Summary), "struck") |
      str_detect(tolower(Summary), "hit") | 
      str_detect(tolower(Summary), "clipped") |
      str_detect(tolower(Summary), "collided")  
    ~ "Collision",
    
    #Do at last
    str_detect(tolower(Summary), "fire") ~ "Mechanical",
    str_detect(tolower(Summary), "runway") ~ "Human",
    str_detect(Summary, "IFR ") ~ "Human",
    str_detect(Summary, "VFR ") ~ "Human", 
    str_detect(tolower(Summary), "improper") ~ "Human",
    
    #Anything else
    TRUE~"Unknown"
  )
)

#Visualization 6#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

MainData$Summary <- iconv(MainData$Summary, "latin1", "ASCII", sub="")

MainData<- MainData %>%  mutate(
  CrashClass = case_when(
    #Main Reasons - This part will override anything
    str_detect(tolower(Summary), "pilot error") ~ "Human",
    str_detect(tolower(Summary), "crew error") ~ "Human",
    str_detect(tolower(Summary), "judge") ~ "Human",
    str_detect(tolower(Summary), "unknown circumstances") ~ "Unknown",
    str_detect(tolower(Summary), "unknown reason") ~ "Unknown",
    str_detect(tolower(Summary), "undetermined reason") ~ "Unknown",
    str_detect(tolower(Summary), "cause undetermined") ~ "Unknown",
    str_detect(tolower(Summary), "undetermined cause") ~ "Unknown",
    str_detect(tolower(Summary), "unknown") ~ "Unknown", 
    is.na(Summary) ~ "Unknown",
    str_detect(tolower(Summary), "disappeared") ~ "Unknown",
    
    #Weather section
    str_detect(tolower(Summary), "weather") | 
      str_detect(tolower(Summary), "visibility") | # poor / low
      str_detect(tolower(Summary), "thunderstorm") |
      str_detect(tolower(Summary), "rain") |
      str_detect(tolower(Summary), "snow") |
      str_detect(tolower(Summary), "icing") |
      str_detect(tolower(Summary), "ice ") |
      str_detect(tolower(Summary), "foggy") |
      str_detect(tolower(Summary), "fog") |
      str_detect(tolower(Summary), "severe turbulence") |
      str_detect(tolower(Summary), "headwinds") |
      str_detect(tolower(Summary), "wind") |
      str_detect(tolower(Summary), "lightning") |
      str_detect(tolower(Summary), "rainstorm") |
      str_detect(tolower(Summary), "overcast") |
      str_detect(tolower(Summary), "snowstorm") |
      str_detect(tolower(Summary), "storm") |
      str_detect(tolower(Summary), "sandstorm") |
      str_detect(tolower(Summary), "cloud") | 
      str_detect(tolower(Summary), "smoke and haze") |
      str_detect(tolower(Summary), "downdrafts") |
      str_detect(tolower(Summary), "turbulence") 
    ~ "Weather",
    
    #Shooting section
    str_detect(tolower(Summary), "shot down") | 
      str_detect(tolower(Summary), "missile") |
      str_detect(tolower(Summary), "flight fire") |
      str_detect(tolower(Summary), "aircraft fire") |
      str_detect(tolower(Summary), "shot down") |
      str_detect(tolower(Summary), "shooting")
    ~ "Shooting",
    
    #Mechanical Section
    str_detect(tolower(Summary), "failure") | 
      str_detect(tolower(Summary), "mechanical") |
      str_detect(tolower(Summary), "engine") |
      str_detect(tolower(Summary), "loss of power") | 
      str_detect(tolower(Summary), "lost power") | 
      str_detect(tolower(Summary), "fuel") |
      str_detect(tolower(Summary), "wing") |
      str_detect(tolower(Summary), "lose of control") |
      str_detect(tolower(Summary), "loss of control") |
      str_detect(tolower(Summary), "control") |
      str_detect(tolower(Summary), "jambed") | 
      str_detect(tolower(Summary), "gear") |
      str_detect(tolower(Summary), "collapsed") |
      str_detect(tolower(Summary), "caught fire") |
      str_detect(tolower(Summary), "flame") |
      str_detect(tolower(Summary), "stall") | 
      str_detect(tolower(Summary), "explo") | # for exploded / explosion
      str_detect(tolower(Summary), "loss") | 
      str_detect(tolower(Summary), "elevator") |
      str_detect(tolower(Summary), "emergency")
    ~ "Mechanical",
    
    #Human Section
    str_detect(tolower(Summary), "error") |
      str_detect(tolower(Summary), "heavily loaded") |
      str_detect(tolower(Summary), "improperly loaded") |
      str_detect(tolower(Summary), "explosive") |  
      str_detect(tolower(Summary), "bomb ") |
      str_detect(tolower(Summary), "bomb,") |
      str_detect(tolower(Summary), "pilot") | 
      str_detect(tolower(Summary), "hijack") |
      str_detect(tolower(Summary), "crew") |
      str_detect(Summary, "ATC ") |
      str_detect(Summary, "ATC.") |
      str_detect(tolower(Summary), "overloaded")
    ~ "Human",
    
    #Collision Section
    str_detect(tolower(Summary), "collision") |
      str_detect(tolower(Summary), "power lines") |
      str_detect(tolower(Summary), "power cables") | 
      str_detect(tolower(Summary), "struck") |
      str_detect(tolower(Summary), "hit") | 
      str_detect(tolower(Summary), "clipped") |
      str_detect(tolower(Summary), "collided")  
    ~ "Collision",
    
    #Do at last
    str_detect(tolower(Summary), "fire") ~ "Mechanical",
    str_detect(tolower(Summary), "runway") ~ "Human",
    str_detect(Summary, "IFR ") ~ "Human",
    str_detect(Summary, "VFR ") ~ "Human", 
    str_detect(tolower(Summary), "improper") ~ "Human",
    
    #Anything else
    TRUE~"Unknown"
  )
)

color_palette <- c("#EF6621", "#186B39", "#99004C", "#2E88B6", "#A0A0A0", "#193E54")

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

summary_crash_class <- MainData %>% group_by(CrashClass) %>% summarise(count = n())

group <- paste("Group", 1:6)
subgroup <- summary_crash_class$CrashClass

percentage_values <- ((summary_crash_class$count)/5009)*100
percentage_values <- format(round(percentage_values, 2), nsmall = 2)
summary_crash_class$percentage_values <- percentage_values
percentages <- summary_crash_class$percentage_values

value <- summary_crash_class$count

df <- data.frame(group, subgroup, value) 

ggplot(df, aes(area = value, fill = group, label = paste(subgroup, sep = "\n"))) + geom_treemap() + geom_treemap_text(colour = "white", place = "centre", size = 25) + theme(legend.position = "none") + scale_fill_manual(values = color_palette)

#Visualization 7#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

Viz7df <- MainData %>%  group_by(Operator) %>% summarise(Crashes = n(), Fatalities = sum(Fatalities))

Viz7df <- top_n(Viz7df, 10, Crashes) %>% drop_na()

Viz7a <- ggplot(data=Viz7df, mapping = aes(x= reorder(Operator, Crashes), y = Crashes, fill= Fatalities)) + geom_col() + coord_flip() + scale_fill_gradient(low = "pink", high = "dark red") +labs(title = "Crashes: Top 10 Operator", x="Operator", subtitle = "Comparison - Bar Chart")

Viz7a

Viz7b <- ggplot(data=subset(MainData, MainData$Operator %in% unique(Viz7df$Operator)), mapping = aes(x= reorder(Operator, Fatalities ), y=Fatalities)) + geom_boxplot() + coord_flip() + labs(title = "Crashes: Top 10 Operator", x="Operator", subtitle = "Fatalities Distribution - Boxplot Chart") 

Viz7b

Viz7c <- ggplot(data=subset(MainData, MainData$Operator %in% unique(Viz7df$Operator)), mapping = aes(x= reorder(Operator, Fatalities ), y=Fatalities, color=Operator)) + geom_jitter() +  coord_flip() + guides(color="none") +  labs(title = "Crashes: Top 10 Operator", x="Operator", subtitle = "Fatalities Distribution - Jitter Chart")

Viz7c

#Visualization 8#####
MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

summary_crash_class <- MainData %>% group_by(CrashClass) %>% summarise(count = n())

percentage_values <- ((summary_crash_class$count)/5009)*100

percentage_values <- format(round(percentage_values, 2), nsmall = 2)

summary_crash_class$percentage_values <- percentage_values

mycols <- c("#FF8A83", "#D65F59", "#991101", "#EABFB9", "#C23210", "#650E15")

mycols1 <- c("#2E88B6", "#C83E6C", "#BC6CCA", "#EF8A58", "#59916F", "#E2B95B")

summary_crash_class <- summary_crash_class %>% arrange(desc(CrashClass)) %>%  mutate(lab.ypos = cumsum(as.numeric(count)) - 0.5 * as.numeric(count)) %>% arrange(desc(percentage_values))

summary_crash_class

ggplot(data = summary_crash_class, aes(x = "", y = count, fill = CrashClass)) + geom_bar(width = 1, stat = "identity", color = "white") + coord_polar("y", start = 0) + geom_text(aes(y = lab.ypos, label = percentage_values), color = "white") + scale_fill_manual(values = mycols1) + theme_void() + theme(legend.title= element_blank())
