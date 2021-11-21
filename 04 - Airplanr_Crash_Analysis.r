#Airplane Crash Analysis - Data Visualization and Communication Project#####

#Importing & installing libraries#####

if (!requireNamespace("ggplot2", quietly = TRUE))
library(ggplot2)
if (!requireNamespace("socviz", quietly = TRUE))
library(socviz)
if (!requireNamespace("tidyverse", quietly = TRUE))
library(tidyverse)
if (!requireNamespace("dplyr", quietly = TRUE))
library(dplyr)
if (!requireNamespace("wordcloud", quietly = TRUE))
library(wordcloud)
if (!requireNamespace("treemapify", quietly = TRUE))
library(RColorBrewer)
if (!requireNamespace("treemapify", quietly = TRUE))
library(wordcloud2)
if (!requireNamespace("treemapify", quietly = TRUE))
library(NLP)
if (!requireNamespace("tm", quietly = TRUE))
library(tm)
if (!requireNamespace("data.table", quietly = TRUE))
library(data.table)
if (!requireNamespace("treemapify", quietly = TRUE))
library(treemapify)
if (!requireNamespace("ggrepel", quietly = TRUE))
library(ggrepel)
if (!requireNamespace("cowplot", quietly = TRUE))
library(cowplot)
if (!requireNamespace("tm", quietly = TRUE))
library(tm)
if (!requireNamespace("RColorBrowser", quietly = TRUE))
library(RColorBrewer)
if (!requireNamespace("RCurl", quietly = TRUE))
library(RCurl)

#Data import and preparation####

setwd("~/Data")

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

MainData$Date.Class <- lubridate::mdy(MainData$Date)

MainData$Year <- as.numeric(format(MainData$Date.Class,'%Y'))

#Visualization 1a#####

crashes_by_year <- MainData %>% group_by(Year) %>% summarise(Crash = n())

ordered_data <- top_n(crashes_by_year, 10, Crash) %>% drop_na()

sub_data <- subset(ordered_data, Crash == max(Crash))

MainData <- MainData %>% mutate(max_flag = ifelse(Year %in% sub_data[1], "1", "0"))

viz1a <- ggplot(data = MainData) + 
                geom_bar(data = MainData, mapping = aes(x = Year, fill = max_flag)) + 
                scale_fill_manual(values = c("red","grey70"), breaks = c("1"), labels = "Year 1946 with maximum of 88 crashes") + 
                ylim(0, 100) +
                labs(title = "Airplane crashes per year",
                x = "Year", y = "Number of crashes") +
                scale_x_continuous(breaks = seq(from = 1908, to = 2022, by = 10)) + 
                theme(text = element_text(size = 17, face = "bold"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                panel.background = element_rect(fill = "transparent", colour = NA), 
                plot.background = element_rect(fill = "transparent", colour = NA)) + 
                coord_cartesian(clip = "off") + 
                theme(legend.position = "bottom", legend.title = element_blank())

viz1a

#Visualization 1b#####

line_data <- MainData %>% group_by(Year) %>% summarise(Crashes = n())

viz1b <- ggplot(data = line_data, 
                aes(x = Year, y = Crashes, group = 1)) + 
                geom_line(color="dark red", size = 0.8) + 
                theme(text = element_text(size = 17, face = "bold"), 
                panel.grid.major = element_blank(), 
                plot.title = element_text(hjust = 0.5), 
                plot.subtitle = element_text(hjust = 0.5),
                panel.grid.minor = element_blank(), 
                panel.background = element_rect(fill = "transparent",colour = NA), 
                plot.background = element_rect(fill = "transparent",colour = NA)) + 
                ggtitle("Trend of crashes" ) + 
                labs(caption = "WW 1: World War 1      WW 2: World War 2") +
                xlab("Year") + 
                ylab("Number of Crashes") + 
                scale_x_continuous(breaks = seq(from = 1908, to = 2022, by = 10)) +
                ggplot2::annotate(geom = "rect", 
                                  xmin = 1914, 
                                  xmax = 1918, 
                                  ymin = -Inf, 
                                  ymax = 88, 
                                  fill = "red", 
                                  alpha = 0.2) + 
                ggplot2::annotate(geom = "text", 
                                  x = 1916, 
                                  y = 85, 
                                  label = "WW 1", hjust = 0.5,
                                  size = 4.5) +
                ggplot2::annotate(geom = "rect", 
                                  xmin = 1939, 
                                  xmax = 1945, 
                                  ymin = -Inf, 
                                  ymax = 88, 
                                  fill = "red", 
                                  alpha = 0.2) + 
                ggplot2::annotate(geom = "text", 
                                  x = 1942, 
                                  y = 85, 
                                  label = "WW 2", 
                                  hjust = 0.5,
                                  size = 4.5) +
                ggplot2::annotate(geom = "rect", 
                                  xmin = 1946, 
                                  xmax = 1991, 
                                  ymin = -Inf, 
                                  ymax = 88, 
                                  fill = "orange", 
                                  alpha = 0.2,
                                  size = 4.5) + 
                ggplot2::annotate(geom = "text", 
                                  x = 1970, 
                                  y = 85, 
                                  label = "Cold War",
                                  size = 4.5) +
                ggplot2::annotate(geom = "rect", 
                                  xmin = 2000, 
                                  xmax = Inf, 
                                  ymin = -Inf, 
                                  ymax = 88, 
                                  fill = "dark blue", 
                                  alpha = 0.2) + 
                ggplot2::annotate(geom = "text", 
                                  x = 2008, 
                                  y = 85, 
                                  label = "  2nd Millennium", hjust = 0,
                                  size = 4.5) +
                ylim(0, 100)

viz1b

#Visualization 2a#####

MainData$Dateobj <- MainData$Date %>% parse_datetime("%m/%d/%Y")

Viz2df <- data.frame(table(format(MainData$Dateobj,"%b"), as.POSIXlt(MainData$Time, format ="%H:%M", optional = FALSE)$hour))

names(Viz2df) <- c("month","hour", "count")

Viz2df$month <- factor(Viz2df$month , levels = month.abb)

Viz2dfa <- Viz2df %>% group_by(month) %>% summarise(count=sum(count))

Viz2a<- ggplot(data = Viz2dfa,
               mapping = aes(y = count, x= month, group = 1)) + 
               geom_line(color = "dark red", size = 0.8) + 
               ylim(200, 400) + 
               geom_text(aes(label = count), hjust = -0.5, vjust = 1.0, size = 5) + 
               ggplot2::annotate(geom = "rect", xmin = "Jul", xmax = Inf, ymin = -Inf, ymax = Inf, fill                                  = "red", alpha = 0.2) + 
               ggplot2::annotate(geom = "text", x = "Sep", y = 380, size = 5, label = "Crashes are higher from Jul to Jan", hjust = 0.2) + 
               ggplot2::annotate(geom = "rect", xmin = -Inf, xmax = "Jan", ymin = -Inf, ymax = 400, fill                                  = "red", alpha = 0.2) +
               labs(x = "Month", y = "Crashes", title = "Crashes by month") +
               theme(text = element_text(size = 17, face = "bold"), panel.grid.major = element_blank(), 
                     plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
                     panel.grid.minor = element_blank(), panel.background = element_rect(fill =                              "transparent", colour = NA), plot.background = element_rect(fill = "transparent"                        , colour = NA))

Viz2a

#Visualization 2b#####

Viz2dfb <- Viz2df %>% group_by(hour) %>% summarise(count = sum(count))

Viz2b <- ggplot(data = Viz2dfb, 
                mapping = aes(y = count, x= hour, group = 1)) + 
                geom_line(color = "dark red", size = 0.8) + 
                ylim(0, 250) + 
                geom_text_repel(aes(label = count), direction = "both") + 
                ggplot2::annotate(geom = "rect", 
                                  xmin = 8, 
                                  xmax = 20, 
                                  ymin = -Inf, 
                                  ymax = Inf, 
                                  fill = "red", 
                                  alpha = 0.2) + 
                ggplot2::annotate(geom = "text", 
                                  x = 14, 
                                  y = 120,
                                  size = 5,
                                  label = "Crashes are higher in busy hours of the day") + 
                labs(x = "Time", y = "Crashes", title = "Crashes by time") +
                theme(text = element_text(size = 17, face = "bold"), 
                panel.grid.major = element_blank(), 
                plot.title = element_text(hjust = 0.5), 
                plot.subtitle = element_text(hjust = 0.5),
                panel.grid.minor = element_blank(), 
                panel.background = element_rect(fill = "transparent", colour = NA), 
                plot.background = element_rect(fill = "transparent", colour = NA))

Viz2b

#Visualization 3#####

MainData<- MainData %>%  mutate(AC_Type_Edited = case_when(
  str_detect(`AC Type`, "Goodyear-Zeppelin U.S.S. Macon (airship)") ~ "Goodyear-Zeppelin U.S.S. Macon   (airship)",
    str_detect(`AC Type`, "Hadley Page 137Jetstream I / Cessna 206") ~ "Hadley Page 137Jetstream I / Cessna 206",
    str_detect(`AC Type`, "Rutan Long EZ (experimental aircraft)") ~ "Rutan Long EZ (experimental aircraft)",
    str_detect(`AC Type`, "PBY4-2 Privateer / PB4Y-2 Privateer") ~ "PBY4-2 Privateer / PB4Y-2 Privateer",
    str_detect(`AC Type`, "Westland Sea King HC-4 (helicopter)") ~ "Westland Sea King HC-4 (helicopter)",
    str_detect(`AC Type`, "Grazhdansky Vozdushnyi Flot PS-84") ~ "Grazhdansky Vozdushnyi Flot PS-84",
    str_detect(`AC Type`, "DHC-6 Twin Otter 300 / NAMC YS-11") ~ "DHC-6 Twin Otter 300 / NAMC YS-11",
    str_detect(`AC Type`, "Shaanxi Yunshuji Y-8/Yunshuji Y-8") ~ "Shaanxi Yunshuji Y-8/Yunshuji Y-8",
    str_detect(`AC Type`, "Aviation Traders ATL-98 Carvair") ~ "Aviation Traders ATL-98 Carvair",
    str_detect(`AC Type`, "EC-121H (Super Constellation)") ~ "EC-121H (Super Constellation)",
    str_detect(`AC Type`, "Transportes AÈreos Orientales") ~ "Transportes AÈreos Orientales",
    str_detect(`AC Type`, "Blackburn Beverley C Mark 1") ~ "Blackburn Beverley C Mark 1",
    str_detect(`AC Type`, "Military - U.S. Air Force") ~ "Military - U.S. Air Force",
    str_detect(`AC Type`, "Burgess RV-6 experimental") ~ "Burgess RV-6 experimental",
    str_detect(`AC Type`, "UC-64A Noorduyn Norseman") ~ "UC-64A Noorduyn Norseman",
    str_detect(`AC Type`, "F-4C Phantom jet fighter") ~ "F-4C Phantom jet fighter",
    str_detect(`AC Type`, "MH-47 Chinook helicopter") ~ "MH-47 Chinook helicopter",
    str_detect(`AC Type`, "Robertson R44 helicopter") ~ "Robertson R44 helicopter",
    str_detect(`AC Type`, "General Aviation GA-43") ~ "General Aviation GA-43",
    str_detect(`AC Type`, "LiorÈ-et-Olivier H-242") ~ "LiorÈ-et-Olivier H-242",
    str_detect(`AC Type`, "Boulton and Paul P-71") ~ "Boulton and Paul P-71",
    str_detect(`AC Type`, "B-17C Flying Fortress") ~ "B-17C Flying Fortress",
    str_detect(`AC Type`, "Hindustan Aeronautics") ~ "Hindustan Aeronautics",
    str_detect(`AC Type`, "Liore et Olivier 213") ~ "Liore et Olivier 213",
    str_detect(`AC Type`, "B17G Flying Fortress") ~ "B17G Flying Fortress",
    str_detect(`AC Type`, "Catalina Flying Boat") ~ "Catalina Flying Boat",
    str_detect(`AC Type`, "IPTN 332C Super Puma") ~ "IPTN 332C Super Puma",
    str_detect(`AC Type`, "Armstrong-Whitworth") ~ "Armstrong-Whitworth",
    str_detect(`AC Type`, "Royal Airship Works") ~ "Royal Airship Works",
    str_detect(`AC Type`, "De Havilland Canada") ~ "De Havilland Canada",
    str_detect(`AC Type`, "C-47 Dakota DT-941") ~ "C-47 Dakota DT-941",
    str_detect(`AC Type`, "Channel Air Bridge") ~ "Channel Air Bridge",
    str_detect(`AC Type`, "Chance Vought F-8E") ~ "Chance Vought F-8E",
    str_detect(`AC Type`, "Agusta A109A MK II") ~ "Agusta A109A MK II",
    str_detect(`AC Type`, "EMB 721C Sertanejo") ~ "EMB 721C Sertanejo",
    str_detect(`AC Type`, "Reims Aviation 406") ~ "Reims Aviation 406",
    str_detect(`AC Type`, "CH53E Sea Stallion") ~ "CH53E Sea Stallion",
    str_detect(`AC Type`, "Noorduyn Norseman") ~ "Noorduyn Norseman",
    str_detect(`AC Type`, "McDonnell Douglas") ~ "McDonnell Douglas",
    str_detect(`AC Type`, "HAL-748-224 Srs.2") ~ "HAL-748-224 Srs.2",
    str_detect(`AC Type`, "British Aerospace") ~ "British Aerospace",
    str_detect(`AC Type`, "Pacific Aerospace") ~ "Pacific Aerospace",
    str_detect(`AC Type`, "Liore et Olivier") ~ "Liore et Olivier",
    str_detect(`AC Type`, "Vickers Viscount") ~ "Vickers Viscount",
    str_detect(`AC Type`, "Savoia-Marchetti") ~ "Savoia-Marchetti",
    str_detect(`AC Type`, "Rochrbach Roland") ~ "Rochrbach Roland",
    str_detect(`AC Type`, "Ilyushin IL-14M") ~ "Ilyushin IL-14M",
    str_detect(`AC Type`, "Sepecat Jaguar A") ~ "Sepecat Jaguar A",
    str_detect(`AC Type`, "PZL-Mielec AN-2R") ~ "PZL-Mielec AN-2R",
    str_detect(`AC Type`, "Rohrbach Roland") ~ "Rohrbach Roland",
    str_detect(`AC Type`, "Spartan Cruiser") ~ "Spartan Cruiser",
    str_detect(`AC Type`, "Saro A-19 Cloud") ~ "Saro A-19 Cloud",
    str_detect(`AC Type`, "Koolhoven FK.43") ~ "Koolhoven FK.43",
    str_detect(`AC Type`, "Shin Meiwa PS-1") ~ "Shin Meiwa PS-1",
    str_detect(`AC Type`, "Britten-Norman ") ~ "Britten-Norman ",
    str_detect(`AC Type`, "Mi-8 helicopter") ~ "Mi-8 helicopter",
    str_detect(`AC Type`, "Sukhoi Superjet") ~ "Sukhoi Superjet",
    str_detect(`AC Type`, "Both Eurocopter") ~ "Both Eurocopter",
    str_detect(`AC Type`, "Super Zeppelin") ~ "Super Zeppelin",
    str_detect(`AC Type`, "Curtiss-Wright") ~ "Curtiss-Wright",
    str_detect(`AC Type`, "North American") ~ "North American",
    str_detect(`AC Type`, "F-88 Sabre Jet") ~ "F-88 Sabre Jet",
    str_detect(`AC Type`, "Aero Commander") ~ "Aero Commander",
    str_detect(`AC Type`, "CF-100 Mark 4B") ~ "CF-100 Mark 4B",
    str_detect(`AC Type`, "Piaggio PD-808") ~ "Piaggio PD-808",
    str_detect(`AC Type`, "Britten-Norman") ~ "Britten-Norman",
    str_detect(`AC Type`, "Norman BN-2A-6") ~ "Norman BN-2A-6",
    str_detect(`AC Type`, "CH-47D Chinook") ~ "CH-47D Chinook",
    str_detect(`AC Type`, "PZL-Mielec M28") ~ "PZL-Mielec M28",
    str_detect(`AC Type`, "Messerschmitt") ~ "Messerschmitt",
    str_detect(`AC Type`, "OFM F-VIIb/3m") ~ "OFM F-VIIb/3m",
    str_detect(`AC Type`, "Blackburn B-2") ~ "Blackburn B-2",
    str_detect(`AC Type`, "Nakajima A-T2") ~ "Nakajima A-T2",
    str_detect(`AC Type`, "Caravelle VIR") ~ "Caravelle VIR",
    str_detect(`AC Type`, "Gates Learjet") ~ "Gates Learjet",
    str_detect(`AC Type`, "F-86 Sabrejet") ~ "F-86 Sabrejet",
    str_detect(`AC Type`, "Enstrom F-28F") ~ "Enstrom F-28F",
    str_detect(`AC Type`, "DHC-5 Buffalo") ~ "DHC-5 Buffalo",
    str_detect(`AC Type`, "Xian Yunshuji") ~ "Xian Yunshuji",
    str_detect(`AC Type`, "CH-47 Chinook") ~ "CH-47 Chinook",
    str_detect(`AC Type`, "Schutte-Lanz") ~ "Schutte-Lanz",
    str_detect(`AC Type`, "De Havilland") ~ "De Havilland",
    str_detect(`AC Type`, "Handley Page") ~ "Handley Page",
    str_detect(`AC Type`, "Bleriot Spad") ~ "Bleriot Spad",
    str_detect(`AC Type`, "Consolidated") ~ "Consolidated",
    str_detect(`AC Type`, "V6 (airship)") ~ "V6 (airship)",
    str_detect(`AC Type`, "Faucett F-19") ~ "Faucett F-19",
    str_detect(`AC Type`, "Five Grumman") ~ "Five Grumman",
    str_detect(`AC Type`, "AAC-1 Toucan") ~ "AAC-1 Toucan",
    str_detect(`AC Type`, "PBY Catalina") ~ "PBY Catalina",
    str_detect(`AC Type`, "Sud Aviation") ~ "Sud Aviation",
    str_detect(`AC Type`, "Aerospatiale") ~ "Aerospatiale",
    str_detect(`AC Type`, "A-7D Corsair") ~ "A-7D Corsair",
    str_detect(`AC Type`, "Shaanxi Y-8D") ~ "Shaanxi Y-8D",
    str_detect(`AC Type`, "Sukhoi Su-27") ~ "Sukhoi Su-27",
    str_detect(`AC Type`, "Supermarine") ~ "Supermarine",
    str_detect(`AC Type`, "Twin Apache") ~ "Twin Apache",
    str_detect(`AC Type`, "HAL-748-224") ~ "HAL-748-224",
    str_detect(`AC Type`, "Volpar C45G") ~ "Volpar C45G",
    str_detect(`AC Type`, "Soloy 12EJ3") ~ "Soloy 12EJ3",
    str_detect(`AC Type`, "HS-125-700B") ~ "HS-125-700B",
    str_detect(`AC Type`, "Eurocopter ") ~ "Eurocopter ",
    str_detect(`AC Type`, "Bandeirante") ~ "Bandeirante",
    str_detect(`AC Type`, "Aeromarine") ~ "Aeromarine",
    str_detect(`AC Type`, "Travel Air") ~ "Travel Air",
    str_detect(`AC Type`, "Cant Z-506") ~ "Cant Z-506",
    str_detect(`AC Type`, "Mitsubishi") ~ "Mitsubishi",
    str_detect(`AC Type`, "Beechcraft") ~ "Beechcraft",
    str_detect(`AC Type`, "C-47(DC-3)") ~ "C-47(DC-3)",
    str_detect(`AC Type`, "MiG-15 UTI") ~ "MiG-15 UTI",
    str_detect(`AC Type`, "Swearingen") ~ "Swearingen",
    str_detect(`AC Type`, "Howard 250") ~ "Howard 250",
    str_detect(`AC Type`, "Helicopter") ~ "Helicopter",
    str_detect(`AC Type`, "ATR-72-212") ~ "ATR-72-212",
    str_detect(`AC Type`, "Eurocopter") ~ "Eurocopter",
    str_detect(`AC Type`, "ATR 42-300") ~ "ATR 42-300",
    str_detect(`AC Type`, "Black Hawk") ~ "Black Hawk",
    str_detect(`AC Type`, "Bombardier") ~ "Bombardier",
    str_detect(`AC Type`, "Dirigible") ~ "Dirigible",
    str_detect(`AC Type`, "Fairchild") ~ "Fairchild",
    str_detect(`AC Type`, "Latecoere") ~ "Latecoere",
    str_detect(`AC Type`, "Desoutter") ~ "Desoutter",
    str_detect(`AC Type`, "Pitcairns") ~ "Pitcairns",
    str_detect(`AC Type`, "Dewoitine") ~ "Dewoitine",
    str_detect(`AC Type`, "CRDA CANT") ~ "CRDA CANT",
    str_detect(`AC Type`, "Potez 621") ~ "Potez 621",
    str_detect(`AC Type`, "VC-Viking") ~ "VC-Viking",
    str_detect(`AC Type`, "Sikorksky") ~ "Sikorksky",
    str_detect(`AC Type`, "Arava 201") ~ "Arava 201",
    str_detect(`AC Type`, "Urocopter") ~ "Urocopter",
    str_detect(`AC Type`, "Zeppelin") ~ "Zeppelin",
    str_detect(`AC Type`, "Handley ") ~ "Handley ",
    str_detect(`AC Type`, "Sikorsky") ~ "Sikorsky",
    str_detect(`AC Type`, "Hamilton") ~ "Hamilton",
    str_detect(`AC Type`, "Stearman") ~ "Stearman",
    str_detect(`AC Type`, "Lockheed") ~ "Lockheed",
    str_detect(`AC Type`, "Pitcairn") ~ "Pitcairn",
    str_detect(`AC Type`, "Northrop") ~ "Northrop",
    str_detect(`AC Type`, "Airspeed") ~ "Airspeed",
    str_detect(`AC Type`, "Ilyushin") ~ "Ilyushin",
    str_detect(`AC Type`, "Canadair") ~ "Canadair",
    str_detect(`AC Type`, "Aeroflot") ~ "Aeroflot",
    str_detect(`AC Type`, "L-Hudson") ~ "L-Hudson",
    str_detect(`AC Type`, "Goodyear") ~ "Goodyear",
    str_detect(`AC Type`, "Yakovlev") ~ "Yakovlev",
    str_detect(`AC Type`, "Transall") ~ "Transall",
    str_detect(`AC Type`, "Dassault") ~ "Dassault",
    str_detect(`AC Type`, "Kawasaki") ~ "Kawasaki",
    str_detect(`AC Type`, "Yunshuji") ~ "Yunshuji",
    str_detect(`AC Type`, "Rockwell") ~ "Rockwell",
    str_detect(`AC Type`, "Fletcher") ~ "Fletcher",
    str_detect(`AC Type`, "Aerocomp") ~ "Aerocomp",
    str_detect(`AC Type`, "Curtiss") ~ "Curtiss",
    str_detect(`AC Type`, "Airship") ~ "Airship",
    str_detect(`AC Type`, "Caproni") ~ "Caproni",
    str_detect(`AC Type`, "Junkers") ~ "Junkers",
    str_detect(`AC Type`, "Salmson") ~ "Salmson",
    str_detect(`AC Type`, "Breguet") ~ "Breguet",
    str_detect(`AC Type`, "Bristol") ~ "Bristol",
    str_detect(`AC Type`, "Unknown") ~ "Unknown",
    str_detect(`AC Type`, "Bleriot") ~ "Bleriot",
    str_detect(`AC Type`, "Caudron") ~ "Caudron",
    str_detect(`AC Type`, "Swallow") ~ "Swallow",
    str_detect(`AC Type`, "Douglas") ~ "Douglas",
    str_detect(`AC Type`, "Dornier") ~ "Dornier",
    str_detect(`AC Type`, "Kalinin") ~ "Kalinin",
    str_detect(`AC Type`, "Loening") ~ "Loening",
    str_detect(`AC Type`, "Heinkel") ~ "Heinkel",
    str_detect(`AC Type`, "Stinson") ~ "Stinson",
    str_detect(`AC Type`, "Wibault") ~ "Wibault",
    str_detect(`AC Type`, "Tupolev") ~ "Tupolev",
    str_detect(`AC Type`, "General") ~ "General",
    str_detect(`AC Type`, "Antonov") ~ "Antonov",
    str_detect(`AC Type`, "Grumman") ~ "Grumman",
    str_detect(`AC Type`, "Pilgrim") ~ "Pilgrim",
    str_detect(`AC Type`, "Lisunov") ~ "Lisunov",
    str_detect(`AC Type`, "Tempest") ~ "Tempest",
    str_detect(`AC Type`, "Convair") ~ "Convair",
    str_detect(`AC Type`, "Learjet") ~ "Learjet",
    str_detect(`AC Type`, "Pilatus") ~ "Pilatus",
    str_detect(`AC Type`, "Embraer") ~ "Embraer",
    str_detect(`AC Type`, "Kubicek") ~ "Kubicek",
    str_detect(`AC Type`, "Shaanxi") ~ "Shaanxi",
    str_detect(`AC Type`, "Wright") ~ "Wright",
    str_detect(`AC Type`, "Farman") ~ "Farman",
    str_detect(`AC Type`, "Fokker") ~ "Fokker",
    str_detect(`AC Type`, "Boeing") ~ "Boeing",
    str_detect(`AC Type`, "Wapiti") ~ "Wapiti",
    str_detect(`AC Type`, "Vultee") ~ "Vultee",
    str_detect(`AC Type`, "Martin") ~ "Martin",
    str_detect(`AC Type`, "Macchi") ~ "Macchi",
    str_detect(`AC Type`, "Siebel") ~ "Siebel",
    str_detect(`AC Type`, "Fairey") ~ "Fairey",
    str_detect(`AC Type`, "Hawker") ~ "Hawker",
    str_detect(`AC Type`, "SNCASE") ~ "SNCASE",
    str_detect(`AC Type`, "Cessna") ~ "Cessna",
    str_detect(`AC Type`, "Shorts") ~ "Shorts",
    str_detect(`AC Type`, "Airbus") ~ "Airbus",
    str_detect(`AC Type`, "Hughes") ~ "Hughes",
    str_detect(`AC Type`, "CH-53D") ~ "CH-53D",
    str_detect(`AC Type`, "MiG-23") ~ "MiG-23",
    str_detect(`AC Type`, "Harbin") ~ "Harbin",
    str_detect(`AC Type`, "Socata") ~ "Socata",
    str_detect(`AC Type`, "Sukhoi") ~ "Sukhoi",
    str_detect(`AC Type`, "Potez") ~ "Potez",
    str_detect(`AC Type`, "CMASA") ~ "CMASA",
    str_detect(`AC Type`, "Short") ~ "Short",
    str_detect(`AC Type`, "Arado") ~ "Arado",
    str_detect(`AC Type`, "Lasco") ~ "Lasco",
    str_detect(`AC Type`, "Bloch") ~ "Bloch",
    str_detect(`AC Type`, "R4D-5") ~ "R4D-5",
    str_detect(`AC Type`, "KB-50") ~ "KB-50",
    str_detect(`AC Type`, "Piper") ~ "Piper",
    str_detect(`AC Type`, "H-21B") ~ "H-21B",
    str_detect(`AC Type`, "Beech") ~ "Beech",
    str_detect(`AC Type`, "SNIAS") ~ "SNIAS",
    str_detect(`AC Type`, "Mi-17") ~ "Mi-17",
    str_detect(`AC Type`, "Mi-35") ~ "Mi-35",
    str_detect(`AC Type`, "Avro") ~ "Avro",
    str_detect(`AC Type`, "Spad") ~ "Spad",
    str_detect(`AC Type`, "Ryan") ~ "Ryan",
    str_detect(`AC Type`, "SPCA") ~ "SPCA",
    str_detect(`AC Type`, "Ford") ~ "Ford",
    str_detect(`AC Type`, "AEGK") ~ "AEGK",
    str_detect(`AC Type`, "Cams") ~ "Cams",
    str_detect(`AC Type`, "Saab") ~ "Saab",
    str_detect(`AC Type`, "Waco") ~ "Waco",
    str_detect(`AC Type`, "Budd") ~ "Budd",
    str_detect(`AC Type`, "Li-2") ~ "Li-2",
    str_detect(`AC Type`, "Fiat") ~ "Fiat",
    str_detect(`AC Type`, "CASA") ~ "CASA",
    str_detect(`AC Type`, "C-46") ~ "C-46",
    str_detect(`AC Type`, "Nord") ~ "Nord",
    str_detect(`AC Type`, "Avia") ~ "Avia",
    str_detect(`AC Type`, "Bell") ~ "Bell",
    str_detect(`AC Type`, "NAMC") ~ "NAMC",
    str_detect(`AC Type`, "Mi-6") ~ "Mi-6",
    str_detect(`AC Type`, "Mi-8") ~ "Mi-8",
    str_detect(`AC Type`, "HESA") ~ "HESA",
    str_detect(`AC Type`, "LVG") ~ "LVG",
    str_detect(`AC Type`, "GVF") ~ "GVF",
    str_detect(`AC Type`, "Let") ~ "Let",
    str_detect(`AC Type`, "VEB") ~ "VEB",
    str_detect(`AC Type`, "BAC") ~ "BAC",
    str_detect(`AC Type`, "IAI") ~ "IAI",
    str_detect(`AC Type`, "GAF") ~ "GAF",
    str_detect(`AC Type`, "Mil") ~ "Mil",
    str_detect(`AC Type`, "BAE") ~ "BAE",
    str_detect(`AC Type`, "ATR") ~ "ATR",
    str_detect(`AC Type`, "PAC") ~ "PAC",
    str_detect(`AC Type`, "M28") ~ "M28",
    str_detect(`AC Type`, "FD") ~ "FD",
    str_detect(`AC Type`, "DC") ~ "DC",
    str_detect(`AC Type`, "Li") ~ "Li",
    str_detect(`AC Type`, "AT") ~ "AT",
    str_detect(`AC Type`, "Mi") ~ "Mi",
    str_detect(`AC Type`, "KJ") ~ "KJ",
    str_detect(`AC Type`, "PA") ~ "PA",
    #Anything else
    TRUE~"Unknown"
  ))

grouped_AC_Type <- MainData %>% filter(AC_Type_Edited %nin% "Unknown") %>% group_by(AC_Type_Edited) %>% summarize(types = n(), Fatalities = sum(Fatalities, na.rm = TRUE))

counts <- grouped_AC_Type[order(-grouped_AC_Type$types),]

top_n <- counts[1:10,]

p <- ggplot(data = top_n, 
            mapping = aes(x = reorder(AC_Type_Edited, types), y = types, fill = Fatalities, na.rm = TRUE))

viz3 <- p + geom_col() +
        geom_text(aes(x = reorder(AC_Type_Edited, types), y = types, label = types), hjust = -0.3, size = 5) +
        coord_flip() + 
        theme(text = element_text(size = 17, face = "bold"), panel.grid.major = element_blank(), 
              plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank(), 
              panel.background = element_rect(fill = "transparent",colour = NA), plot.background =                    element_rect(fill = "transparent", colour = NA),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              legend.spacing.y = unit(0.8, "cm")) + 
        ggtitle("Frequent aircraft types" ) + 
        xlab("Aircraft type") + 
        ylab("Frequency") + 
        scale_fill_gradient(low = "pink", high = "dark red") + 
        guides(fill = guide_colourbar(barwidth = 1.0, barheight = 10))

viz3

#Visualization 5a#####

Viz5df <- MainData %>% separate(Location , c("City", "Country"), sep = ',')

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

Viz5df1 <- Viz5df1 %>%  mutate(Crahsebkt = case_when(Crashes < 50 ~ "< 50",
                               Crashes < 100 ~ "> 50 & < 100",
                               Crashes < 250 ~ "> 100 & < 250",  
                               Crashes >= 250 ~ "> 250"))

Viz5a <- ggplot(data = Viz5df1, 
                mapping = aes(x = long, y = lat, group = group))

Viz5a <- Viz5a + 
         geom_polygon(data = Viz5df1, 
                      mapping = aes(fill = factor(Crahsebkt, levels = c("< 50","> 50 & < 100","> 100 & < 250", "> 250" )))) + 
         scale_fill_manual(values = c("lightpink","indianred2","indianred3", "indianred4")) + labs(title = "Crashes by country", fill = "Crashes") + 
         theme_map() + 
         coord_sf(ylim = c(-50, 90), datum = NA) + 
         theme(text = element_text(size = 17, face = "bold"), 
                                   plot.title = element_text(hjust = 0.6), 
                                   plot.subtitle = element_text(hjust = 0.6),
                                   legend.title = element_text(size = 15),
                                   legend.text = element_text(size = 15))

Viz5a

#Visualization 5b#####

Viz5df$region[Viz5df$region %in% US$state] <- "usa"

Viz5df2 <- Viz5df %>% group_by(region) %>% summarise(Crashes = n(), Fatalities = sum(Fatalities))

Viz5df2 <- top_n(Viz5df2, 10, Crashes) %>% drop_na()

Viz5df2$region <- str_to_title(Viz5df2$region)

Viz5b <- ggplot(data = Viz5df2, 
                mapping = aes(x = reorder(region, Crashes, na.rm=TRUE), 
                              y = Crashes, fill = Fatalities)) + 
        coord_flip() + 
        geom_col() + 
        labs(x="Country", title = "Top ten countries with highest number of airplane crashes") + 
        scale_fill_gradient(low = "pink", high = "dark red") +
        geom_text(aes(label = Crashes), hjust = -0.3, size = 5) +
        theme(text = element_text(size = 19, face = "bold"), 
              panel.grid.major = element_blank(), 
              plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5),
              panel.grid.minor = element_blank(), 
              panel.background = element_rect(fill = "transparent",colour = NA), 
              plot.background = element_rect(fill = "transparent",colour = NA),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 18),
              legend.spacing.y = unit(0.5, "cm")) + 
         guides(fill = guide_colourbar(barwidth = 1.0, barheight = 10))

Viz5b

#Visualization 6#####

MainData$Summary <- iconv(MainData$Summary, "latin1", "ASCII", sub="")

MainData<- MainData %>%  mutate(
  CrashClass = case_when(
    #Main reasons - This part will override anything
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
    
    #Mechanical section
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
    
    #Human section#
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
    
    #Collision section#
    str_detect(tolower(Summary), "collision") |
    str_detect(tolower(Summary), "power lines") |
    str_detect(tolower(Summary), "power cables") | 
    str_detect(tolower(Summary), "struck") |
    str_detect(tolower(Summary), "hit") | 
    str_detect(tolower(Summary), "clipped") |
    str_detect(tolower(Summary), "collided")  
    ~ "Collision",
    
    #Do at last#
    str_detect(tolower(Summary), "fire") ~ "Mechanical",
    str_detect(tolower(Summary), "runway") ~ "Human",
    str_detect(Summary, "IFR ") ~ "Human",
    str_detect(Summary, "VFR ") ~ "Human", 
    str_detect(tolower(Summary), "improper") ~ "Human",
    
    #Anything else#
    TRUE~"Unknown"
  )
)

color_palette <- c("#EF6621", "#186B39", "#99004C", "#2E88B6", "#A0A0A0", "#193E54")

summary_crash_class <- MainData %>% group_by(CrashClass) %>% summarise(count = n())

group <- paste("Group", 1:6)

subgroup <- summary_crash_class$CrashClass

percentage_values <- ((summary_crash_class$count)/5009)*100

percentage_values <- format(round(percentage_values, 2), nsmall = 2)

summary_crash_class$percentage_values <- percentage_values

percentages <- summary_crash_class$percentage_values

value <- summary_crash_class$count

df <- data.frame(group, subgroup, value) 

viz6 <- ggplot(df, aes(area = value, fill = group, label = paste(subgroup, value, sep = "\n"))) + 
        geom_treemap() + 
        geom_treemap_text(colour = "white", place = "centre", size = 20) + 
        theme(legend.position = "none") + 
        scale_fill_manual(values = color_palette) +
        labs(title = "Crashes by most common reason") +
        theme(text = element_text(size = 17, face = "bold"), 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

viz6

#Visualization 7a#####

Viz7df <- MainData %>%  group_by(Operator) %>% summarise(Crashes = n(), Fatalities = sum(Fatalities))

Viz7df <- top_n(Viz7df, 10, Crashes) %>% drop_na()

Viz7a <- ggplot(data=Viz7df, 
                mapping = aes(x = reorder(Operator, Crashes), 
                              y = Crashes, fill = Fatalities)) + 
                geom_col() + 
                geom_text(aes(x = reorder(Operator, Crashes), y = Crashes, label = Crashes), hjust = -0.3, size = 5) + 
                coord_flip() + 
                scale_fill_gradient(low = "pink", high = "dark red") +
                labs(title = "Top 10 operators involved in crashes", x = "Operator") +
                ylim(0, 300) + 
                theme(text = element_text(size = 19, face = "bold"), 
                panel.grid.major = element_blank(), 
                plot.title = element_text(hjust = 0.4),
                panel.grid.minor = element_blank(), 
                panel.background = element_rect(fill = "transparent",colour = NA), 
                plot.background = element_rect(fill = "transparent",colour = NA),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                legend.spacing.y = unit(0.5, "cm")) + 
         guides(fill = guide_colourbar(barwidth = 1.0, barheight = 10))

Viz7a

#Visualization 7b#####

Viz7df <- Viz7df[order(-Viz7df$Crashes),]

unique(Viz7df$Operator)

Viz7b <- ggplot(data=subset(MainData, 
                            MainData$Operator %in% unique(Viz7df$Operator)), 
                            mapping = aes(x = reorder(Operator, Fatalities, na.rm = TRUE, FUN = median),
                                          y = Fatalities)) +
         geom_boxplot(aes(fill=factor(Operator))) + 
         coord_flip() + 
         labs(title = "Fatalities distribution of top 10 most frequent operators involved in crashes", x = "Operator") +
         guides(fill = "none") +
         ylim(0, 300) + 
         theme(text = element_text(size = 19, face = "bold"), 
               panel.grid.major = element_blank(), 
               plot.title = element_text(hjust = -0.65), 
               plot.subtitle = element_text(hjust = -0.65),
               panel.grid.minor = element_blank(), 
               panel.background = element_rect(fill = "transparent", colour = NA), 
               plot.background = element_rect(fill = "transparent", colour = NA))
         

Viz7b

#Visualization 8#####

MainData <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908_final.csv")

MainData$Summary <- iconv(MainData$Summary, "latin1", "ASCII", sub = "")

MainData<- MainData %>% mutate(Flight_Stage = case_when(
  #Take-off Flight Stage
  str_detect(tolower(Summary), "take off") ~ "takeoff",
  str_detect(tolower(Summary), "taking off") ~ "takeoff",
  str_detect(tolower(Summary), "off") ~ "takeoff",
  str_detect(tolower(Summary), "took off") ~ "takeoff",
  str_detect(tolower(Summary), "taken off") ~ "takeoff",
  
  #Landing Flight Stage
  str_detect(tolower(Summary), "landing") ~ "landing",
  str_detect(tolower(Summary), "land") ~ "landing",
  str_detect(tolower(Summary), "attempting to land") ~ "landing",
  
  #In-the-air Flight Stage
  TRUE~"intheair"
)
)

stage_grouping <- MainData %>% group_by(Flight_Stage) %>% summarise(Stages = n(), Fatalities = sum(Fatalities, na.rm = TRUE))

viz_8 <- ggplot(data = stage_grouping, aes(x = reorder(Flight_Stage, -Stages), y = Stages, fill = Fatalities)) + 
         geom_col() + 
         labs(x = "Flight stage", y = "Crashes") + 
         scale_fill_gradient(low = "pink", high = "dark red") + 
         ggtitle("Flight stage of crashes") + 
         theme(text = element_text(size = 17, face = "bold"), panel.grid.major = element_blank(), 
               plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank(), 
               panel.background = element_rect(fill = "transparent",colour = NA), 
               plot.background = element_rect(fill = "transparent", colour = NA),
               legend.position = "none")

viz_8
