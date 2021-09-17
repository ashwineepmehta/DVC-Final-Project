# R Templates - Master File #######

## Libraries ####
library(ggplot2)
library(socviz)
library(tidyverse)
library(dplyr)

## Import Data & Manipulate data####
Application_Records <- read_csv("data/Credit Card Approval Prediction/application_record.csv") # import Application data
App.Credit.all <- merge(Application_Records,Credit_Records,by="ID") # Join Application and Credit data into one single DataFram
App.Credit.all <- App.Credit.all %>%
  mutate(STATUS = case_when(
    STATUS == 0 ~ "1-29 days past due",
    STATUS == 1 ~ "30-59 days past due",
    STATUS == 2 ~ "60-89 days overdue",
    STATUS == 3 ~ "90-119 days overdue",
    STATUS == 4 ~ "120-149 days overdue",
    STATUS == 5 ~ "Overdue or bad debts, write-offs for more than 150 days",
    STATUS == "C" ~ "paid off that month",
    STATUS == "X" ~ "No loan for the month",
    TRUE ~ "OTHER"
  ))

App.Credit.all.reduced = subset(App.Credit.all, select = -c(CNT_CHILDREN,ID, AMT_INCOME_TOTAL,DAYS_BIRTH, DAYS_EMPLOYED,FLAG_MOBIL,FLAG_WORK_PHONE, FLAG_PHONE,FLAG_EMAIL,CNT_FAM_MEMBERS  , MONTHS_BALANCE       ) )
App.Credit.all.reduced <- drop_na(App.Credit.all.reduced)

## Describe Data #####
help(mpg)
names(mpg)
summary(mpg$cty)
summary(mpg$hwy)
head(mpg$cty)
head(mpg$hwy)
dim(mpg)
str(mpg$cty)
str(mpg$hwy)
class(mpg)
unique(mpg$hwy)

## ggplot #####

### Data and Mapping ####
ggplot(data=mpg, mapping=aes(
      X=
        
))

### Geoms ####
Graphical primitives:
  geom_blank(): display nothing. Most useful for adjusting axes limits using data.
  geom_point(): points.
  geom_path(): paths.
  geom_ribbon(): ribbons, a path with vertical thickness.
  geom_segment(): a line segment, specified by start and end position.
  geom_rect(): rectangles.
  geom_polyon(): filled polygons.
  geom_text(): text.
One variable:
  Discrete:
    geom_bar(): display distribution of discrete variable.
  Continuous
    geom_histogram(): bin and count continuous variable, display with bars.
    geom_density(): smoothed density estimate
    geom_dotplot(): stack individual points into a dot plot.
    geom_freqpoly(): bin and count continuous variable, display with lines.
Two variables:
  Both continuous:
    geom_point(): scatterplot.
    geom_quantile(): smoothed quantile regression.
    geom_rug(): marginal rug plots.
    geom_smooth(): smoothed line of best fit.
    geom_text(): text labels.
Show distribution:
  geom_bin2d(): bin into rectangles and count.
  geom_density2d(): smoothed 2d density estimate.
  geom_hex(): bin into hexagons and count.
At least one discrete:
  geom_count(): count number of point at distinct locations
  geom_jitter(): randomly jitter overlapping points.
One continuous, one discrete:
  geom_bar(stat = "identity"): a bar chart of precomputed summaries
  geom_boxplot(): boxplots.
  geom_dotplot(): carefully adjust location of overlapping points.
  geom_violin(): show density of values in each group.
One time, one continuous
  geom_area(): area plot.
  geom_line(): line plot.
  geom_step(): step plot.
Display error:
  geom_crossbar(): vertical bar with center.
  geom_errorbar(): error bars.
  geom_linerange(): vertical line.
  geom_pointrange(): vertical line with center.
Spatial
  geom_map(): fast version of geom_polygon() for map data.
Three variables:
  geom_contour(): contours.
  geom_tile(): tile the plane with rectangles.
  geom_raster(): fast version of geom_tile() for equal sized tiles.

