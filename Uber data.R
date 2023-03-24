getwd()
setwd("C:/Users/user/Downloads/Documents/R projects/Uber data/R Uber Trips Project")
April<-read.csv("C:/Users/user/Downloads/Documents/R projects/Uber data/R Uber Trips Project/uber-raw-data-apr14.csv")
May<-read.csv("C:/Users/user/Downloads/Documents/R projects/Uber data/R Uber Trips Project/uber-raw-data-may14.csv")
June<-read.csv("C:/Users/user/Downloads/Documents/R projects/Uber data/R Uber Trips Project/uber-raw-data-jun14.csv")
July<-read.csv("C:/Users/user/Downloads/Documents/R projects/Uber data/R Uber Trips Project/uber-raw-data-jul14.csv")
August<-read.csv("C:/Users/user/Downloads/Documents/R projects/Uber data/R Uber Trips Project/uber-raw-data-aug14.csv")
September<-read.csv("C:/Users/user/Downloads/Documents/R projects/Uber data/R Uber Trips Project/uber-raw-data-sep14.csv")
Combined_data<-rbind(April,May,June,July,August,September)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("DT")
library(DT)
install.packages("scales")
library(scales)
install.packages("ggthemes")
library(ggthemes)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
install.packages("dplyr")
dim(Combined_data)
view(Combined_data)
head(Combined_data)
colors<-c("#CC1011","#665555","#05a399","#cfcaca","#f5e840","#0683c9","#e075b0")
Combined_data$Date.Time<-as.POSIXct(Combined_data$Date.Time,format="%m/%d/%Y %H:%M:%S")
Combined_data$Time<-format(as.POSIXct(Combined_data$Date.Time,format="%m/%d/%Y %H:%M:%S"),format="%H:%M:%S")
view(Combined_data)
Combined_data$Day<-factor(day(Combined_data$Date.Time))
Combined_data$Month<-factor(month(Combined_data$Date.Time,label=TRUE))
Combined_data$Year<-factor(year(Combined_data$Date.Time))
Combined_data$Day_of_Week<-factor(wday(Combined_data$Date.Time,label = TRUE))
Combined_data$Date.Time<-ymd_hms(Combined_data$Date.Time)
Combined_data$second = factor(second(hms(Combined_data$Time)))
Combined_data$minute = factor(minute(hms(Combined_data$Time)))
Combined_data$hour = factor(hour(hms(Combined_data$Time)))
head(Combined_data)
hourly_data <- Combined_data %>% group_by(hour) %>% dplyr::summarize(Total = n())
head(hourly_data)
datatable(hourly_data)
ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="red") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
month_hour_data <- Combined_data %>% group_by(Month, hour) %>%  dplyr::summarize(Total = n())
head(month_hour_data )
ggplot(month_hour_data, aes(hour, Total, fill=Month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)
day_data <- Combined_data %>% group_by(Day) %>% dplyr::summarize(Trips = n())
ggplot(day_data, aes(Day, Trips)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)
day_month_data <-Combined_data %>% group_by(Day_Of_Week, Month) %>% dplyr::summarize(Trips = n())
day_month_data
ggplot(day_month_data, aes(Day_of_Week, Trips, fill = Month)) + 
  geom_bar(stat = "identity",fill= "orange") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)
#Number of Trips place during months in a year
month_data <- Combined_data %>% group_by(Month) %>% dplyr::summarize(Total = n())

month_data
ggplot(month_data, aes(Month, Total, fill = Month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)