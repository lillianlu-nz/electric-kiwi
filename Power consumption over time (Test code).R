library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)

library(readr)
EK <- read_csv("C:/Users/Momo/Downloads/EK.csv", 
                               col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                `Time of Day` = col_time(format = "%H")))
View(EK)

datetime <- as.POSIXct(paste(Consumption$Date,Consumption$`Time of Day`),format="%Y-%m-%d %H:%M:%S")

Consumption$datetime<-datetime

class(Consumption$`Time of Day`)

#use ggplot to generate plot#
p<- ggplot(Consumption, aes(x=datetime,y=`Per person kWh`))+
  geom_line(color='#1a98c9',size=1)+
  geom_point()
p

l<- ggplot(Consumption, aes(x=,y=`Per person kWh`))+
  geom_line(color='#1a98c9',size=1)+
  geom_point()
l+ labs(x='Date',y='Power Consumption(kWh)')



# use pipline to generate plot#
Consumption %>% 
  # rename(pp_kwh = `Per person kWh) %>% 
  ggplot(aes(x=datetime,y=`Per person kWh`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=20)) 
# +
#   scale_x_continuous(n.breaks =5)
