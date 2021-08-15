library(tidyverse)
library(lubridate)

setwd("C:/Users/ShibaprasadB/Desktop/Desktop")

scrobbles <- read.csv('nameofthefile.csv', header = F)

names(scrobbles) <- c('artist', 'album', 'song', 'datetime')

scrobbles$datetime<-dmy_hm(scrobbles$datetime)

scrobbles$datetime <- with_tz(scrobbles$datetime, tzone = "")

scrobbles<-scrobbles %>% 
  separate(datetime, into = c('date', 'time'), sep=' ', remove = FALSE)


scrobbles<-scrobbles[(scrobbles$datetime >= "2020-06-01" & scrobbles$datetime < "2021-06-01"),]

ggplot(data=scrobbles %>%
         group_by(date) %>%
         summarise(count=n()), aes(x=count)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(count)),
             color="red", linetype="dashed", size=1)+
  theme_bw()+
  ggtitle('Distribution of number of songs per day')+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

scrobbles$date<-as.POSIXct(scrobbles$date)
scrobbles$month<-months(scrobbles$date)

ggplot(data=scrobbles %>%
         group_by(month) %>%
         summarise(count=n()), aes(x=count)) + 
  geom_density(color="darkgreen", fill="lightgreen")+
  geom_vline(aes(xintercept=mean(count)),
             color="darkblue", linetype="dashed", size=1)+
  theme_bw()+
  ggtitle('Distribution of number of songs per month')+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


ggplot(data=scrobbles %>%
         group_by(month) %>%
         summarise(count=n()))+
  geom_bar(aes(x=reorder(month, -count), y=count, fill=month), stat = 'identity')+
  theme_bw()+
  theme(legend.position = 'none')+
  xlab('Month')+
  ylab('Number of songs listened')

scrobbles %>%
  mutate(day_night=ifelse((hour(datetime)>=5 | hour(datetime) <=17), 'Day', 'Night'))

scrobbles$day_night <- ifelse(hour(scrobbles$datetime) <= 5 | 17 <=hour(scrobbles$datetime), "Night","Day" )

viridis::inferno(20)

theme_set(theme_minimal())

ggplot(data=scrobbles %>%
         group_by(day_night, month) %>%
         summarise(count=n()))+
  scale_fill_manual(values = c('seagreen','darkblue'))+
  geom_bar(aes(x=month, y=count, fill=day_night, width=0.5), stat ='identity', position = 'fill')+
  theme(legend.title = element_blank(),
        legend.position = 'top')+
  xlab('Month')+
  ylab('Proportion of songs')



ggplot(data=scrobbles %>%
  group_by(date, day_night) %>%
  summarise(count=n()))+
  geom_boxplot(aes(y=count, x=day_night, fill=day_night))+
  theme_bw()+
  ylab('Count')+
  xlab('Time of the day')+
  theme(legend.position = c(0.9,0.9),
        legend.box.background = element_rect(linetype="solid"),
        legend.title =element_blank())


