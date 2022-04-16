#library 
library(tidyr)
library(plyr)
library(readr)
library(dplyr)
library(ggplot2)

metaldata <- read.csv("Results.csv")
median_value <- metaldata %>%
  group_by(PARM_NM, SITE_NO) %>%
  summarize(median = median(RESULT_VA))

median_value_2<- metaldata %>%
  group_by(PARM_NM) %>%
  summarize(median = median(RESULT_VA))

ggplot(median_value_2, aes(x=PARM_NM, y=median)) + 
  geom_point() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0)) + 
  ggtitle("PNSQA") + 
  labs(y = "Concentration", x = "Metal")

median <- median_value_2[-c(1, 17, 38), ]

ggplot(median, aes(x=PARM_NM,y=median)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0))

##selecting variables 
selectdata <- median_value_2[c(5, 30, 43, 37, 11, 39, 6, 10, 19), ]

ggplot(selectdata, aes(x=PARM_NM,y=median)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0))

ggplot(median_value, aes(x=PARM_NM,y=median, colour = SITE_NO)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0))

median_county <- metaldata %>%
  group_by(PARM_NM, SITE_NO, COUNTY_NM) %>%
  summarize(median = median(RESULT_VA))

ggplot(median_county, aes(x=PARM_NM,y=median, colour = COUNTY_NM)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0))

##MSQA 
metaldata <- read.csv("Results.csv")
median_value <- metaldata %>%
  group_by(PARM_NM, SITE_NO) %>%
  summarize(median = median(RESULT_VA))

median_value_2<- metaldata %>%
  group_by(PARM_NM) %>%
  summarize(median = median(RESULT_VA))

ggplot(median_value_2, aes(x=PARM_NM, y=median)) + 
  geom_point() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0)) + 
  ggtitle("MSQA") + 
  labs(y = "Concentration", x = "Metal")

median <- median_value_2[-c(1, 16, 38), ]

ggplot(median, aes(x=PARM_NM,y=median)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0)) +
  ggtitle("MSQA") + 
  labs(y = "Concentration", x = "Metal")

##By state
ggplot(median_value, aes(x=PARM_NM,y=median, colour = SITE_NO)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0))

median_state <- metaldata %>%
  group_by(PARM_NM, SITE_NO, STATE_NM) %>%
  summarize(median = median(RESULT_VA))

ggplot(median_state, aes(x=PARM_NM,y=median, colour = STATE_NM)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0))



#SESQA
metaldata <- read.csv("Results.csv")
median_value <- metaldata %>%
  group_by(PARM_NM, SITE_NO) %>%
  summarize(median = median(RESULT_VA))

meanvalue <- metaldata %>%
  group_by(PARM_NM) %>%
  summarize(mean = mean(RESULT_VA))

ggplot(meanvalue, aes(x=PARM_NM, y=mean)) + 
  geom_point() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0)) + 
  ggtitle("SESQA") + 
  labs(y = "Concentration", x = "Metal")

##By state
ggplot(median_value, aes(x=PARM_NM,y=median, colour = SITE_NO)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0))

median_state <- metaldata %>%
  group_by(PARM_NM, SITE_NO, STATE_NM) %>%
  summarize(median = median(RESULT_VA))

ggplot(median_state, aes(x=PARM_NM,y=median, colour = STATE_NM)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0))




##Heavy Metal Code
metals <- read.csv("metals.csv")

long <- metals %>%
  pivot_longer(!Location, names_to = "Metal", values_to = "Concentration")
  
ggplot(long, aes(x=Location, y=Concentration, fill = Metal, colour = Metal)) + 
  geom_point() + 
  theme_bw()

ggplot(long, aes(x=Location, y= Concentration, colour = Metal)) + 
  geom_boxplot() + 
  scale_y_log10() +
  theme_classic()
