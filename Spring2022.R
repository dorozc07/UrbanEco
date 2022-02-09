library(plyr)
library(dplyr)
library(ggbiplot)
library(readr)
library(tidyr)

results <- read.csv("Results.csv")
sites <- read.csv("Sites.csv")
median_results <- results %>%
  group_by(PARM_NM, SITE_NO) %>%
  summarize(median = median(RESULT_VA))
Short_data <- pivot_wider(median_results, id_cols = SITE_NO, names_from = PARM_NM, values_from = median) 
#this sees how many NA's there are 
colSums(is.na(short))
#Selecting columns without NA's 
Interestingcolumns_data<-as.data.frame(Short_data[,c(2,7,8,9,11,12,16,17)])
rownames(Interestingcolumns_data)<-Short_data$SITE_NO
#Removing rows (sites) with NA's 
finaldata <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]
