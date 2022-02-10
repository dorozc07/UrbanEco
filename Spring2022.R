library(plyr)
library(dplyr)
library(ggbiplot)
library(readr)
library(tidyr)

##Preparing Data
results <- read.csv("Results.csv")
sites <- read.csv("Sites.csv")
median_results <- results %>%
  group_by(SITE_NO, PARM_NM) %>%
  summarize(median = median(RESULT_VA)) %>%
  ungroup()
Short_data <- pivot_wider(median_results, id_cols = SITE_NO, names_from = PARM_NM, values_from = median) 
#this sees how many NA's there are 
colSums(is.na(Short_data))
#Selecting columns without NA's 
Interestingcolumns_data<-as.data.frame(Short_data[,c(2,7,8,9,11,12,16,17)])
rownames(Interestingcolumns_data)<-Short_data$SITE_NO
#Removing rows (sites) with NA's 
finaldata <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]
metadata <- sites[match(rownames(finaldata), sites$SITE_NO),]
##Code for PCA
prcompData <- prcomp(finaldata, center = T, scale. = T)
summary(prcompData)
ggscreeplot(prcompData, type = "pev")
ggscreeplot(prcompData, type = "cev")
ggbiplot(prcompData, choices = c(1,2), var.axes = F)
#grouping by RSQA_STUDY for each data point
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata$RSQA_STUDY), ellipse = T)
#Variable axes
ggbiplot(prcompData, choices = c(1,2), var.axes = T, groups = paste(metadata$RSQA_STUDY)) + theme_classic()
#compare by state groups 
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata$STATE_ABBREV), ellipse = T)
