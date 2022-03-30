library(plyr)
library(dplyr)
library(ggbiplot)
library(readr)
library(tidyr)
library(pheatmap)


##MSQA
##Loading data
results <- read.csv("Results.csv")
sites <- read.csv("Sites.csv")
#Making Median 
median_results <- results %>%
  group_by(SITE_NO, PARM_NM) %>%
  summarize(median = median(RESULT_VA)) %>%
  ungroup()
##Making short dataframe 
short <- pivot_wider(median_results, id_cols = SITE_NO, names_from = PARM_NM, values_from = median) 
##Seeing how many NA's
colSums(is.na(Short_data))
##Selecing columns
dataframeout <- as.data.frame(short[,colSums(is.na(short)) < 4])
##Making rownames be site_no
rownames(dataframeout) <- dataframeout$SITE_NO
##taking out final NA's
finaldata <- dataframeout[complete.cases(dataframeout),]
##metadata
metadata <- sites[match(rownames(finaldata),sites$SITE_NO),]
prcompData <- prcomp(finaldata, center = T, scale. = T)
#PCA of just MSQA
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata$RSQA_STUDY), ellipse = T)
#PCA with axes
ggbiplot(prcompData, choices = c(1,2), var.axes = T, groups = paste(metadata$RSQA_STUDY), ellipse = T)
##PCA with Oregon and Washington 
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata$STATE_ABBREV), ellipse = T)

