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
short <- pivot_wider(median_results, id_cols = SITE_NO, names_from = PARM_NM, values_from = median) 
#this sees how many NA's there are 
colSums(is.na(short))
