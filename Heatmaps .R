library(plyr)
library(dplyr)
library(ggbiplot)
library(readr)
library(tidyr)
library(pheatmap)

results <- read.csv("Results.csv")
sites <- read.csv("Sites.csv")

results$RESULT_VA <- ifelse(results$REMARK_CD== "<",0,as.numeric(results$RESULT_VA))

median_value <- results %>%
  group_by(PARM_NM, SITE_NO) %>%
  summarize(median = median(RESULT_VA))

short <- pivot_wider(median_value, id_cols = SITE_NO, names_from = PARM_NM, values_from = median)

colSums(is.na(short))

dataframeout <- as.data.frame(short[,colSums(is.na(short)) < 4])

rownames(dataframeout) <- dataframeout$SITE_NO
finaldata <- dataframeout[complete.cases(dataframeout),]
metadata <- sites[match(rownames(finaldata),sites$SITE_NO),]

data_cor <- finaldata%>%cor()%>%as.data.frame()
pheatmap(data_cor, annotations = rownames(data_cor), show_rownames = T, show_colnames = T)
data_cor <- cor(t(finaldata))
#Heatmap of Heavy metals 
pheatmap(data_cor, show_rownames = T, show_colnames = T)
prcompData <- prcomp(finaldata, center = T, scale. = T)
#PCA of just PNSQA
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata$RSQA_STUDY), ellipse = T)
#PCA with axes
ggbiplot(prcompData, choices = c(1,2), var.axes = T, groups = paste(metadata$RSQA_STUDY), ellipse = T)
##PCA with Oregon and Washington 
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata$STATE_ABBREV), ellipse = T)
PCA <- ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata$DEC_LAT_VA), ellipse = FALSE)
PCA <- PCA + theme(legend.title = element_blank())

##Heat map for biodata
# Correct coding 
datalong <-pivot_longer(data=biodata, cols=15:674, names_to = "variables", values_to = "val", 
                        values_transform = list(val = as.numeric), values_drop_na = TRUE)
View(datalong)
#remove NA's in its own line, and group by SITE_NO and variables 
newdatalong <- na.omit(datalong)
View(newdatalong)

Bio_median_value <- newdatalong %>%
  group_by(SITE_NO, variables) %>%
  summarize(median = median(val))
#Converting data back to short 
shortbio <- pivot_wider(Bio_median_value, id_cols = SITE_NO, names_from = variables, values_from = median)
View(shortbio)
#deleting NA
colSums(is.na(shortbio))

#Choosing columns 
#Code from Simone but not used 
datatrimmed <- as.data.frame(shortbio[,c(colSums(is.na(shortbio)) < 100 & colSums(is.na(shortbio)) != 0)])
nrow(shortbio)

finalbiodata <- shortbio[complete.cases(shortbio), ]
View(finalbiodata)

sites <- read.csv("Sites.csv")
metadatabio <- sites[match(rownames(finalbiodata),sites$SITE_NO),]

newfinaldata <- length(unique(finalbiodata))
View(newfinaldata)

newfinalbio <- finalbiodata[, ! apply(finalbiodata , 2 , function(x) sd(x, na.rm = TRUE)==0 ) ]
Fdata <- newfinaldata(select(-sites))
data_cor <- cor(newfinalbio)

map <- pheatmap(data_cor, annotations=rownames(data_cor),show_rownames=F, show_colnames=F)

prcompData <- prcomp(newfinalbio, center=T, scale=T)
fdata <- newfinaldata%>%select(rownaname_to_column()) 

plot(map$tree_row)
abline(h=14.75, col="red", lty=2, lwd=2)
bio_map <- as.data.frame(sort(cutree(map$tree_row, h=14.75)))
setDT(bio_map, keep.rownames = TRUE)
##pheatmap for biodata
setDT(bio_map, keep.rownames = TRUE)[]
colnames(bio_map)[1] <- "chemicals"
colnames(bio_map)[2] <- "modules"
module2 <- as.data.frame(bio_map[bio_map$modules == 2,])
data_cor1 <- as.data.frame(data_cor)
data_cortrim <- data_cor1[rownames(data_cor) %in% module2$chemicals, colnames(data_cor1) %in% module2$chemicals]
dev.off()
quartz()
bioheatmap <- pheatmap(data_cortrim, annotations=rownames(data_cortrim),show_rownames=T, show_colnames=T, fontsize = 5, fontsize_col = 3)

library(data.table)


##Merging data
results <- read.csv("Results.csv")
sites <- ("Sites.csv")
biodata <- read.csv("National Environ n Eco Metrics combined_v6.csv")

##Making short database 
Results_median <- results %>%
  group_by(SITE_NO, PARM_NM)  %>%
  summarize(median_val = median(RESULT_VA)) %>%
  ungroup()
data <- pivot_wider(Results_median, id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val)

dataframeout <- as.data.frame(data[,colSums(is.na(data)) < 4])
rownames(dataframeout) <- dataframeout$SITE_NO
table(colSums(is.na(data)))
finaldata <- dataframeout[complete.cases(dataframeout), ]
metadata <- sites[match(rownames(finaldata),sites$SITE_NO),]
final_data <- subset(finaldata, select = -c(SITE_NO))
newfinaldata <- final_data[, ! apply(final_data , 2 , function(x) sd(x, na.rm = TRUE)==0 ) ]
rownames(final_data) <- paste0("T", rownames(final_data))
pheatmap(log(newfinaldata))

## Adding metadata 
metadata$TSITE_QW <- paste0("T", metadata$SITE_NO)
mergedmetadata <- merge(metadata, biodata, by="TSITE_QW")
View(mergedmetadata)
metadata1 <- mergedmetadata[match(rownames(final_data),mergedmetadata$TSITE_QW),]

##Merged heat map and PCA 
prcompData <- prcomp(final_data, center = T, scale. = T)
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata1$RSQA_STUDY), ellipse = T)

ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata1$pSC_rich), ellipse = T)
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata1$pFC_rich), ellipse = T)

ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata2$group_tags), ellipse = T)
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata1$A_MMI))
ggbiplot(prcompData, choices = c(1,2), var.axes = F, groups = paste(metadata1$FMMI))

data_cor <- cor(metadata1)
data_cor <- cor(finalbiodata)
pheatmap(data_cor, annotations=rownames(data_cor),show_rownames=T, show_colnames=T)

pheatmap(data_cor, annotations=rownames(data_cor),show_rownames=T, show_colnames=T)

