getwd()
setwd("C:/R/knapweed")
dir<-getwd()
list.files(dir)

###############################################################################

data <- read.csv("C:/R/knapweed/data/2016_seed head data.csv", header=T)
#data <- na.omit(data)

#omit variables: ..."LooseSeeds_black", "LooseSeeds_other", "heads_NO", 
#"Chae_acr", "Larinus", "Metz_pau", "Urop_aff", "Urop_qua", etc...

data <- data[c("site", "sampleNO", "transect", "meter",
               "Seedhead1_black", "Seedhead1_other", "Seedhead2_black", "Seedhead2_other", "Seedhead3_black",
               "Seedhead3_other", "Seedhead4_black", "Seedhead4_other", "Seedhead5_black", "Seedhead5_other",
               "Seedhead1_Chae_acr", "Seedhead1_Larinus", "Seedhead1_Metz_pau", "Seedhead1_Urop_aff", "Seedhead1_Urop_qua",
               "Seedhead2_Chae_acr", "Seedhead2_Larinus", "Seedhead2_Metz_pau", "Seedhead2_Urop_aff", "Seedhead2_Urop_qua",
               "Seedhead3_Chae_acr", "Seedhead3_Larinus", "Seedhead3_Metz_pau", "Seedhead3_Urop_aff", "Seedhead3_Urop_qua",
               "Seedhead4_Chae_acr", "Seedhead4_Larinus", "Seedhead4_Metz_pau", "Seedhead4_Urop_aff", "Seedhead4_Urop_qua",
               "Seedhead5_Chae_acr", "Seedhead5_Larinus", "Seedhead5_Metz_pau", "Seedhead5_Urop_aff", "Seedhead5_Urop_qua")]

library(reshape)
md <-melt(data, id=c("site", "transect", "meter", "sampleNO"))

md$seedhead <- strtrim(md$variable, 9)
md$var <- substr(md$variable, start = 11, stop = 20)
md$seedhead <- substr(md$seedhead, start = 9, stop = 20)
df1 <- cast(md, site + transect + meter + sampleNO + seedhead ~ var)

df1$siteID[df1$site == "blue"] <- 1
df1$siteID[df1$site == "road"] <- 2
df1$siteID[df1$site == "hill"] <- 3

df1$sampleID <- 1:nrow(df1)
df <- df1[, c(13, 14, 1, 4, 2, 3, 5, 6, 10, 7, 8, 9, 11, 12)]
names(df)

write.table(df, "C:/R/knapweed/data/2016_seedhead.csv", sep = ",", row.names=F)

############################# END ###########################################################