getwd()
setwd("C:/R/knapweed/data")
dir<-getwd()
list.files(dir)

########################################################################################
### Create seedhead data frame ###

dat1 <- read.csv("C:/R/knapweed/data/2016_seedhead.csv", header=T)
#missing <- dat1[!complete.cases(dat1),] # find the rows with incomplete entries
seedhead <- na.omit(dat1) # remove rows with NA

### Add the variable 'total' which is the sum of the 5 insects ###
seedhead$total <- seedhead$Chae_acr + seedhead$Larinus + 
  seedhead$Metz_pau + seedhead$Urop_aff + seedhead$Urop_qua

#write.table(seedhead, "C:/R/knapweed/data/seedhead.csv", sep = ",", row.names=F)

########################################################################################
### Create vegetation / soil seed data frame ###

dat2 <- read.csv("C:/R/knapweed/data/2016_seedsoil&veg.csv", header=T, na.strings="NA")
#missing<-dat2[!complete.cases(dat2),] # find the rows with incomplete entries
vegsoil <- na.omit(dat2) # remove rows with NA

### Convert the seed count to m2 from sample 5 cm high x 8 cm diameter ###
vegsoil$bank_m2 <- vegsoil$seedNO * 40
vegsoil$germ_m2 <- vegsoil$germNO * 40
vegsoil$rupt_m2 <- vegsoil$ruptNO * 40

### Add the variable 'viable' which is the % viable seeds remaining in seed bank ###
vegsoil$viable <- vegsoil$germ_m2 + vegsoil$rupt_m2

### Convert the vegetation count to m2 from quadrat 20 cm wide x 50 cm long ###
vegsoil$bolt_m2 <- vegsoil$boltNO * 10
vegsoil$stem_m2 <- vegsoil$stemNO * 10
vegsoil$head_m2 <- vegsoil$headNO * 10
vegsoil$rose_m2 <- vegsoil$roseNO * 10
vegsoil$ling_m2 <- vegsoil$lingNO * 10

#write.table(vegsoil, "C:/R/knapweed/data/vegsoil.csv", sep = ",", row.names=F)

########################################################################################
### Create data frames with the vegsoil and seedhead matrices combined ###
### Based on either the means or the sums ###

#### Create unique identifiers common in the 'seedhead' and 'vegsoil' matrices ###
vegsoil$ID <- paste(vegsoil$site, vegsoil$transect, vegsoil$meter)
seedhead$ID <- paste(seedhead$site, seedhead$transect, seedhead$meter)
library(plyr) # for the 'rename' function

#### Create data frame aggregated by means based on the 'seedhead' and 'vegsoil' matrices
dat3<-merge(seedhead, vegsoil, by.x = "ID", by.y="ID", all = F)
df1 <- aggregate(. ~ ID, dat3, mean)
df1 <- df1[,c(1:16, 31:39)]
df1 <- rename(df1, c("site.x" = "site", "transect.x" = "transect", "meter.x" = "meter"))
names(df1)

#write.table(df1, "C:/R/knapweed/data/df1.csv", sep = ",", row.names=F)

#### Create data frame aggregated by sum based on the 'seedhead' and 'vegsoil' matrices
dat4<-merge(seedhead, vegsoil, by.x = "ID", by.y="ID", all = F)
df2 <- aggregate(. ~ ID, dat4, sum)
df2 <- df2[,c(1:16, 31:39)]
df2 <- rename(df2, c("site.x" = "site", "transect.x" = "transect", "meter.x" = "meter"))
names(df2)

#write.table(df2, "C:/R/knapweed/data/df2.csv", sep = ",", row.names=F)

############################# END #######################################################