Venn Diagram

##############################################################################

dat2 <- read.csv("C:/R/knapweed/data/2016_seedhead.csv", header=T)
#missing <- dat2[!complete.cases(dat2),]
seedhead <- na.omit(dat2)

seedhead$total <- seedhead$Chae_acr + seedhead$Larinus + 
  seedhead$Metz_pau + seedhead$Urop_aff + seedhead$Urop_qua

names(seedhead)
spp <- seedhead[11:14]
spp[spp > 0] <- 1
df <- spp

colSums(df)

#Urop_aff = 1; Larinus = 2; Urop_qua = 3;  Metz_pau = 4
nrow(subset(df, Urop_aff == 1 & Larinus == 1))  #n12
nrow(subset(df, Urop_aff == 1 & Urop_qua == 1)) #n13
nrow(subset(df, Urop_aff == 1 & Metz_pau == 1)) #n14
nrow(subset(df, Larinus == 1 & Urop_qua == 1))  #n23
nrow(subset(df, Larinus == 1 & Metz_pau == 1))  #n24
nrow(subset(df,Urop_qua == 1 & Metz_pau == 1))  #n34
nrow(subset(df, Urop_aff == 1 & Larinus == 1 & Urop_qua == 1))  #n123
nrow(subset(df, Urop_aff == 1 & Larinus == 1 & Metz_pau == 1))  #n124
nrow(subset(df, Urop_aff == 1 & Urop_qua == 1 & Metz_pau == 1)) #n134
nrow(subset(df, Larinus == 1 & Urop_qua == 1 & Metz_pau == 1))  #n234
nrow(subset(df, Urop_aff == 1 & Larinus == 1 & Urop_qua == 1 & Metz_pau == 1)) #n1234

grid.newpage()
draw.quad.venn(area1 = 658, area2 = 250, area3 = 26, area4 = 5,
               n12 = 204, n13 = 18, n14 = 5, n23 = 10, n24 = 1, n34 = 0,
               n123 = 4, n124 = 1, n134 = 0, n234 = 0, n1234 = 0,
               category = c("U. affinis", "L. minutus", "U. quadrifasciata", "M. paucipunctella"),
               cat.cex = rep(1.5, 4), cat.fontface = rep("italic", 4), fill = c("skyblue", "pink1", "mediumorchid", "orange"),
               lwd = rep(1, 4), lty = rep("solid", 4), col = rep("black", 4))
#category = c("Urophora affinis", "Larinus minutus", "Urophora quadrifasciata", "Metzneria paucipunctella"),
##############################################################################
########################### as a proportion of the seedheads #################

sums<-colSums(df)
cent<- sums/740*100
cent

#Urop_aff = 1; Larinus = 2; Urop_qua = 3
nrow(subset(df, Urop_aff == 1 & Larinus == 1))  #n12
nrow(subset(df, Urop_aff == 1 & Urop_qua == 1)) #n13
nrow(subset(df, Larinus == 1 & Urop_qua == 1))  #n23
nrow(subset(df, Urop_aff == 1 & Larinus == 1 & Urop_qua == 1))  #n123

grid.newpage()
draw.triple.venn(area1 = 89, area2 = 34, area3 = 4,
                 n12 = 28, n13 = 0, n23 = 0, n123 = 0,
                 category = c("Urophora affinis", "Larinus minutus", "Urophora quadrifasciata"),
                 cat.cex = rep(1, 3), cat.fontface = rep("italic", 3), fill = c("skyblue", "pink1", "mediumorchid"),
                 lwd = rep(1, 3), lty = rep("solid", 3), col = rep("black", 3))

#############################################################################

nrow(subset(df, Urop_aff == 0 & Larinus == 0 & Urop_qua == 0 & Metz_pau == 0))  #n = 34

#########################################################################################
#https://rstudio-pubs-static.s3.amazonaws.com/13301_6641d73cfac741a59c0a851feb99e98b.html
