vignette("conover.test")

#require(muStat)
require (PMCMR)
require (stats)
require(psych)
require(plotrix)

# *     <0.05
# **    <0.01
# ***   <0.001
#The conventional standard error of the mean = sd(x)/sqrt(sum(!is.na(x)))

df740 <- seedhead

################################################################# Larinus

df740$larcat[df740$Larinus == 0] <- 1
df740$larcat[df740$Larinus == 1] <- 2
df740$larcat[df740$Larinus == 2] <- 3

df740$larcat <- as.factor(df740$larcat)

describe(df740[df740$larcat == 1,])
#describe(df740[df740$larcat == 2,])
#describe(df740[df740$larcat == 3,])

# other
kruskal.test(other ~ larcat, data=df740)
posthoc.kruskal.conover.test(df740$other, g=df740$larcat, p.adjust.method="bonferroni")
with(df740, tapply(other, larcat, mean))
with(df740, tapply(other, larcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_larinus_other.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(
  other ~ larcat,data=df740,
  main=expression(paste("Non-viable seeds by ", italic("Larinus"))),
  sub="Kruskal-Wallis chi-squared = 36.178, df = 2, p-value = 1.394e-08",
  cex.sub=0.75,
  ylim = c(0,25),
  names = c("0", "1", "2"),
  xlab="Density", ylab="Seeds (other)")
text(c(24, 17, 10),c("a", "b", "ab"),cex=1) #indicate on the y axis where to add star
dev.off()


# black
kruskal.test(black~larcat, data=df740)
posthoc.kruskal.conover.test(df740$black, g=df740$larcat, p.adjust.method="bonferroni")
with(df740, tapply(black, larcat, mean))
with(df740, tapply(black, larcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_larinus_black.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(black ~ larcat,data=df740,
        main=expression(paste("Viable seeds by ", italic("Larinus"))),
        sub="Kruskal-Wallis chi-squared = 4.3115, df = 2, p-value = 0.1158",
        cex.sub=0.75,
        ylim = c(0,25),
        xlab="Density", ylab="Seeds (black)")
#text(c(-10, -10, -10),"*",cex=1) #indicate on the y axis where to add stars
dev.off()

################################################################# Urophora affinins

order(df740$Urop_aff)
uro_order <- df740[order(df740$Urop_aff),] 
uro_order$Urop_aff

df740$urocat[df740$Urop_aff == 0] <- 1
df740$urocat[df740$Urop_aff > 0 & df740$Urop_aff < 5] <- 2
df740$urocat[df740$Urop_aff > 4] <- 3

df740$urocat <- as.factor(df740$urocat)

describe(df740[df740$urocat == 1,])
describe(df740[df740$urocat == 2,])
describe(df740[df740$urocat == 3,])

# other
kruskal.test(other ~ urocat, data=df740)
posthoc.kruskal.conover.test(df740$other, g=df740$urocat, p.adjust.method="bonferroni")
with(df740, tapply(other, urocat, mean))
with(df740, tapply(other, urocat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_urop_aff_other.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(other ~ urocat,data=df740,
        font.main = 2,
        main=expression(paste("Non-viable seeds by ", italic("Urophora affinis"))),
        sub="Kruskal-Wallis chi-squared = 11.434, df = 2, p-value = 0.00329",
        cex.sub=0.75,
        ylim = c(0,25),
        names = c("0", "1-4", "5-10"),
        xlab="Density", ylab="Seeds (other)")
text(c(22, 24, 20), c("a", "a", "b"), cex=1) #indicate on the y axis where to add stars
dev.off()

# black
kruskal.test(black ~ urocat, data=df740)
posthoc.kruskal.conover.test(df740$black, g=df740$urocat, p.adjust.method="bonferroni")
with(df740, tapply(black, urocat, mean))
with(df740, tapply(black, urocat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_urop_aff_black.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(black ~ urocat,data=df740,
        main=expression(paste("Viable seeds by ", italic("Urophora affinis"))),
        sub="Kruskal-Wallis chi-squared = 3.5301, df = 2, p-value = 0.1712",
        cex.sub=0.75,
        ylim = c(0,25),
        xlab="Density", ylab="Seeds (black)")
#text(c(-10, -10, -10),"*",cex=1) #indicate on the y axis where to add stars
dev.off()

################################################################# Urophora quadrifasciata
df740$urqcat[df740$Urop_qua == 0] <- 1
df740$urqcat[df740$Urop_qua == 1] <- 2
df740$urqcat[df740$Urop_qua > 1] <- 3

nrow(df740[df740$urqcat == 1,])
nrow(df740[df740$urqcat == 2,])
nrow(df740[df740$urqcat == 3,])

df740$urqcat <- as.factor(df740$urqcat)

# other
kruskal.test(other ~ urqcat, data=df740)
posthoc.kruskal.conover.test(df740$other, g=df740$urqcat, p.adjust.method="bonferroni")
with(df740, tapply(other, urqcat, mean))
with(df740, tapply(other, urqcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_urop_qua_other.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(other ~ urqcat,data=df740,
        main=expression(paste("Non-viable seeds by ", italic("Urophora quadrifasciata"))),
        sub="Kruskal-Wallis chi-squared = 1.79171, df = 2, p-value = 0.4083",
        xlab="Density Category", ylab="Seeds (other)")
#text(c(-10, -10, -10),"*",cex=1) #indicate on the y axis where to add stars
dev.off()

# black
kruskal.test(black~urqcat, data=df740)
posthoc.kruskal.conover.test(df740$black, g=df740$urqcat, p.adjust.method="bonferroni")
with(df740, tapply(black, urqcat, mean))
with(df740, tapply(black, urqcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_urop_qua_black.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(black ~ urqcat,data=df740,
        main=expression(paste("Viable seeds by ", italic("Urophora quadrifasciata"))),
        sub="Kruskal-Wallis chi-squared = 2.7533, df = 2, p-value = 0.2524",
        xlab="Density Category", ylab="Seeds (black)")
#text(c(-10, -10, -10),"*",cex=1) #indicate on the y axis where to add stars
dev.off()

################################################################# Metzneria paucipunctella
df740$metzcat[df740$Metz_pau == 0] <- 1
df740$metzcat[df740$Metz_pau == 1] <- 2
df740$metzcat[df740$Metz_pau > 1] <- 3

nrow(df740[df740$metzcat == 1,])
nrow(df740[df740$metzcat == 2,])
nrow(df740[df740$metzcat == 3,])

df740$metzcat <- as.factor(df740$metzcat)

# other
kruskal.test(other ~ metzcat, data=df740)
posthoc.kruskal.conover.test(df740$other, g=df740$metzcat, p.adjust.method="bonferroni")
with(df740, tapply(other, metzcat, mean))
with(df740, tapply(other, metzcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_metz_pau_other.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(other ~ metzcat,data=df740,
        main=expression(paste("Non-viable seeds by ", italic("Metzneria paucipunctella"))),
        sub="Kruskal-Wallis chi-squared = 1.-794, df = 2, p-value = 0.2988",
        xlab="Density Category", ylab="Seeds (other)")
#text(c(-10, -10, -10),"*",cex=1) #indicate on the y axis where to add stars
dev.off()

# black
kruskal.test(black~metzcat, data=df740)
posthoc.kruskal.conover.test(df740$black, g=df740$metzcat, p.adjust.method="bonferroni")
with(df740, tapply(black, metzcat, mean))
with(df740, tapply(black, metzcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_metz_pau_black.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(black ~ metzcat,data=df740,
        main=expression(paste("Viable seeds by ", italic("Metzneria paucipunctella"))),
        sub="Kruskal-Wallis chi-squared = 2.7533, df = 2, p-value = 0.2524",
        xlab="Density Category", ylab="Seeds (black)")
#text(c(-10, -10, -10),"*",cex=1) #indicate on the y axis where to add stars
dev.off()

################################################################# total

order(df740$total)
total_order <- df740[order(df740$total),] 
total_order$total

df740$totcat[df740$total ==0] <- 1
df740$totcat[df740$total ==1] <- 2
df740$totcat[df740$total > 1] <- 3

df740$totcat <- as.factor(df740$totcat)

# other
kruskal.test(other ~ totcat, data=df740)
posthoc.kruskal.conover.test(df740$other, g=df740$totcat, p.adjust.method="bonferroni")
with(df740, tapply(other, totcat, mean))
with(df740, tapply(other, totcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_total_other.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(other ~ totcat,data=df740,
        main="Non-viable seeds by total insect number",
        sub="",
        cex.sub=0.75,
        ylim = c(0,15),
        xlab="Density", ylab="Seeds (other)")
text(c(8, 11, 14.5), c("a", "ab", "b"), cex=1) #indicate on the y axis where to add stars
dev.off()

# black
kruskal.test(black ~ totcat, data=df740)
posthoc.kruskal.conover.test(df740$black, g=df740$totcat, p.adjust.method="bonferroni")
with(df740, tapply(black, totcat, mean))
with(df740, tapply(black, totcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_total_black.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(black ~ totcat,data=df740,
        main="Viable seeds by total insect number",
        sub="",
        cex.sub=0.75,
        ylim = c(0,15),
        xlab="Density", ylab="Seeds (black)")
#text(c(-10, -10, -10),"*",cex=1) #indicate on the y axis where to add stars
dev.off()

####################### END ############