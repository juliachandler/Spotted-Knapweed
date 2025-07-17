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

#describe(df740[df740$larcat == 1,])
#describe(df740[df740$larcat == 2,])
#describe(df740[df740$larcat == 3,])


x <- df740$Urop_aff
x <- df740$Metz_pau

kruskal.test(x ~ larcat, data=df740)
posthoc.kruskal.conover.test(x, g=df740$larcat, p.adjust.method="bonferroni")
with(df740, tapply(x, larcat, mean))
with(df740, tapply(x, larcat, std.error))

tiff(file = "C:/R/knapweed/boxplots/Larinus_Uaff.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(Urop_aff ~ larcat,data=df740,
        main=expression(paste(italic("Urophora affinis"), " by ", italic("Larinus minutus"))),
        sub="Kruskal-Wallis chi-squared = 56.591, df = 2, p-value = 5.145e-13",
        cex.sub=0.75,
        ylim = c(0,12),
        names = c("0", "1", "2"),
        xlab=expression(paste(italic("L. minutus"))), ylab=expression(paste(italic("U. affinis"))))
text(c(10, 11.25, 9),c("a", "b", "ab"),cex=1) #indicate on the y axis where to add star
dev.off()

tiff(file = "C:/R/knapweed/boxplots/Larinus_Metz.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(Metz_pau ~ larcat,data=df740,
        main=expression(paste(italic("Metzneria paucipunctella"), " by ", italic("Larinus minutus"))),
        sub="Kruskal-Wallis chi-squared = 18.432, df = 2, p-value = 9.941e-05",
        cex.sub=0.75,
        ylim = c(0,2),
        names = c("0", "1", "2"),
        xlab=expression(paste(italic("L. minutus"))), ylab=expression(paste(italic("M. paucipunctella"))))
text(c(1.2, 0.15, 1.2),c("a", "a", "b"),cex=1) #indicate on the y axis where to add star
dev.off()

################################################################# Urophora affinis

df740$urocat[df740$Urop_aff == 0] <- 1
df740$urocat[df740$Urop_aff > 0 & df740$Urop_aff < 5] <- 2
df740$urocat[df740$Urop_aff > 4] <- 3

df740$urocat <- as.factor(df740$urocat)

#describe(df740[df740$urocat == 1,])
#describe(df740[df740$urocat == 2,])
#describe(df740[df740$urocat == 3,])

x <- df740$Larinus
x <- df740$Urop_qua

kruskal.test(x ~ urocat, data=df740)
posthoc.kruskal.conover.test(x, g=df740$urocat, p.adjust.method="bonferroni")
with(df740, tapply(x, urocat, mean))
with(df740, tapply(x, urocat, std.error))

tiff(file = "C:/R/knapweed/boxplots/Uaff_Larinus.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(Larinus ~ urocat,data=df740,
        main=expression(paste(italic("Larinus minutus"), " by ", italic("Urophora affinis"))),
        sub="Kruskal-Wallis chi-squared = 38.427, df = 2, p-value = 4.526e-09",
        cex.sub=0.75,
        ylim = c(0,25),
        names = c("0", "1-4", "5-10"),
        xlab=expression(paste(italic("U. affinis"))), ylab=expression(paste(italic("L. minutus"))))
text(c(4, 4, 4),c("a", "b", "c"),cex=1) #indicate on the y axis where to add star
dev.off()

tiff(file = "C:/R/knapweed/boxplots/Uaff_Uqua.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(Urop_qua ~ urocat,data=df740,
        main=expression(paste(italic("Urophora Quadrifasciata"), " by ", italic("Urophora affinis"))),
        sub="Kruskal-Wallis chi-squared = 13.855, df = 2, p-value = 0.0009804",
        cex.sub=0.75,
        ylim = c(0,5),
        names = c("0", "1-4", "5-10"),
        xlab=expression(paste(italic("U. affinis"))), ylab=expression(paste(italic("U. Quadrifasciata"))))
text(c(4.5, 3.5, 1.5),c("a", "b", "b"),cex=1) #indicate on the y axis where to add star
dev.off()


####################### END ######################################################