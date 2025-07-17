getwd()
setwd("C:/R/knapweed")
dir<-getwd()
list.files(dir)

require (PMCMR)
require(plotrix)

## Data ##########

df740 <- seedhead

########################### Seeds #############################################

kruskal.test(black ~ site, data=df740)
posthoc.kruskal.conover.test(df740$black, g=df740$site, p.adjust.method="bonferroni")
with(df740, tapply(black, site, mean))
with(df740, tapply(black, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_geog_black.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(black ~ site,data=df740,
        font.main = 2,
        main="Viable seeds by site",
        sub="Kruskal-Wallis chi-squared = 11.913, df = 2, p-value = 0.002589",
        cex.sub=0.75,
        ylim = c(0,30),
        xlab="Site", ylab="Seeds (black)")
text(c(29, 19, 30),c("a", "b", "b"), cex=1) #indicate on the y axis where to add stars
dev.off()

kruskal.test(other ~ site, data=df740)
posthoc.kruskal.conover.test(df740$other, g=df740$site, p.adjust.method="bonferroni")
with(df740, tapply(other, site, mean))
with(df740, tapply(other, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_geog_other.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(other ~ site,data=df740,
        main="Non-viable seeds by site",
        sub="Kruskal-Wallis chi-squared = 76.927, df = 2, p-value < 2.2e-16",
        cex.sub=0.75,
        ylim = c(0,30),
        xlab="Site", ylab="Seeds (other)")
text(c(24, 19, 22),c("a", "b", "b")) #indicate on the y axis where to add stars
dev.off()

########################### Insects ###########################################

x <- df740$Larinus
x <- df740$Metz_pau
x <- df740$Urop_aff
x <- df740$Urop_qua

kruskal.test(x ~ site, data=df740)
posthoc.kruskal.conover.test(x, g=df740$site, p.adjust.method="bonferroni")
with(df740, tapply(x, site, mean))
with(df740, tapply(x, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_geog_Larinus.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(x ~ site,data=df740,
        main=expression(paste(italic("Larinus minutus "), "by site")),
        sub="Kruskal-Wallis chi-squared = 19.498, df = 2, p-value = 5.834e-05",
        cex.sub=0.75,
        ylim = c(0,2.75),
        xlab="Site", ylab=expression(paste(italic("L. minutus"))))
text(c(2.25, 2.25, 2.25), c("a", "a", "b"), cex=1) #indicate on the y axis where to add stars
dev.off()

tiff(file = "C:/R/knapweed/boxplots/seedhead_geog_Urophora affinis.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(x ~ site,data=df740,
        main=expression(paste(italic("Urophora affinis "), "by site")),
        sub="Kruskal-Wallis chi-squared = 31.953, df = 2, p-value = 1.152e-07",
        cex.sub=0.75,
        ylim = c(0,12),
        xlab="Site", ylab=expression(paste(italic("U. affinis"))))
text(c(10.75, 6.75, 8.75), c("a", "b", "a"), cex=1) #indicate on the y axis where to add stars
dev.off()

########################### Solitary Insects ###########################################

################################### Urophora ###########

df436 <- subset(df740, Urop_aff >= 1 & Larinus == 0 & Urop_qua == 0 & Metz_pau == 0)

nrow(df436[df436$site == "blue",])
nrow(df436[df436$site == "hill",])
nrow(df436[df436$site == "road",])

kruskal.test(black ~ site, data=df436)
posthoc.kruskal.conover.test(df436$black, g=df436$site, p.adjust.method="bonferroni")
with(df436, tapply(black, site, mean))
with(df436, tapply(black, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_geog_black_Urophora affinis ONLY.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(black ~ site,data=df436,
        main=expression(paste
                        ("Viable seeds by site\nSeed heads only containing ", italic("Urophora affinis")) ),
        sub="Kruskal-Wallis chi-squared = 7.9996, df = 2, p-value = 0.01832",
        cex.sub=0.75,
        ylim = c(0,30),
        xlab="Site", ylab="Seeds (black)")
text(c(28.5, 19, 30), c("a", "b", "ab"),cex=1) #indicate on the y axis where to add stars
dev.off()

###############################################

kruskal.test(other ~ site, data=df436)
posthoc.kruskal.conover.test(df436$other, g=df436$site, p.adjust.method="bonferroni")
with(df436, tapply(other, site, mean))
with(df436, tapply(other, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_geog_other_Urophora affinis ONLY.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(other ~ site,data=df436,
        main=expression(paste
                        ("Non-viable seeds by site\nSeed heads only containing ", italic("Urophora affinis")) ),
        sub="Kruskal-Wallis chi-squared = 42.331, df = 2, p-value = 6.427e-10",
        cex.sub=0.75,
        ylim = c(0,25),
        xlab="Site", ylab="Seeds (other)")
text(c(24, 19, 20),c("a", "b", "b"),cex=1) #indicate on the y axis where to add stars
dev.off()


################################################ Larinus ###############

df40 <- subset(df740, Larinus >= 1 & Urop_aff == 0 & Urop_qua == 0 & Metz_pau == 0)

nrow(df40[df40$site == "blue",])
nrow(df40[df40$site == "hill",])
nrow(df40[df40$site == "road",])

kruskal.test(black ~ site, data=df40)
posthoc.kruskal.conover.test(df40$black, g=df40$site, p.adjust.method="bonferroni")
with(df40, tapply(black, site, mean))
with(df40, tapply(black, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_geog_black_Larinus minutus ONLY.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(black ~ site,data=df40,
        main=expression(paste
                        ("Viable seeds by site\nSeed heads only containing ", italic("Larinus minutus")) ),
        sub="Kruskal-Wallis chi-squared = 2.2156, df = 2, p-value = 0.3303",
        cex.sub=0.75,
        ylim = c(0,12),
        xlab="Site", ylab="Seeds (black)")
#text(c(6.75, 11.75, 10.75),c("a", "ab", "b"),cex=1)
dev.off()

###############################################

kruskal.test(other ~ site, data=df40)
posthoc.kruskal.conover.test(df40$other, g=df40$site, p.adjust.method="bonferroni")
with(df40, tapply(other, site, mean))
with(df40, tapply(other, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/seedhead_geog_other_Larinus minutus ONLY.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(other ~ site,data=df40,
        main=expression(paste
                        ("Non-viable seeds by site\nSeed heads only containing ", italic("Larinus minutus")) ),
        sub="Kruskal-Wallis chi-squared = 7.87, df = 2, p-value = 0.01955",
        cex.sub=0.75,
        ylim = c(0,12),
        xlab="Site", ylab="Seeds (other)")
text(c(10.75, 7, 3),c("a", "ab", "b"),cex=1)
dev.off()

########################### Soil Seed ###########################################

df57 <- vegsoil
names(df57)

x <- df57$germ_m2
x <- df57$rupt_m2
x <- df57$viable

kruskal.test(x ~ site, data=df57)
posthoc.kruskal.conover.test(x, g=df57$site, p.adjust.method="bonferroni")
with(df57, tapply(x, site, mean))
with(df57, tapply(x, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/site_soil_ruptured seeds.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(rupt_m2 ~ site, data = df57,
        main = "Ruptured seeds by site", # ruptured and germinated seeds
        sub = "Kruskal-Wallis chi-squared = 18.907, df = 2, p-value = 7.842e-05",
        cex.sub = 0.75,
        ylim = c(0,1100),
        xlab = "Site", ylab = "Ruptured Seeds")
text(c(225, 825, 1050), c("a", "b", "a"), cex = 1)
dev.off()

tiff(file = "C:/R/knapweed/boxplots/site_soil_seed bank.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(viable ~ site, data = df57,
        main = "Seed bank by site", # ruptured and germinated seeds
        sub = "Kruskal-Wallis chi-squared = 19.185, df = 2, p-value = 6.826e-05",
        cex.sub = 0.75,
        ylim = c(0,1100),
        xlab = "Site", ylab = "Viable seeds")
text(c(225, 825, 1050), c("a", "b", "c"), cex = 1)
dev.off()

########################### Vegetation ###########################################

df57 <- vegsoil
names(df57)

x <- df57$bolt_m2
x <- df57$stem_m2
x <- df57$head_m2
x <- df57$rose_m2
x <- df57$ling_m2

kruskal.test(x ~ site, data=df57)
posthoc.kruskal.conover.test(x, g=df57$site, p.adjust.method="bonferroni")
with(df57, tapply(x, site, mean))
with(df57, tapply(x, site, std.error))

tiff(file = "C:/R/knapweed/boxplots/vegetation_geog_seedlings.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(x ~ site, data = df57,
        main = "Seedlings by site",
        sub = "Kruskal-Wallis chi-squared = 19.966, df = 2, p-value = 4.618e-05",
        cex.sub = 0.75,
        ylim = c(0,525),
        xlab = "Site", ylab = "Seedlings")
text(c(525, 75, 300), c("a", "b", "a"), cex = 1)
dev.off()


################################################################################

#test <- function(x, na.rm = TRUE, print = TRUE)
#{kruskal.test(x ~ site, data=df740)}