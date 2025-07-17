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

df57 <- vegsoil
names(df57)

################################################################# SeedNo

df57$bank[df57$viable < 1] <- 1
df57$bank[df57$viable > 1 & df57$viable < 199] <- 2
df57$bank[df57$viable > 199] <- 3

df57$bank <- as.factor(df57$bank)

describe(df57[df57$bank == 1,])
describe(df57[df57$bank == 2,])
describe(df57[df57$bank == 3,])

x <- df57$rose_m2
x <- df57$bolt_m2
x <- df57$stem_m2
x <- df57$head_m2
x <- df57$ling_m2

kruskal.test(x ~ bank, data=df57)
posthoc.kruskal.conover.test(x, g=df57$bank, p.adjust.method="bonferroni")
with(df57, tapply(x, bank, mean))
with(df57, tapply(x, bank, std.error))

tiff(file = "C:/R/knapweed/boxplots/vegetation_seedbank_seedlings.tif", bg = "transparent",
     units = "px", width = 3200, height = 3200, res = 600, compression = "lzw")
boxplot(x ~ bank,data=df57,
        #font.main = 1,
        main="Seedlings by seed bank", #ruptured and germinated seeds (m2)
        sub="Kruskal-Wallis chi-squared = 6.0814, df = 2, p-value = 0.0478",
        cex.sub=0.75,
        ylim = c(0,525),
        names = c("0", "1-199", ">200"),
        xlab="Seed bank", ylab="Seedlings")
text(c(520, 275, 300),c("a", "ab", "b"), cex=1) #indicate on the y axis where to add stars
dev.off()

################################################################# GermNo
# only 2 observations of germinated seeds

#df57$germ[df57$germ_m2 < 4500] <- 1
#df57$germ[df57$germ_m2 > 4500 & df57$germ_m2 < 9000] <- 2
#df57$germ[df57$germ_m2 > 9000] <- 3

#df57$germ <- as.factor(df57$germ)

#describe(df57[df57$germ == 1,])
#describe(df57[df57$germ == 2,])
#describe(df57[df57$germ == 3,])

#x <- df57$rose_m2
#x <- df57$ling_m2
#x <- df57$bolt_m2
#x <- df57$stem_m2
#x <- df57$head_m2

#kruskal.test(x ~ germ, data=df57)
#posthoc.kruskal.conover.test(x, g=df57$germ, p.adjust.method="bonferroni")
#with(df57, tapply(x, germ, mean))
#with(df57, tapply(x, germ, std.error))
####################### END ######################################################