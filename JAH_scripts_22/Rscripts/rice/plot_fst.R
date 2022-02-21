library(dplyr)
library(tidyr)
library("RColorBrewer")
library("ggplot2")

I5fst <- read.delim("popI5_vs_popI234_slidingwindow.windowed.weir.fst.txt",header = TRUE)

head(I5fst)



pdf(file = "japonica_imputed_I5vsI234fst.pdf",width=6,height=6)
hist(I5fst$MEAN_FST,breaks = 100)
dev.off()

summary(I5fst)

I5fstn <- I5fst %>% mutate(pos = 1:n())

mycolors_change <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#ebca0e","#B15928")


#pgs  <- pg + scale_color_manual(values=mycolors)

p <- ggplot(data=I5fstn,aes(x=pos, y=MEAN_FST,color=CHROM))

pg  <- p + geom_point(size=0.3) + xlab("chromosome") + ylab("Fst")
#pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))
#pgs  <- pg + scale_color_brewer(palette = "Paired")
pgs  <- pg + scale_color_manual(values=mycolors_change)

pgslc <- pgs  + theme_classic()

pdf(file = "I5vsI234fst.pdf",width=12,height=2)
pgslc
dev.off()





p <- ggplot(data=I5fst,aes(x=BIN_START, y=MEAN_FST))

p <- ggplot(data=I5fst,aes(x=CHROM, y=MEAN_FST ,color=CHROM))
pg  <- p + geom_point(size=0.3) + xlab("chromosome") + ylab("gene fst")
   

pg  <- p + geom_point(size=0.3) + xlab("chromosome") + ylab("gene fst")
pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))

pgs  <- pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))

pdf(file = "I5vsI1genefst.pdf",width=12,height=2)
pgs
dev.off()



