library(reshape)
library(ggplot2)
library(dplyr)
library(tidyr)
#  Now plot region

loc <- read.csv("plot_location.csv", header=TRUE)

> dim(loc)
[1] 451   2
> table (loc)
Subpopulation
Location              admixed I1 I2 I3 I4 I5 Im J1 J2 J3 J4 Jm
Central Highlands         0  1  0  0  0  0  0  0  0  0  0  2
Mekong River Delta        0 15 44  0  0  0  4  0  0  0  0  0
North Central Coast       3  5  0  6  9 13  2 34  4  1  3  2
Northeast                 4  5  1  7  1  2  5 22 13  0  1  1
Northwest                 7  4  1 14  5  0  7 55 11  1  0  0
Red River Delta          13  6  1  0 32 12  5  0  6  0  8  0
South Central Coast       2  3  1  8  2  4 13  0  1 12  0  0
Southeast                 0  1  3  1  0  0  0  0  1  1  0  0
> 

loc_tab <- table (loc)
write.csv(loc_tab,"loc_tab.csv")

#### Do bubble plot

bub <- read.csv("plot_proportion_location.csv", header=TRUE)

mbub <- melt(bub)
head(mbub)

colnames(mbub) <- c("subpop","Location","proportion")

#scale_fill_manual(values = fill)
#palette(c("magenta3","magenta","cyan","steelblue3","green","orange","gold","indianred1","steelblue3",))

p <- ggplot(mbub, aes(x=subpop,y=Location))
p1 <- p + geom_point(aes(x=subpop, y=Location, size=proportion, col=factor(Location)),alpha=0.9)
p2 <- p1 + theme_classic()
p3 <- p2 + scale_color_manual(values=c("magenta","magenta3","steelblue3","cyan","green","indianred1","gold","orange"))

p4 <- p3 +theme(axis.text.x=element_text( size=10))
p5 <- p4 + theme(axis.text.y=element_text( size=10))
p6  <- p5 + theme(legend.text=element_text(size=10))


pdf(file = "R_Location.pdf", width = 6, height = 3.5);
p6
dev.off()



## check IRRI pops

ip <- read.csv("107_IRRI_pops.csv", header=TRUE)

> table (ip$K9_group_Admixture)

cA (Aus) cB (Bas)   GJ-adm GJ-sbtrp   GJ-tmp   GJ-trp    XI-1A    XI-1B     XI-2     XI-3   XI-adm 
5        5        4       10        8        9        7       11        8       17       23 
> table (ip$K15_Admixture_group)

cA-1     cA-2   cA-adm       cB   GJ-adm GJ-sbtrp   GJ-tmp  GJ-trp1  GJ-trp2    XI-1A   XI-1B1   XI-1B2    XI-2A    XI-2B 
2        1        2        5        2       10       10        8        1        7        3        1        6        1 
XI-3B1   XI-3B2   XI-adm 
15       12       21 




