library("RColorBrewer")
library("ggplot2")
library(reshape) 
library(dplyr)
library(tidyr)
library(lattice)
library(gridExtra)
library(grid)

########### 672 viet samples 8 pops

#  input list
I5<- read.csv("pop_K5-I5combined-merged2.csv",header=TRUE)

#I5 <- I5[-1]
I5m <- melt(I5) 

#pcaPalette <- c("#4363d8","#f58231","#33A02C","#911eb4","#e6194B")
pcaPalette <- c("blue","darkorchid","forestgreen","darkorange","red")
# keep order

#I5m$Taxa <- factor(I5$Taxa, levels = I5$Taxa[order(I5$I5)])
I5m$Taxa <- factor(I5$Taxa, levels = I5$Taxa)

#plot pop proportion
cog<-ggplot(data=I5m, aes(x=Taxa, y=value, fill=variable)) +
  geom_bar(stat="identity")
cog2 <- cog + scale_fill_manual(values=pcaPalette)
cog3 <- cog2 +theme(axis.text.x=element_text(angle = 55, hjust = 1, size=9))+ylab ("Genotype") + scale_y_continuous(expand = c(0,0)) 

cog4 <- cog3 + theme(axis.text.y=element_text( size=12))
cog5  <- cog4 + theme(legend.text=element_text(size=16))

pdf(file = "R_I5_pop5.pdf",width=8,height=4)
cog5
dev.off()


