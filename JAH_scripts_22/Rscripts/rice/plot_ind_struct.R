library("RColorBrewer")
library("ggplot2")
library(reshape) 
library(dplyr)
library(tidyr)
library(lattice)
library(gridExtra)
library(grid)


#  input list
ind<- read.csv("pop_K5-combined-merged.csv",header=TRUE)

> table(ind$ind_pop5_final)

I1  I2  I3  I4  I5  Im 
145  91  37  62  43  48 

indp <- ind[-7]
indpm <- melt(indp) 

pcaPalette <- c("blue","darkorchid","forestgreen","darkorange","red")

# keep order

indpm$sample <- factor(indp$sample, levels = indp$sample)

#plot pop proportion
cog<-ggplot(data=indpm, aes(x=sample, y=value, fill=variable)) +
  geom_bar(stat="identity")
cog2 <- cog + scale_fill_manual(values=pcaPalette)
cog3 <- cog2 +theme(axis.text.x=element_text(angle = 55, hjust = 1, size=8))+ylab ("Genotype") + scale_y_continuous(expand = c(0,0)) 
cog4 <- cog3 + theme(axis.text.y=element_text( size=12))
cog5  <- cog4 + theme(legend.text=element_text(size=16))

pdf(file = "R_ind_pop5.pdf",width=10,height=4)
cog5
dev.off()


