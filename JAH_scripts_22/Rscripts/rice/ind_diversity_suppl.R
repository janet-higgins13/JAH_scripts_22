library("RColorBrewer")
library("ggplot2")
library(reshape) 
library(dplyr)
library(tidyr)
library(lattice)
library(gridExtra)
library(grid)

I1<-read.table ("ind_I1.windowed.pi",header=TRUE)
I2<-read.table ("ind_I2.windowed.pi",header=TRUE)
I3<-read.table ("ind_I3.windowed.pi",header=TRUE)
I4<-read.table ("ind_I4.windowed.pi",header=TRUE)
I5<-read.table ("ind_I5.windowed.pi",header=TRUE)
cm<-read.csv ("Rice_CentromereForDiv.csv",header=TRUE)

I1S <- mutate(I1,pop = "I1")
I2S <- mutate(I2,pop = "I2")
I3S <- mutate(I3,pop = "I3")
I4S <- mutate(I4,pop = "I4")
I5S <- mutate(I5,pop = "I5")
cen <- mutate(cm,pop = "centromere")


all <- rbind(cen, I1S,I2S,I3S,I4S,I5S)
mdata_all <- melt(all, id=c("pop","BIN_START","PI","CHROM","N_VARIANTS")) 

colnames(mdata_all) <- c("subpop","Position","Diversity_PI","CHROM","N_VARIANTS","variable","value")
### line plot

ind_palette <- c("grey40","#4363d8","#911eb4","#33A02C","#f58231","#e6194B")

p <- ggplot(data=mdata_all,
            aes(x=Position, y=Diversity_PI, colour=subpop)) +
  geom_line(size = 0.5) + scale_colour_manual(values=ind_palette) + facet_wrap(~CHROM,ncol=1) 

pdf(file = "I12345_diversity_cen.pdf",width=10,height=16)
p
dev.off()


p <- ggplot(data=mdata_all,
            aes(x=Position, y=Diversity_PI, colour=subpop)) +
  geom_line() + facet_wrap(~CHROM,ncol=1) 



I5S <- mutate(I5,pop = "I5")
cen <- mutate(cm,pop = "centromere")

all <- rbind(cen,I5S)
mdata_cen <- melt(all, id=c("pop","BIN_START","PI","CHROM","N_VARIANTS")) 

colnames(mdata_cen) <- c("subpop","Position","Diversity_PI","CHROM","N_VARIANTS","variable","value")
### line plot


#ind_palette <- c("grey40")
#ind_palette <- c("grey40","#e6194B")
ind_palette <- c"#e6194B","grey40")

p <- ggplot(data=mdata_cen,
            aes(x=Position, y=Diversity_PI, colour=subpop)) +
  geom_line(size = 0.5) + scale_colour_manual(values=ind_palette) + facet_wrap(~CHROM,ncol=1) 

pdf(file = "I5_diversity_cen.pdf",width=10,height=16)
p
dev.off()





chr6<- subset(all,CHROM==6)
mchr6<-melt(chr6, id=c("pop","BIN_START","PI","CHROM","N_VARIANTS")) 
colnames(mchr6) <- c("subpop","Position","Diversity_PI","CHROM","N_VARIANTS","variable","value")

p <- ggplot(data=mchr6,aes(x=Position, y=Diversity_PI, colour=subpop)) + geom_line() 
p1 <- p + scale_colour_manual(values=ind_palette)
p2 <- p1 + xlim( c(18000000, 22000000))
p3 <- p2 + geom_vline(xintercept = 19470641, linetype="solid", color = "gold4", size=0.5)
p4 <- p3 + geom_vline(xintercept = 20509810, linetype="solid", color = "gold4", size=0.5)

pdf(file = "chr6_region_diversity.pdf",width=10,height=5)
p4
dev.off()


chr7<- subset(all,CHROM==7)
mchr7<-melt(chr7, id=c("pop","BIN_START","PI","CHROM","N_VARIANTS")) 
colnames(mchr7) <- c("subpop","Position","Diversity_PI","CHROM","N_VARIANTS","variable","value")

p <- ggplot(data=mchr7,aes(x=Position, y=Diversity_PI, colour=subpop)) + geom_line() 
p1 <- p + scale_colour_manual(values=ind_palette)
p2 <- p1 + xlim( c(18000000, 32000000))
p3 <- p2 + geom_vline(xintercept = 18991429, linetype="solid", color = "gold4", size=0.5)
p4 <- p3 + geom_vline(xintercept = 19489875, linetype="solid", color = "gold4", size=0.5)
p5 <- p4 + geom_vline(xintercept = 20530677, linetype="solid", color = "blue", size=0.5)
p6 <- p5 + geom_vline(xintercept = 20989643, linetype="solid", color = "blue", size=0.5)
p7 <- p6 + geom_vline(xintercept = 26510388, linetype="solid", color = "gold4", size=0.5)
p8 <- p7 + geom_vline(xintercept = 27018603, linetype="solid", color = "gold4", size=0.5)

pdf(file = "selI1_chr7_region_diversity.pdf",width=10,height=5)
p8
dev.off()

chr9<- subset(all,CHROM==9)
mchr9<-melt(chr9, id=c("pop","BIN_START","PI","CHROM","N_VARIANTS")) 
colnames(mchr9) <- c("subpop","Position","Diversity_PI","CHROM","N_VARIANTS","variable","value")

p <- ggplot(data=mchr9,aes(x=Position, y=Diversity_PI, colour=subpop)) + geom_line() 
p1 <- p + scale_colour_manual(values=ind_palette)
p2 <- p1 + xlim( c(10000000, 20000000))
p3 <- p2 + geom_vline(xintercept = 16690209, linetype="solid", color = "gold4", size=0.5)
p4 <- p3 + geom_vline(xintercept = 18049085, linetype="solid", color = "gold4", size=0.5)

pdf(file = "chr9_region_diversity.pdf",width=10,height=5)
p4
dev.off()



chr10<- subset(all,CHROM==10)
mchr10<-melt(chr10, id=c("pop","BIN_START","PI","CHROM","N_VARIANTS")) 
colnames(mchr10) <- c("subpop","Position","Diversity_PI","CHROM","N_VARIANTS","variable","value")


p <- ggplot(data=mchr10,aes(x=Position, y=Diversity_PI, colour=subpop)) + geom_line() 
p1 <- p + scale_colour_manual(values=ind_palette)
p2 <- p1 + xlim( c(5000000, 10000000))
p3 <- p2 + geom_vline(xintercept = 5381471, linetype="solid", color = "gold4", size=0.5)
p4 <- p3 + geom_vline(xintercept = 5829470, linetype="solid", color = "gold4", size=0.5)

p5 <- p4 + geom_vline(xintercept = 6250442, linetype="solid", color = "blue", size=0.5)
p6 <- p5 + geom_vline(xintercept = 6599938, linetype="solid", color = "blue", size=0.5)


p7 <- p6 + geom_vline(xintercept = 8654521, linetype="solid", color = "gold4", size=0.5)
p8 <- p7 + geom_vline(xintercept = 9599409, linetype="solid", color = "gold4", size=0.5)


pdf(file = "chr10_region_diversity.pdf",width=10,height=5)
p8
dev.off()





