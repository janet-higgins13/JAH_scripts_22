library("RColorBrewer")
library("ggplot2")
library(reshape) 
library(dplyr)
library(tidyr)
library(lattice)
library(gridExtra)
library(grid)


### plot diversity
I1<-read.table ("ind_I1.windowed.pi",header=TRUE)
I2<-read.table ("ind_I2.windowed.pi",header=TRUE)
I3<-read.table ("ind_I3.windowed.pi",header=TRUE)
I4<-read.table ("ind_I4.windowed.pi",header=TRUE)
I5<-read.table ("ind_I5.windowed.pi",header=TRUE)

I1S <- mutate(I1,pop = "I1")
I2S <- mutate(I2,pop = "I2")
I3S <- mutate(I3,pop = "I3")
I4S <- mutate(I4,pop = "I4")
I5S <- mutate(I5,pop = "I5")

all <- rbind(I1S,I2S,I3S,I4S,I5S)

chr5 <- subset(all,all$CHROM == "5")

mdata_all <- melt(chr5, id=c("pop","BIN_START","PI","CHROM","N_VARIANTS")) 

colnames(mdata_all) <- c("subpop","Position","Diversity_PI","CHROM","N_VARIANTS","variable","value")
### line plot

ind_palette <- c("#4363d8","#911eb4","#33A02C","#f58231","#e6194B")

p <- ggplot(data=mdata_all,
            aes(x=Position, y=Diversity_PI, colour=subpop)) +
  geom_line(size = 0.5) + scale_colour_manual(values=ind_palette)
pl <- p + xlim(0,2000000)
plk <- pl + geom_segment(aes(x=386347,xend=1563159,y=0.0001, yend=0.0001), size=1, colour="red")
plt <- plk + geom_segment(aes(x=719132,xend=721816,y=-0.0003, yend=-0.0003), size=2, colour="blue")
plw <- plt + geom_segment(aes(x=717557,xend=743760,y=-0.0001, yend=-0.0001), size=2, colour="darkgreen")
plx <- plw + theme_classic()
ply <- plx + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=14), #change legend title font size
                   legend.text = element_text(size=14)) #change legend text font size

plll <- ply + guides(color = guide_legend(override.aes = list(size = 5) ) )


pdf(file = "chr5_diversity.pdf",width=10,height=4)
plll
dev.off()


theme(legend.key.size = unit(1, 'cm'), #change legend key size
      legend.key.height = unit(1, 'cm'), #change legend key height
      legend.key.width = unit(1, 'cm'), #change legend key width
      legend.title = element_text(size=14), #change legend title font size
      legend.text = element_text(size=10)) #change legend text font size



plot fst

I5fst <- read.delim("popI5_vs_popI234_slidingwindow.windowed.weir.fst.txt",header = TRUE)

fchr5 <- subset(I5fst,I5fst$CHROM == "echr5")

head(fchr5)

I5fstn <- fchr5 %>% mutate(pos = 1:n())

mycolors_change <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#ebca0e","#B15928")


#pgs  <- pg + scale_color_manual(values=mycolors)

p <- ggplot(data=fchr5,aes(x=BIN_START, y=MEAN_FST,color="red"))
pg  <- p + geom_point(size=0.3) + xlab("chromosome") + ylab("Fst")
pl <- pg + xlim(0,2000000)
plk <- pl + geom_segment(aes(x=386347,xend=1563159,y=0.0001, yend=0.0001), size=1, colour="red")
plt <- plk + geom_segment(aes(x=719132,xend=721816,y=-0.0003, yend=-0.0003), size=2, colour="blue")
plw <- plt + geom_segment(aes(x=717557,xend=743760,y=-0.0001, yend=-0.0001), size=2, colour="darkgreen")
plx <- plw + theme_classic()
#ply <- plx + theme(legend.text = element_text(size=14))
plx <- plk + theme_classic()
ply <- plx + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=14), #change legend title font size
                   legend.text = element_text(size=14)) #change legend text font size

plll <- ply + guides(color = guide_legend(override.aes = list(size = 5) ) )

pdf(file = "I5vsI234fst.pdf",width=12,height=2)
ply
dev.off()


#  plot xplclr
xp51 <- read.delim ("chr5_xp_AI5_BI1.out", header = TRUE)
xp52 <- read.delim ("chr5_xp_AI5_BI2.out", header = TRUE)
xp53 <- read.delim ("chr5_xp_AI5_BI3.out", header = TRUE)
xp54 <- read.delim ("chr5_xp_AI5_BI4.out", header = TRUE)

xp51S <- mutate(xp51,pop = "I1")
xp52S <- mutate(xp52,pop = "I2")
xp53S <- mutate(xp53,pop = "I3")
xp54S <- mutate(xp54,pop = "I4")

all <- rbind(xp51S,xp52S,xp53S,xp54S)

chr5 <- subset(all,all$chrom == "5")

chr5S <- chr5 %>% select(chrom,start,xpclr,pop)

mdata_all <- melt(chr5S, id=c("chrom","start","xpclr")) 

#colnames(mdata_all) <- c("subpop","Position","Diversity_PI","CHROM","N_VARIANTS","variable","value")
### line plot

ind_palette <- c("#4363d8","#911eb4","#33A02C","#f58231","#e6194B")

p <- ggplot(data=mdata_all,
            aes(x=start, y=xpclr, colour=value)) +
  geom_point(size = 0.3, alpha = 0.7) + scale_colour_manual(values=ind_palette)
plk <- p + geom_segment(aes(x=386347,xend=1563159,y=0.0001, yend=0.0001), size=1, colour="red")
#plt <- plk + geom_segment(aes(x=719132,xend=721816,y=-0.0003, yend=-0.0003), size=2, colour="blue")
#plw <- plt + geom_segment(aes(x=717557,xend=743760,y=-0.0001, yend=-0.0001), size=2, colour="darkgreen")
plx <- plk + theme_classic()
ply <- plx + theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=14), #change legend title font size
                   legend.text = element_text(size=14)) #change legend text font size

plll <- ply + guides(color = guide_legend(override.aes = list(size = 5) ) )

pdf(file = "I5_xpclr.pdf",width=10,height=2)
plll
dev.off()


p <- ggplot(data=mdata_all,
            aes(x=start, y=xpclr, colour=value)) +
  geom_line() + scale_colour_manual(values=ind_palette)
pl <- p + xlim(0,2000000)
plk <- pl + geom_segment(aes(x=386347,xend=1563159,y=0.0001, yend=0.0001), size=1, colour="red")
plt <- plk + geom_segment(aes(x=719132,xend=721816,y=0.0002, yend=0.0002), size=2, colour="blue")
plw <- plt + geom_segment(aes(x=717557,xend=743760,y=-0.0003, yend=-0.0003), size=2, colour="darkgreen")

pdf(file = "I5_xpclr_line.pdf",width=10,height=3)
plw
dev.off()












p7 <- p6 + geom_vline(xintercept = 8654521, linetype="solid", color = "gold4", size=0.5)
p8 <- p7 + geom_vline(xintercept = 9599409, linetype="solid", color = "gold4", size=0.5)


pdf(file = "chr10_region_diversity.pdf",width=10,height=5)
p8
dev.off()





