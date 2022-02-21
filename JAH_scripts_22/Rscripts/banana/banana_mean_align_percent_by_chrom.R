library(reshape)
library(ggplot2)

library(dplyr)
library(tidyr)


### get align stats per chromsome by separating the chromosomes in the bam file
#and running bamtools stats

### using pandas in iPython
##  add cluster details and total reads
## put together stats for each sample and calculate mean for each cluster
## add chr in excel

## plot mean data - for only the 13 clusters
data <- read.csv("mean_ABB_AA_BB_stats_13gp.csv")
str(data)
dim(data)

order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
data$chrom <- factor(data$chrom, levels = order)

mycolors_change <- c("dodgerblue","darkolivegreen","firebrick3")

### point and line plot of alignment stats by group
p<-ggplot(data,aes(chrom,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~our_group)

pdf(file = "align_gp13_percent_gen_line.pdf",width=12,height=12)
p5
dev.off()

### point and line plot of alignment stats by chromosome
p<-ggplot(data,aes(our_group,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "chrom_align_gp13_percent.pdf",width=12,height=12)
p5
dev.off()


### point and line plot of alignment stats by genome

mycolors_group <- c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990")


p<-ggplot(data,aes(chrom,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=our_group))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_group)
p5 <- p4 + facet_wrap(~genome)

pdf(file = "group_align_gp13_percent.pdf",width=12,height=12)
p5
dev.off()

### point and join up chrom line plot of alignment stats by group

p <- ggplot(data, aes(x = chrom, y = percent, color = genome, group = genome)) + 
  geom_line(col = "grey60") + geom_point() 
p2 <- p + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p3 <- p2 +  scale_color_manual(values=mycolors_change)
p4 <- p3 + facet_wrap(~our_group)

pdf(file = "chrom_join_align_gp13_percent.pdf",width=12,height=12)
p4
dev.off()

### boxplot of alignment stats by group
p<-ggplot(data,aes(our_group,percent))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=genome)) + theme(text = element_text(size=10))
p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10))
p3 <- p2 +  scale_color_manual(values=mycolors_change)

pdf(file = "box_align_gen_percent_gp13.pdf",width=12,height=8)
p3
dev.off()

### boxplot of alignment stats by genome

mycolors_group <- c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990")

p<-ggplot(data,aes(chrom,percent))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=10))
p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p3 <- p2 +  scale_color_manual(values=mycolors_group)
p4 <- p3 + facet_wrap(~genome)

pdf(file = "box_genome_align_gp13.pdf",width=12,height=8)
p4
dev.off()



#### now do point and line plot each genome separately #########

mycolors_group <- c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990")

AA <- subset(data, data$genome== "AA")
dim(AA)
table(AA$our_group)

p<-ggplot(AA,aes(chrom,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=our_group))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_group)

pdf(file = "align_per_chromAA.pdf",width=8,height=6)
p3
dev.off()

ABB <- subset(data, data$genome== "ABB")
dim(ABB)
table(ABB$our_group)

### boxplot of alignment stats
p<-ggplot(ABB,aes(chrom,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=our_group))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_group)

pdf(file = "align_per_chromABB.pdf",width=8,height=6)
p3
dev.off()


BB <- subset(data, data$genome== "BB")
dim(BB)
table(BB$our_group)

### boxplot of alignment stats
p<-ggplot(BB,aes(chrom,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=our_group))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_group)

pdf(file = "align_per_chromBB.pdf",width=8,height=6)
p3
dev.off()

#### point and line plot each genome separately #########
