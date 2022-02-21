library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(dplyr)
library(tidyr)

pops <- read.csv("viet672_sub_pop_K8-combined-merged.csv")

allele <- read.delim("1_38274711_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")

p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os01g65904  chr1:38274711")

p2 <- p1 + theme_classic()

p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),
        axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)



pdf(file = "R_1_38274711.pdf",width=6,height=6)
p4
dev.off()



allele <- read.delim("5_719563_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")


p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1<- p + ggtitle("LOC_Os05g02260  chr5:719563")

p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),               axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)

pdf(file = "5_719563.pdf",width=6,height=6)
p4
dev.off()


allele <- read.delim("8_5278838_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")


p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os08g09110 chr8:5278838")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),               axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)

pdf(file = "8_5278838.pdf",width=6,height=6)
p4
dev.off()


allele <- read.delim("10_19045026_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")

p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os10g35604 chr10:19045026")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),               axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)

pdf(file = "10_19045026.pdf",width=6,height=6)
p4
dev.off()




allele <- read.delim("3_6907102_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")


p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <-  p + ggtitle("LOC_Os03g12840  chr3:6907102")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)

pdf(file = "3_6907102.pdf",width=6,height=6)
p4
dev.off()

allele <- read.delim("1_38192458_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")


p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os01g65770  chr1:38192458")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)

pdf(file = "1_38192458.pdf",width=6,height=6)
p4
dev.off()



allele <- read.delim("4_34939233_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")


p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os04g58740  chr4:34939233")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)


pdf(file = "4_34939233.pdf",width=6,height=6)
p4
dev.off()


allele <- read.delim("11_5026584_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")


p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os11g09360 OsFBX398 chr11:5026584")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)

pdf(file = "11_5026584.pdf",width=6,height=6)
p4
dev.off()

allele <- read.delim("11_5424384_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")

p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os11g10070 OsSEU2 chr11:5424384")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)


pdf(file = "11_5424384.pdf",width=6,height=6)
p4
dev.off()






allele <- read.delim("4_35019178_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")

p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os04g58870  chr4:35019178")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)

pdf(file = "4_35019178.pdf",width=6,height=6)
p4
dev.off()



allele <- read.delim("6_1559993_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")

p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os06g03860 OsSPX-MFS3 chr6:1559993")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)


pdf(file = "6_1559993.pdf",width=6,height=6)
p4
dev.off()





allele <- read.delim("10_19045026_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","allele")

pop_rat <- left_join(pops,alle, by = "Taxa")


p <- ggplot(data=pop_rat, aes(subpops1, fill=allele)) +
  geom_bar(stat = "count",
           position = "stack")
p1 <- p + ggtitle("LOC_Os10g35604 10_19045026")
p2 <- p1 + theme_classic()
p3 <- p2 + theme(axis.text.x=element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_text(size=14),axis.text.y=element_text(size=14))
p4 <- p3 + coord_cartesian(expand = FALSE)


pdf(file = "10_19045026.pdf",width=6,height=6)
p4
dev.off()

