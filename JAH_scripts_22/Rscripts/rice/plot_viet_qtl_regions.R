library(reshape)
library(ggplot2)
library(RColorBrewer)

library(dplyr)
library(tidyr)

display.brewer.pal(8, "Dark2")

brewer.pal(n = 8, name = "Dark2")

QT<-read.csv ("Hoang_2019_tol_S7.csv")
QR<-read.csv ("Phung2016_root_QTL_region50000.csv")
QP<-read.csv ("Nhung_2018_pQTL.csv")
leaf<-read.csv ("LeafQTL_Hoang2019.csv")
gwas<-read.csv ("sorted_GWAS_QTL_list.csv")
all<-read.csv ("I5_all_merge.csv")
jas <-read.csv ("jasmonate_qtl.csv")
phoM <- read.csv ("Mai_phosphate_qtl.csv")
phoT <- read.csv ("To_QTL_phosphate.csv")


chrl<-read.csv("chr_length.csv")
cm <- read.csv("Rice_Centromere.csv")


I5_1 <-slice(all,1L)
I5_5 <-slice(all,5L)
I5_6 <-slice(all,6L)
I5_16 <-slice(all,16L)
I5_30 <-slice(all,30L)
I5_31 <-slice(all,31L)
I5_32 <-slice(all,32L)
I5_33 <-slice(all,33L)
I5_34 <-slice(all,34L)
I5_35 <-slice(all,35L)
I5_36 <-slice(all,36L)
I5_37 <-slice(all,37L)
I5_39 <-slice(all,39L)
I5_48 <-slice(all,48L)
I5_49 <-slice(all,49L)

p <- ggplot(data=chrl, aes(chr, length),colour=chr) + geom_bar( stat="identity", fill="grey85",width = 0.94) + coord_flip() + theme_classic ()
p1 <- p + geom_segment(data=QT, aes(x=chrom,xend=chrom,y=start, yend=end), size=0.5, colour="#1B9E77")
p2 <- p1 + geom_segment(data=QP, aes(x=chrom+0.1,xend=chrom+0.1,y=start, yend=end), size=0.5, colour="#D95F02")
p3 <- p2 + geom_segment(data=QR, aes(x=chrom+0.2,xend=chrom+0.2,y=start, yend=end), size=0.5, colour="purple")
p4 <- p3 + geom_segment(data=leaf, aes(x=chrom-0.1,xend=chrom-0.1,y=start, yend=end), size=0.5, colour="steelblue2")
p5 <- p4 + geom_segment(data=gwas, aes(x=chrom-0.3,xend=chrom-0.3,y=start, yend=end), size=0.5, colour="#66A61E")
p6 <- p5 + geom_segment(data=all, aes(x=chrom-0.4,xend=chrom-0.4,y=start, yend=end), size=0.5, colour="red")
p7 <- p6 + geom_segment(data=cm, aes(x=chromosome,xend=chromosome,y=start, yend=end), size=9, colour="grey50")
p8 <- p7 + geom_segment(data=jas, aes(x=Chr-0.2,xend=Chr-0.2,y=start, yend=end), size=0.5, colour="#A6761D")
p9 <- p8 + geom_segment(data=phoM, aes(x=chrom+0.3,xend=chrom+0.3,y=start, yend=end), size=0.5, colour="#E6AB02")
p10 <- p9 + geom_segment(data=phoT, aes(x=chrom+0.4,xend=chrom+0.4,y=start, yend=end), size=0.5, colour="navy")
#p11 <- p10 + geom_segment(data=I5_6, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p12 <- p10 + geom_segment(data=I5_16, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p13 <- p12 + geom_segment(data=I5_30, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p14 <- p13 + geom_segment(data=I5_31, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p15 <- p14 + geom_segment(data=I5_32, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p16 <- p15 + geom_segment(data=I5_33, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p17 <- p16 + geom_segment(data=I5_34, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p18 <- p17 + geom_segment(data=I5_35, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p19 <- p18 + geom_segment(data=I5_36, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p20 <- p19 + geom_segment(data=I5_37, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p21 <- p20 + geom_segment(data=I5_48, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p22 <- p21 + geom_segment(data=I5_49, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p23 <- p22 + geom_segment(data=I5_1, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p24 <- p23 + geom_segment(data=I5_5, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.2)
p25 <- p24 + geom_segment(data=I5_39, aes(x=chrom,xend=chrom,y=start, yend=end), size=9, colour="darkorchid4", alpha=0.3)


p26 <- p25  + scale_x_continuous(expand = c(0,0.1)) 

pdf(file = "all_qtl_I5_v5.pdf",width=10,height=4)
p26
dev.off()

## plot legend separtely

leg.txt <- c("P efficiency","P starvation","Root","Panicle","Tolerance","Leaf","Jasmonate","Plant and seed architecture","I5 selected")
col.txt <- c("navy","#E6AB02","purple","#D95F02","#1B9E77","steelblue2","#A6761D","#66A61E","red")
legend("left", leg.txt,ncol = 1,fill = col.txt)


pdf(file = "legend2.pdf",width=9,height=3)
plot.new()
legend("left", leg.txt,ncol = 3,fill = col.txt)
dev.off()

pdf(file = "legend2.pdf",width=6,height=6)
plot.new()
legend("left", leg.txt,ncol = 1,fill = col.txt)
dev.off()

## Run the example in '?matplot' or the following:

plot.new()
legend("left", leg.txt,ncol = 4,col = col.txt,title = "QTLs",pch = 19)

plot.new()
legend("left", leg.txt,ncol = 4,fill = col.txt,title = "QTLs")
       
  