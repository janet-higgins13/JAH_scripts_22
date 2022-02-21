library("RColorBrewer")
library("ggplot2")

library(dplyr)
library(tidyr)


xp1 <- read.delim ("chr1_xp_AI5_BI1.out", header = TRUE)
xp2 <- read.delim ("chr2_xp_AI5_BI1.out", header = TRUE)
xp3 <- read.delim ("chr3_xp_AI5_BI1.out", header = TRUE)
xp4 <- read.delim ("chr4_xp_AI5_BI1.out", header = TRUE)
xp5 <- read.delim ("chr5_xp_AI5_BI1.out", header = TRUE)
xp6 <- read.delim ("chr6_xp_AI5_BI1.out", header = TRUE)
xp7 <- read.delim ("chr7_xp_AI5_BI1.out", header = TRUE)
xp8 <- read.delim ("chr8_xp_AI5_BI1.out", header = TRUE)
xp9 <- read.delim ("chr9_xp_AI5_BI1.out", header = TRUE)
xp10 <- read.delim ("chr10_xp_AI5_BI1.out", header = TRUE)
xp11 <- read.delim ("chr11_xp_AI5_BI1.out", header = TRUE)
xp12 <- read.delim ("chr12_xp_AI5_BI1.out", header = TRUE)

xp1m <- mutate(xp1,chr = "achr1")
xp2m <- mutate(xp2,chr = "bchr2")
xp3m <- mutate(xp3,chr = "cchr3")
xp4m <- mutate(xp4,chr = "dchr4")
xp5m <- mutate(xp5,chr = "echr5")
xp6m <- mutate(xp6,chr = "fchr6")
xp7m <- mutate(xp7,chr = "gchr7")
xp8m <- mutate(xp8,chr = "hchr8")
xp9m <- mutate(xp9,chr = "ichr9")
xp10m <- mutate(xp10,chr = "jchr10")
xp11m <- mutate(xp11,chr = "kchr11")
xp12m <- mutate(xp12,chr = "lchr12")


allm<-rbind(xp1m,xp2m,xp3m,xp4m,xp5m,xp6m,xp7m,xp8m,xp9m,xp10m,xp11m,xp12m)

allmn <- allm %>% mutate(pos = row_number())

Mean   :  63.384

write.csv(allmn,"AI5_BI1_xp_all.csv")

allmn_removeNA <- allmn %>% select(chrom,start,nSNPs,xpclr)%>%filter(!is.na(xpclr))

allmn_removeNA %>% 
  do(data.frame(t(quantile(.$xpclr, probs = c(0.10, 0.30, 0.50, 0.80,0.90, 0.95, 0.98, 0.99)))))

X10. X30.      X50.     X80.     X90.    X95.     X98.     X99.
1    0    0 0.8719836 85.07991 244.2332 375.316 521.3445 610.5851

subset99all<- subset(allmn,xpclr >610.5851)

dim(subset99all)
[1] 373  15


subset95all<- subset(allmn,xpclr >375.316)
dim(subset95all)

write.csv (subset95all,"subsetAI5_BI1_95.csv")

[1] 863  15
subset500all<- subset(allmn,xpclr >500)
dim(subset500all)

write.csv (subset500all,"subsetAI5_BI1_500.csv")

p <- ggplot(data=allmn,aes(x=pos, y=xpclr,color=chr))

pg  <- p + geom_point(size=0.3) + xlab("chromosome") + ylab("XPCLR score")
pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))

pgs  <- pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))

pdf(file = "AI5_BI1_selection.pdf",width=12,height=2)
pgs
dev.off()



pgsl <- pgs + geom_hline(yintercept=610.5851, linetype="dashed", color = "blue",size=0.3) + ylim(0,1000)

pdf(file = "AI5_BI1_selection_99_scale_1000.pdf",width=12,height=2)
pgsl
dev.off()

pgsl <- pgs + geom_hline(yintercept=500, linetype="dashed", color = "blue",size=0.3) + ylim(0,1000)

pdf(file = "AI5_BI1_selection_500_scale_1000.pdf",width=12,height=2)
pgsl
dev.off()

######### plot fst

allfst<- read.csv("I5vsI1_meanFSTgene.csv")
head(allfst)

p <- ggplot(data=allfst,aes(x=pos, y=fst,color=chr))

pg  <- p + geom_point(size=0.3) + xlab("chromosome") + ylab("gene fst")
pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))

pgs  <- pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))

pdf(file = "I5vsI1genefst.pdf",width=12,height=2)
pgs
dev.off()


fst_nozero <-  filter(allfst, fst > 0)
dim(fst_nozero)
[1] 41785     7

write.table(fst_nozero, "fst_nozero.txt", sep = "\t", quote=F)

fst_nozero %>% 
  do(data.frame(t(quantile(.$fst, probs = c(0.10, 0.30, 0.50, 0.80,0.90, 0.95, 0.98, 0.99)))))

X10.      X30.      X50.      X80.     X90.      X95.      X98.    X99.
1 0.02194071 0.0807559 0.1592375 0.3702095 0.515163 0.6404743 0.7628486 0.83259


fst95 <-  filter(fst_nozero,fst >= 0.6404743)
dim(fst95)
2090

fst90 <-  filter(fst_nozero,fst >= 0.515163)
dim(fst90)
4179

fst05 <-  filter(fst_nozero,fst >= 0.5)
dim(fst05)
4507
write.table(fst05, "fst05.txt", sep = "\t", quote=F)

write.table(fst95, "fst95.txt", sep = "\t", quote=F)
write.table(fst90, "fst90.txt", sep = "\t", quote=F)





### plot fst xpclr

xp_fst <- read.csv ("AI5_BI1_xpclr_fst_no.csv")

p <- ggplot(data=xp_fst,aes(x=mean_xpclr, y=mean_fst_snp))
p + geom_point()

cor(xp_fst$mean_xpclr,xp_fst$mean_fst_snp)

cor(xp_fst$mean_fst_gene,xp_fst$mean_fst_snp)
cor(xp_fst$mean_xpclr,xp_fst$mean_fst_gene)

p <- ggplot(data=xp_fst,aes(x=mean_xpclr, y=mean_fst_gene))
p + geom_point()

pg  <- p + geom_point(size=0.3) + xlab("chromosome") + ylab("gene fst")


linearMod <- lm(mean_fst_gene ~ mean_xpclr, data=xp_fst) 
linearMod <- lm(mean_xpclr ~ mean_fst_gene, data=xp_fst) 


xp_fst <- read.csv ("AI5_BI1_95_xpclr_fst.csv")


### fst per chr per chr

allfst<- read.csv("I5vsI1_meanFSTgene.csv")
head(allfst)

fst1 <- subset(allfst,chr=='achr1')
summary(fst1)

fst1 <- subset(allfst,chr=='achr1')
summary(fst1)

fst2 <- subset(allfst,chr=='bchr2')
summary(fst2)

fst3 <- subset(allfst,chr=='cchr3')
summary(fst3)

fst4 <- subset(allfst,chr=='dchr4')
summary(fst4)

fst5 <- subset(allfst,chr=='echr5')
summary(fst5)

fst6 <- subset(allfst,chr=='fchr6')
summary(fst6)

fst7 <- subset(allfst,chr=='gchr7')
summary(fst7)

fst8 <- subset(allfst,chr=='hchr8')
summary(fst8)

fst9 <- subset(allfst,chr=='ichr9')
summary(fst9)

fst10 <- subset(allfst,chr=='jchr10')
summary(fst10)

fst11 <- subset(allfst,chr=='kchr11')
summary(fst11)

fst12 <- subset(allfst,chr=='lchr12')
summary(fst12)




xp_fst <- read.csv ("AI5_BI1_95_xpclr_fst.csv")


### fst chr per chr

allfst<- read.table("no_nan_popI1_vs_popI5snp.weir.fst", header=TRUE)
head(allfst)

fst1 <- subset(allfst,CHROM=='1')
summary(fst1)

fst2 <- subset(allfst,CHROM=='2')
summary(fst2)

fst3 <- subset(allfst,CHROM=='3')
summary(fst3)


fst4 <- subset(allfst,CHROM=='4')
summary(fst4)

fst5 <- subset(allfst,CHROM=='5')
summary(fst5)

fst6 <- subset(allfst,CHROM=='6')
summary(fst6)

fst7 <- subset(allfst,CHROM=='7')
summary(fst7)
fst8 <- subset(allfst,CHROM=='8')
summary(fst8)
fst9 <- subset(allfst,CHROM=='9')
summary(fst9)
fst10 <- subset(allfst,CHROM=='10')
summary(fst10)

fst11 <- subset(allfst,CHROM=='11')
summary(fst11)

fst12 <- subset(allfst,CHROM=='12')
summary(fst12)


fst4 <- subset(allfst,chr=='dchr4')
summary(fst4)

fst5 <- subset(allfst,chr=='echr5')
summary(fst5)

fst6 <- subset(allfst,chr=='fchr6')
summary(fst6)

fst7 <- subset(allfst,chr=='gchr7')
summary(fst7)

fst8 <- subset(allfst,chr=='hchr8')
summary(fst8)

fst9 <- subset(allfst,chr=='ichr9')
summary(fst9)

fst10 <- subset(allfst,chr=='jchr10')
summary(fst10)

fst11 <- subset(allfst,chr=='kchr11')
summary(fst11)

fst12 <- subset(allfst,chr=='lchr12')
summary(fst12)



subset439all<- subset(allmn,xpclr >=439)
dim(subset439all)
1270 
write.csv (subset439all,"subsetAI5_BI1_439.csv")

p <- ggplot(data=allmn,aes(x=pos, y=xpclr,color=chr))

pg  <- p + geom_point(size=0.3) + xlab("chromosome") + ylab("XPCLR score")
pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))

pgs  <- pg + scale_color_manual(values=c("#f58231", "#3cb44b", "#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b","#f58231", "#3cb44b"))

pgsl <- pgs + geom_hline(yintercept=439, linetype="dashed", color = "blue",size=0.3) + ylim(0,2000)

pdf(file = "AI5_BI1_selection_439_scale_2000.pdf",width=12,height=2)
pgsl
dev.off()

pgsl <- pgs + geom_hline(yintercept=439, linetype="dashed", color = "blue",size=0.3) + ylim(0,1500)

pdf(file = "AI5_BI1_selection_439_scale_1500.pdf",width=12,height=2)
pgsl
dev.off()


