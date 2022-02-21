library(reshape)
library(ggplot2)

library(dplyr)
library(tidyr)

A1<-read.csv ("GWASPlot_extended.csv")
chrl<-read.csv("chr_length.csv")
cm <- read.csv("Rice_Centromere.csv")

DI_FP_2 <-slice(A1,1L)
GL_FP_2 <-slice(A1,2L)
GL_Jap_2_1 <-slice(A1,3L)
GL_Jap_2_2 <-slice(A1,4L)
GW_jap_3_1 <-slice(A1,5L)
GL_FP_3 <-slice(A1,6L)
GL_Ind_3 <-slice(A1,7L)
LW_jap_3_1 <-slice(A1,8L)
GW_jap_3_2 <-slice(A1,9L)
GW_FP_3 <-slice(A1,10L)
LW_FP_3 <-slice(A1,11L)
LW_jap_3_2 <-slice(A1,12L)
GW_jap_3_3 <-slice(A1,13L)
GL_jap_3 <-slice(A1,14L)
GL_FP_4 <-slice(A1,15L)
HD_FP_4 <-slice(A1,16L)
PL_FP_5 <-slice(A1,17L)
GW_FP_5 <-slice(A1,18L)
GW_Ind_5 <-slice(A1,19L)
LW_Ind_5 <-slice(A1,20L)
GL_FP_6_1 <-slice(A1,21L)
GL_Ind_6 <-slice(A1,22L)
GL_FP_6_2 <-slice(A1,23L)
GL_ind_6 <-slice(A1,24L)
GL_FP_6_3 <-slice(A1,25L)
PL_FP_6 <-slice(A1,26L)
GL_jap_7 <-slice(A1,27L)
FP_FP_8_1 <-slice(A1,28L)
FP_FP_8_2 <-slice(A1,29L)
FP_FP_9 <-slice(A1,30L)
HD_FP_9 <-slice(A1,31L)
GW_jap_10 <-slice(A1,32L)
LW_FP_12 <-slice(A1,33L)






p <- ggplot(data=chrl, aes(chr, length),colour=chr) + geom_bar( stat="identity", fill="grey85") + coord_flip() + theme_classic()
p1 <- p + geom_segment(data=DI_FP_2, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p2 <- p1 + geom_segment(data=DI_FP_2, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p3 <- p2 + geom_segment(data=GL_FP_2, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p4 <- p3 + geom_segment(data=GL_Jap_2_1, aes(x=chrom-0.15,xend=chrom-0.15,y=start, yend=end), size=0.7, colour="red")
p5 <- p4 + geom_segment(data=GL_FP_2, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p6 <- p5 + geom_segment(data=GL_Jap_2_2, aes(x=chrom-0.15,xend=chrom-0.15,y=start, yend=end), size=0.7, colour="red")
p7 <- p6 + geom_segment(data=GL_Jap_2_2, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p8 <- p7 + geom_segment(data=GW_jap_3_1, aes(x=chrom-0.15,xend=chrom-0.15,y=start, yend=end), size=0.7, colour="red")
p9 <- p8 + geom_segment(data=GW_jap_3_1, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p10 <- p9 + geom_segment(data=GL_FP_3, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid4")
p11 <- p10 + geom_segment(data=GL_Ind_3, aes(x=chrom,xend=chrom,y=start, yend=end), size=0.7, colour="dodgerblue2")
p12 <- p11 + geom_segment(data=LW_jap_3_1, aes(x=chrom-0.15,xend=chrom-0.15,y=start, yend=end), size=0.7, colour="red")
p13 <- p12 + geom_segment(data=GW_jap_3_2, aes(x=chrom-0.30,xend=chrom-0.30,y=start, yend=end), size=0.7, colour="red")
p14 <- p13 + geom_segment(data=GL_FP_3, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p15 <- p14 + geom_segment(data=GW_FP_3, aes(x=chrom+0.30,xend=chrom+0.30,y=start, yend=end), size=0.7, colour="darkorchid4")
p16 <- p15 + geom_segment(data=LW_FP_3, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid4")
p17 <- p16 + geom_segment(data=LW_jap_3_2, aes(x=chrom,xend=chrom,y=start, yend=end), size=0.7, colour="red")
p18 <- p17 + geom_segment(data=GW_jap_3_3, aes(x=chrom-0.15,xend=chrom-0.15,y=start, yend=end), size=0.7, colour="red")
p19 <- p18 + geom_segment(data=GL_jap_3, aes(x=chrom-0.30,xend=chrom-0.30,y=start, yend=end), size=0.7, colour="red")
p20 <- p19 + geom_segment(data=GW_FP_3, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p22 <- p20 + geom_segment(data=GL_FP_4, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p23 <- p22 + geom_segment(data=GL_FP_4, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p24 <- p23 + geom_segment(data=HD_FP_4, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p25 <- p24 + geom_segment(data=HD_FP_4, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p26 <- p25 + geom_segment(data=PL_FP_5, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p27 <- p26 + geom_segment(data=PL_FP_5, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p28 <- p27 + geom_segment(data=GW_FP_5, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p29 <- p28 + geom_segment(data=GW_Ind_5, aes(x=chrom,xend=chrom,y=start, yend=end), size=0.7, colour="dodgerblue2")
p30 <- p29 + geom_segment(data=LW_Ind_5, aes(x=chrom,xend=chrom,y=start, yend=end), size=0.7, colour="dodgerblue2")
p31 <- p30 + geom_segment(data=GW_FP_5, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p32 <- p31 + geom_segment(data=GL_FP_6_1, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p33 <- p32 + geom_segment(data=GL_Ind_6, aes(x=chrom,xend=chrom,y=start, yend=end), size=0.7, colour="dodgerblue2")
p34 <- p33 + geom_segment(data=GL_FP_6_1, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p35 <- p34 + geom_segment(data=GL_FP_6_2, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p36 <- p35 + geom_segment(data=GL_ind_6, aes(x=chrom,xend=chrom,y=start, yend=end), size=0.7, colour="dodgerblue2")
p37 <- p36 + geom_segment(data=GL_FP_6_2, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p38 <- p37 + geom_segment(data=GL_FP_6_3, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p39 <- p38 + geom_segment(data=GL_FP_6_3, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p40 <- p39 + geom_segment(data=PL_FP_6, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p41 <- p40 + geom_segment(data=PL_FP_6, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p42 <- p41 + geom_segment(data=GL_jap_7, aes(x=chrom-0.15,xend=chrom-0.15,y=start, yend=end), size=0.7, colour="red")
p43 <- p42 + geom_segment(data=GL_jap_7, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p44 <- p43 + geom_segment(data=FP_FP_8_1, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p45 <- p44 + geom_segment(data=FP_FP_8_1, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta",alpha=0.3)

p46 <- p45 + geom_segment(data=FP_FP_8_2, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p47 <- p46 + geom_segment(data=FP_FP_8_2, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p48 <- p47 + geom_segment(data=FP_FP_9, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p49 <- p48 + geom_segment(data=FP_FP_9, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p50 <- p49 + geom_segment(data=HD_FP_9, aes(x=chrom+0.15,xend=chrom+0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p51 <- p50 + geom_segment(data=HD_FP_9, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p52 <- p51 + geom_segment(data=GW_jap_10, aes(x=chrom-0.15,xend=chrom-0.15,y=start, yend=end), size=0.7, colour="red")
p53 <- p52 + geom_segment(data=GW_jap_10, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p54<- p53 + geom_segment(data=LW_FP_12, aes(x=chrom-0.15,xend=chrom-0.15,y=start, yend=end), size=0.7, colour="darkorchid3")
p55 <- p54 + geom_segment(data=LW_FP_12, aes(x=chrom,xend=chrom,y=qstart, yend=qend), size=7, colour="magenta", alpha=0.3)

p60 <- p55 + geom_segment(data=cm, aes(x=chromosome,xend=chromosome,y=start, yend=end), size=7, colour="grey50")

pdf(file = "GWAS_extended.pdf",width=9,height=4)
p60
dev.off()
 

plot.new()

legend("bottomleft",legend= c("Full Panel","Indica Panel","Japonica Panel"),
       col=c("darkorchid3","dodgerblue2","red"),cex=2,pch=15,
       box.lty=1, box.col="black")

