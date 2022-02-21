library(pca3d)
library(RColorBrewer)
library(reshape)
library(ggplot2)
library(dplyr)
library(tidyr)

pop_info <- read.csv("../143_metadata_13_clusters.csv")

## need to read this in again otherwise it retains species that are no longer presesnt
AA<-read.csv("190sam_musa_acumv4_DP10_300_MISS3_MAF_011.csv",header=T)

pca_pops <- left_join(pop_info, AA, by = "Taxa")

pop_type <-pca_pops$our_group
table(pop_type)

pop_type_col <-pca_pops$new_group_col
table(pop_type_col)

pdf(file = "143_newgp_187133_AA.pdf", width = 10, height = 8)
pca2d(as.matrix(pca_pops[,20:21]),show.plane = T,show.labels = FALSE,col=pop_type_col,bg="grey90",radius = 1)
legend("topright",legend= c("cl1_Sucrier","cl2_Cavendish","cl3_GrosMichel","cl4_GrosMichel","cl5_Red","cl6_AAAA","cl7_Mutika_Lujugira","cl8_Plantain-like","cl9_AAAB","cl10_MaiaMaoli_Popoulu","cl11_Plantain"),
       col=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3"),cex=1,pch=16,
       box.lty=1, box.col="black")
dev.off()


pdf(file = "143_newgp_187133_AA_label.pdf", width = 80, height = 70)
pca2d(as.matrix(pca_pops[,20:21]),show.plane = T,show.labels = pca_pops$Taxa,col=pop_type_col,bg="grey90",radius = 1)
legend("topright",legend= c("cl1_Sucrier","cl2_Cavendish","cl3_GrosMichel","cl4_GrosMichel","cl5_Red","cl6_AAAA","cl7_Mutika_Lujugira","cl8_Plantain-like","cl9_AAAB","cl10_MaiaMaoli_Popoulu","cl11_Plantain"),
       col=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3"),cex=1,pch=16,
       box.lty=1, box.col="black")
dev.off()



pdf(file = "143_newgp_187133_AA_PC23.pdf", width = 10, height = 8)
pca2d(as.matrix(pca_pops[,21:22]),show.plane = T,show.labels = FALSE,col=pop_type_col,bg="grey90",radius = 1)
legend("topright",legend= c("cl1_Sucrier","cl2_Cavendish","cl3_GrosMichel","cl4_GrosMichel","cl5_Red","cl6_AAAA","cl7_Mutika_Lujugira","cl8_Plantain-like","cl9_AAAB","cl10_MaiaMaoli_Popoulu","cl11_Plantain"),
       col=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3"),cex=1,pch=16,
       box.lty=1, box.col="black")
dev.off()




pdf(file = "146_newgp_187133_AA_PC34.pdf", width = 10, height = 8)
pca2d(as.matrix(pca_pops[,22:23]),show.plane = T,show.labels = FALSE,col=pop_type_col,bg="grey90",radius = 1)
legend("topleft",legend= c("cl1_Sucrier","cl2_Cavendish","cl3_GrosMichel","cl4_GrosMichel","cl5_Red","cl6_AAAA","cl7_Mutika_Lujugira","cl8_Plantain-like","cl9_AAAB","cl10_MaiaMaoli_Popoulu","cl11_Plantain"),
       col=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3"),cex=1,pch=16,
       box.lty=1, box.col="black")
dev.off()
