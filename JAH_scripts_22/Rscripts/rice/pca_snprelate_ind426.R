library(gdsfmt)
library(SNPRelate)
library(pca3d)

library(RColorBrewer)
#display.brewer.all()

library(dplyr)
library(tidyr)


### on cluster  

vcf.fn <- "ind426_SNP_beagle_MAF05.recode.vcf"
snpgdsVCF2GDS(vcf.fn, "ind426_SNP_beagle_MAF05.recode.gds", method="biallelic.only")
import 2027294 variants.
### on cluster  

genofile <- snpgdsOpen("ind426_SNP_beagle_MAF05.recode.gds")

sample.id <- read.gdsn(index.gdsn(genofile, "sample.id"))

pca<-snpgdsPCA(genofile,autosome.only = FALSE,sample.id = sample.id)

head(round(pca$varprop*100, 2))
[1] 7.94 6.91 4.86 3.75 2.64 1.72

pop5<-read.csv("indica_both_pops_for_pca_426_IRI.csv")
head(pop5)

order <- data.frame(pca$sample.id)
colnames(order)<- "Taxa"

pops <- left_join(order, pop5, by = "Taxa")

dim(pops)
pop_ind56 <-pops[,2]
table(pop_ind56)
I1  I2  I3  I4  I5  Im 
145  91  37  62  43  48 

pop_sy <-pops$shape
table(pop_sy) 

cube sphere 
47    379 

palette(c("blue","darkorchid","forestgreen","darkorange","red","#696969"))

pdf(file = "ind_2027294_col.pdf", width = 8, height =6);
pca2d(pca$eigenvect,show.plane = T,radius = 1,show.labels = FALSE,  title="jap233", bg="lightgray", col=pop_ind56, shape = pop_sy)
legend("bottomleft",legend= c("I1","I2","I3","I4","I5","Im"),
       col=c("blue","darkorchid","forestgreen","darkorange","red","#696969"),cex=0.9,pch=16,
       box.lty=1, box.col="black")
dev.off()


pdf(file = "ind_2027294_col_name.pdf", width = 30, height =20);
pca2d(pca$eigenvect,show.plane = T,radius = 1,show.labels = pops$Taxa,  title="jap233", bg="lightgray", col=pops$pop_col, shape = pop_sy)
legend("bottomleft",legend= c("I1","I2","I3","I4","I5","Im"),
       col=c("blue","darkorchid","forestgreen","darkorange","red","#696969"),cex=0.9,pch=16,
       box.lty=1, box.col="black")
dev.off()



These option show a confidence interval (CI) ellipse (2D) or ellipsoid (3D). The option ellipse.ci corresponds to the level for which the CI is supposed to be drawn (default 0.95). A less conservative level (e.g. 0.75) will result in a morre narrow ellipse. For the 3D representation, it is worthwile to remove the grey plane at z=0 with the show.plane=FALSE option.
Biplots
A biplot shows information on variable loadings, which helps to interpret a PCA. To see a biplot, use the biplot parameter. If TRUE, then variable loadings will be extracted from the princomp or prcomp object. If biplot is a matrix, then it is assumed to be a matrix of variable loadings.
The second option is biplot.vars, which describes how many variables are shown on the plot. The top N variables are selected based on their absolute loadings.
pca2d(pca$eigenvect, group=gr, biplot=TRUE, biplot.vars=3)
pca3d(pca, group=gr, show.ellipses=TRUE, ellipse.ci=0.75, show.plane=FALSE)
snapshotPCA3d(file="ellipses.png")

gr <- factor(pop_ind56)

pdf(file = "ind_426_2027294_ellipse.pdf", width = 8, height =6);
pca2d(pca$eigenvect,show.plane = T,radius = 0.8,group=gr,show.ellipses=TRUE,ellipse.ci=0.95,show.labels = FALSE,  title="jap233", bg="lightgray", col=pop_ind56,shape = pop_sy)
legend("bottomleft",legend= c("I1","I2","I3","I4","I5","Im"),
       col=c("blue","darkorchid","forestgreen","darkorange","red","#696969"),cex=0.9,pch=16,
       box.lty=1, box.col="black")
dev.off()

