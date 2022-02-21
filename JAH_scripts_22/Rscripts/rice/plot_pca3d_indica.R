## pca analysis
library(pca3d)
library(dplyr)

ind <- read.csv("no_adm_1605_IndicaForPCA3d.csv")
dim(ind)
[1] 1605    7

PC1,4,5

#components 1 to 3
inda <- as.matrix(ind[,4:6])
pca3d(inda)

[1] 2.993328 3.168771 1.124258



# 3d plot with shape and colour
pca3d(inda,col=ind$colour,radius =0.8)

[1] 2.8296962 0.6109258 2.8448517

snapshotPCA3d(file="ind1605.png")
