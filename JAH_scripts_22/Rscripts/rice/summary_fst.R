library(dplyr)
library(tidyr)

I5genes <- read.delim("I5_gene_weir.fst.txt",header = FALSE)

I5genes_values <- subset(I5genes, V5 != ".")

colMeans(I5genes_values)


#put together gene FST with function

enrich <- read.csv ("all_enriched_unique_list.csv",header=T)
phyto<- read.csv ("all_results.csv",header=T)
fun <- read.csv ("geneInfo_FunRiceGenes.csv",header=T)
I5_list <- read.csv ("I5_gene_weir.fst.csv",header=T)

j1 <- left_join(I5_list,enrich,by = "gene")
j2 <- left_join(j1,phyto,by = "gene")
j3 <- left_join(j2,fun,by = "gene")

write.csv(j3, "annotateI5genes.csv")


#select snps



#put together gene FST with function

enrich <- read.csv ("all_enriched_unique_list.csv",header=T)
phyto<- read.csv ("all_results.csv",header=T)
fun <- read.csv ("geneInfo_FunRiceGenes.csv",header=T)

all_genes <- read.csv ("I5vsI234_mean_FST_per_gene.csv",header=T)
I5_list <- read.table ("I5genes.txt",header=F)
colnames(I5_list) <- "gene"

j1 <- left_join(I5_list,all_genes,by = "gene")
j2 <- left_join(j1,phyto,by = "gene")
j3 <- left_join(j2,fun,by = "gene")

j4 <- left_join(j3,fun,by = "gene")

write.csv(j3, "annotateI5genes.csv")


get mean gene fst for regions
fst <- read.csv ("annotateI5_fst_genes.csv",header=T)

dim(fst)
[1] 4576   18

nvfst <- read.csv ("annotateI5_fst_genes_no_value.csv",header=T)

dim(nvfst)

3767   18

mean_fst_region <- nvfst %>%
  group_by(region) %>%
  summarise(
    n = n(),
    mean_gene_fst = mean(mean_fst))

write.csv(mean_fst_region, "mean_fst_regionI5genes.csv")


#now add high impact and phytozome enricheda to fst


enrich <- read.csv ("all_enriched_unique_list.csv",header=T)
high<- read.csv ("7396_HIGH_genes.csv",header=T)


j1 <- left_join(fst,enrich,by = "gene")
j2 <- left_join(j1,high,by = "gene")

write.csv(j2, "annotateI5_mean_fst_genes.csv")

