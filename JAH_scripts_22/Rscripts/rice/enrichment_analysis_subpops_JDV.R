setwd("~/OneDrive - Norwich BioScience Institutes/XP_CLR_paper/jose_genes_EA")

library(topGO,quietly = T)
library(data.table,quietly = T)
library(dplyr)
options(scipen=999)

run.topgo.pipeline.BP <- function(mytemp) {
  list.mytemp <- factor(as.integer(allgenes$V1 %in% mytemp$V1))
  names(list.mytemp) <- allgenes$V1
  #BP
  fdata.mytemp.BP <- new("topGOdata", ontology="BP", allGenes=list.mytemp, annot = annFUN.gene2GO, gene2GO = geneID2GO)
  results.fdata.mytemp.BP <- GenTable(fdata.mytemp.BP, weight01_fisher = runTest(fdata.mytemp.BP, algorithm = "weight01", statistic = "fisher"), topNodes=100)
  results.fdata.mytemp.BP  
}



allgenes <- read.delim(file ="id_all_annotated_genes.txt", header = FALSE)
topgo.file <- read.delim("agrigo_item2term_53.txt_fortopgo.txt", header = F, sep = '\t')
geneID2GO <- readMappings(file = "agrigo_item2term_53.txt_fortopgo.txt", sep = '\t', IDsep = ',')



my.i5 <- data.frame()
for (i in 1:52) {
name <- paste0("I5_",i,".txt")
my.file <- read.delim(file = name, header = FALSE)
my.run <- run.topgo.pipeline.BP(my.file) 
my.run$set <- name
my.sign <- my.run #%>% filter(weight01_fisher<0.05)
my.i5 <- rbind(my.sign, my.i5)
}
write.table(my.i5,file = "results_I5.txt")


my.i4 <- data.frame()
for (i in 1:38) {
  name <- paste0("I4_",i,".txt")
  my.file <- read.delim(file = name, header = FALSE)
  my.run <- run.topgo.pipeline.BP(my.file) 
  my.run$set <- name
  my.sign <- my.run #%>% filter(weight01_fisher<0.05)
  my.i4 <- rbind(my.sign, my.i4)
}
write.table(my.i4,file = "results_I4.txt")


my.i3 <- data.frame()
for (i in 1:42) {
  name <- paste0("I3_",i,".txt")
  my.file <- read.delim(file = name, header = FALSE)
  my.run <- run.topgo.pipeline.BP(my.file) 
  my.run$set <- name
  my.sign <- my.run #%>% filter(weight01_fisher<0.05)
  my.i3 <- rbind(my.sign, my.i3)
}
write.table(my.i3,file = "results_I3.txt")


my.i2 <- data.frame()
for (i in 1:41) {
  name <- paste0("I2_",i,".txt")
  my.file <- read.delim(file = name, header = FALSE)
  my.run <- run.topgo.pipeline.BP(my.file) 
  my.run$set <- name
  my.sign <- my.run #%>% filter(weight01_fisher<0.05)
  my.i2 <- rbind(my.sign, my.i2)
}
write.table(my.i2,file = "results_I2.txt")


my.i1 <- data.frame()
for (i in 1:44) {
  name <- paste0("I1_",i,".txt")
  my.file <- read.delim(file = name, header = FALSE)
  my.run <- run.topgo.pipeline.BP(my.file) 
  my.run$set <- name
  my.sign <- my.run #%>% filter(weight01_fisher<0.05)
  my.i1 <- rbind(my.sign, my.i1)
}
write.table(my.i1,file = "results_I1.txt")



my.j1 <- data.frame()
for (i in 1:28) {
  name <- paste0("J1_",i,".txt")
  my.file <- read.delim(file = name, header = FALSE)
  my.run <- run.topgo.pipeline.BP(my.file) 
  my.run$set <- name
  my.sign <- my.run #%>% filter(weight01_fisher<0.05)
  my.j1 <- rbind(my.sign, my.j1)
}
write.table(my.j1,file = "results_J1.txt")


my.j2 <- data.frame()
for (i in 1:23) {
  name <- paste0("J2_",i,".txt")
  my.file <- read.delim(file = name, header = FALSE)
  my.run <- run.topgo.pipeline.BP(my.file) 
  my.run$set <- name
  my.sign <- my.run #%>% filter(weight01_fisher<0.05)
  my.j2 <- rbind(my.sign, my.j2)
}
write.table(my.j2,file = "results_J2.txt")


my.j3 <- data.frame()
for (i in 1:24) {
  name <- paste0("J3_",i,".txt")
  my.file <- read.delim(file = name, header = FALSE)
  my.run <- run.topgo.pipeline.BP(my.file) 
  my.run$set <- name
  my.sign <- my.run #%>% filter(weight01_fisher<0.05)
  my.j3 <- rbind(my.sign, my.j3)
}
write.table(my.j3,file = "results_J3.txt")



my.j4 <- data.frame()
for (i in 1:25) {
  name <- paste0("J4_",i,".txt")
  my.file <- read.delim(file = name, header = FALSE)
  my.run <- run.topgo.pipeline.BP(my.file) 
  my.run$set <- name
  my.sign <- my.run #%>% filter(weight01_fisher<0.05)
  my.j4 <- rbind(my.sign, my.j4)
}
write.table(my.j4,file = "results_J4.txt")



#PLOT
library(ggplot2)
library(RColorBrewer)
library(viridis)
table.join <- read.csv(file="export_process_results_to_plot.txt", header = T, sep = '\t')
table.join <- table.join %>% filter (genes>=10)  %>% filter(min_weight01_fisher<=0.01) 

p2 <- ggplot(data=table.join, aes(x=subpopulation,y=reorder(Term,desc(Term))))
p2 <- p2 + geom_point(data=table.join, aes(x=subpopulation, y=reorder(Term,desc(Term)), size=genes, colour=min_weight01_fisher),  alpha=1) +
  geom_point(data=table.join, aes(x=subpopulation, y=reorder(Term,desc(Term)), size=genes), shape = 1, colour = "darkgrey")
p2 <- p2 + scale_size(breaks = c(5,10,15,20,25,30,40,50,75,100), range = (c(3,13)))
p2 <- p2 + scale_color_gradientn(colours = rev(brewer.pal(9,"YlGn")))
p2 <- p2 + theme_linedraw()
p2

#using number of regions for colouring instead:
p2 <- ggplot(data=table.join, aes(x=subpopulation,y=reorder(Term,desc(Term))))
p2 <- p2 + geom_point(data=table.join, aes(x=subpopulation, y=reorder(Term,desc(Term)), size=genes, colour=regions),  alpha=1) +
  geom_point(data=table.join, aes(x=subpopulation, y=reorder(Term,desc(Term)), size=genes), shape = 1, colour = "darkgrey") 
p2 <- p2 + scale_size(breaks = c(5,10,15,20,25,30,40,50,75,100), range = (c(3,13)))
p2 <- p2 + scale_color_gradientn(colours = brewer.pal(9,"Greens"))
p2 <- p2 + theme_linedraw()
p2

