/tgac/workarea/group-cg/higginsj/rice/higginsj_rice/Gene_annot_qtl/allele_plot_2020

head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "5_719563" 672_all_SNP_hapmap.txt.hmp.txt > 5_719563.txt
cat header.txt 5_719563.txt > 5_719563_head.txt


allele <- read.delim("5_719563_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X5_719563")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "5_719563.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X5_719563)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os05g02260 5_719563")
dev.off()


head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "8_5278838" 672_all_SNP_hapmap.txt.hmp.txt > 8_5278838.txt
cat header.txt 8_5278838.txt > 8_5278838_head.txt


allele <- read.delim("8_5278838_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X8_5278838")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "8_5278838.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X8_5278838)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os08g09110 8_5278838")
dev.off()



head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "3_6907102" 672_all_SNP_hapmap.txt.hmp.txt > 3_6907102.txt
cat header.txt 3_6907102.txt > 3_6907102_head.txt




allele <- read.delim("3_6907102_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X3_6907102")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "3_6907102.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X3_6907102)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os03g12840 3_6907102")
dev.off()


head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "1_38192458" 672_all_SNP_hapmap.txt.hmp.txt > 1_38192458.txt
cat header.txt 1_38192458.txt > 1_38192458_head.txt




allele <- read.delim("1_38192458_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X1_38192458")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "1_38192458.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X1_38192458)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os01g65770 1_38192458")
dev.off()


head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "4_34939233" 672_all_SNP_hapmap.txt.hmp.txt > 4_34939233.txt
cat header.txt 4_34939233.txt > 4_34939233_head.txt




allele <- read.delim("4_34939233_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X4_34939233")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "4_34939233.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X4_34939233)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os04g58740 4_34939233")
dev.off()






head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "11_5026584" 672_all_SNP_hapmap.txt.hmp.txt > 11_5026584.txt
cat header.txt 11_5026584.txt > 11_5026584_head.txt




allele <- read.delim("11_5026584_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X11_5026584")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "11_5026584.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X11_5026584)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os11g09360 OsFBX398 11_5026584")
dev.off()




head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "11_5424384" 672_all_SNP_hapmap.txt.hmp.txt > 11_5424384.txt
cat header.txt 11_5424384.txt > 11_5424384_head.txt




allele <- read.delim("11_5424384_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X11_5424384")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "11_5424384.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X11_5424384)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os11g10070 OsSEU2 11_5424384")
dev.off()



head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "4_35019178" 672_all_SNP_hapmap.txt.hmp.txt > 4_35019178.txt
cat header.txt 4_35019178.txt > 4_35019178_head.txt




allele <- read.delim("4_35019178_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X4_35019178")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "4_35019178.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X4_35019178)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os04g58870 4_35019178")
dev.off()






head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "6_1559993" 672_all_SNP_hapmap.txt.hmp.txt > 6_1559993.txt
cat header.txt 6_1559993.txt > 6_1559993_head.txt




allele <- read.delim("6_1559993_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X6_1559993")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "6_1559993.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X6_1559993)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os06g03860 6_1559993")
dev.off()



4	34939233	4_34939233	A	G	.	PASS	ANN=G|start_lost|HIGH|LOC_Os04g58740|L




head -1 672_all_SNP_hapmap.txt.hmp.txt > header.txt
grep "10_19045026" 672_all_SNP_hapmap.txt.hmp.txt > 10_19045026.txt
cat header.txt 10_19045026.txt > 10_19045026_head.txt




allele <- read.delim("10_19045026_head.txt", header=T, stringsAsFactors=FALSE, colClasses = c("character"))
dalle <- as.data.frame(allele)
rdalle <- dalle[,-c(1,2,3,4,5,6,7,8,9,10,11)]
talle <- t(rdalle)
dtalle <- as.data.frame(talle)
alle <- tibble::rownames_to_column(dtalle, var = "sample")
colnames(alle) <- c("Taxa","X10_19045026")

pop_rat <- left_join(pops,alle, by = "Taxa")

pdf(file = "10_19045026.pdf",width=6,height=6)
p <- ggplot(data=pop_rat, aes(subpops1, fill=X10_19045026)) +
  geom_bar(stat = "count",
           position = "stack")
p + ggtitle("LOC_Os10g35604 10_19045026")
dev.off()


