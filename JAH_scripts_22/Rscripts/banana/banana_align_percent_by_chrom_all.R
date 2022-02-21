library(reshape)
library(ggplot2)

library(dplyr)
library(tidyr)

### input alignment stats for all references plus genotype and subtype metadata
## see details for mean alignment plots

data <- read.csv("all_ABB_AA_BB_stats.csv")
str(data)
dim(data)

table(data$our_group)

mycolors_change <- c("dodgerblue","darkolivegreen","firebrick3")

plan <- subset(data,data$our_group == "k_Plantain_ABB")

write.csv(plan, "plan.csv")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "plantain_align_percent.pdf",width=16,height=12)
p5
dev.off()


plan <- subset(data,data$our_group == "a_Sucrier_AA")

order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "AA_Sucrier_align_percent.pdf",width=16,height=12)
p5
dev.off()


plan <- subset(data,data$our_group == "b_Cavendish_AAA")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "cavendish_align_percent.pdf",width=12,height=12)
p5
dev.off()

plan <- subset(data,data$our_group == "c_GrosMichel_AAA")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "c_GrosMichel_AAA_percent.pdf",width=8,height=12)
p5
dev.off()

plan <- subset(data,data$our_group == "d_GrosMichel-like_AAA")
#write.csv(plan, "d_AAA_GrosMichel_like")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "d_AAA_GrosMichel_like_percent.pdf",width=6,height=12)
p5
dev.off()


plan <- subset(data,data$our_group == "e_Red_AAA")

order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "e_Red_AAA_percent.pdf",width=8,height=12)
p5
dev.off()

plan <- subset(data,data$our_group == "f_AAAA")

write.csv(plan, "f_cl6_AAAA")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "f_AAAA_align_percent.pdf",width=6,height=12)
p5
dev.off()

plan <- subset(data,data$our_group == "g_Mutika_AAA")

order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "g_Mutika_AAA_percent.pdf",width=6,height=12)
p5
dev.off()





plan <- subset(data,data$our_group == "h_geno_unknown")

write.csv(plan, "h_cl8_Plantain-like")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "h_geno_unknown_align_percent.pdf",width=6,height=12)
p5
dev.off()



plan <- subset(data,data$our_group == "i_AAAB")

write.csv(plan, "i_AAAB")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "i_AAAB_align_percent.pdf",width=6,height=12)
p5
dev.off()


plan <- subset(data,data$our_group == "j_Popoulu_ABB")
write.csv(plan, "j_Popoulu_ABB.csv")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "AAB_Popoulu_align_percent.pdf",width=6,height=12)
p5
dev.off()

plan <- subset(data,data$our_group == "l_Bluggoe_ABB")

write.csv(plan, "l_Bluggoe_ABB.csv")
order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "Bluggoe_ABB_align_percent.pdf",width=6,height=12)
p5
dev.off()



plan <- subset(data,data$our_group == "wild_species")

order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "wild_species_align__percent.pdf",width=4,height=12)
p5
dev.off()

plan <- subset(data,data$our_group == "m_Pelipita_ABB")

order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")
plan$chrom <- factor(plan$chrom, levels = order)

### line plot of alignment stats
p<-ggplot(plan,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "ABB_Pelipita_align_percent.pdf",width=12,height=12)
p5
dev.off()

table(data$our_group)

plan$chrom <- factor(plan$chrom, levels = order)

plan <- subset(data,data$our_group == "x_cl_unknown")

write.csv(plan, "x_cl_unknown.csv")

## put unknown into the correct order

un_order <- read.csv("unknown35_order.csv")

unknown_order <- inner_join(un_order,plan, by = "Taxa")
dim(unknown_order)

order <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11")


unknown_order$chrom <- factor(unknown_order$chrom, levels = order)
unknown_order$Taxa <- factor(unknown_order$Taxa, levels = un_order$Taxa)


### line plot of alignment stats
p<-ggplot(unknown_order,aes(Taxa,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~chrom)

pdf(file = "unknown_align_percent_chrom.pdf",width=16,height=12)
p5
dev.off()



### line plot of alignment stats
p<-ggplot(unknown_order,aes(chrom,percent))
p1 <- p + geom_line(col = "grey60")
p2 <- p1 + geom_point(aes(colour=genome))
p3 <- p2 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=6)) + xlab("") + ylab("percentage of properly paired reads mapped to each chromosome")
p4 <- p3 +  scale_color_manual(values=mycolors_change)
p5 <- p4 + facet_wrap(~Taxa)

pdf(file = "unknown_align_percent_taxa.pdf",width=16,height=12)
p5
dev.off()


