library(reshape)
library(ggplot2)

library(dplyr)
library(tidyr)

### input alignment stats for all references plus genotype and subtype metadata

### change colour of references
data <- read.csv("align_stats_AA_BB_ABB_13gp.csv")
str(data)
dim(data)


mycolors_change <- c("dodgerblue","forestgreen","firebrick3")



### boxplot of alignment stats
p<-ggplot(data,aes(our_group,percent_pp))

p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=1.5, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10))

p3 <- p2 +  scale_color_manual(values=mycolors_change)

pdf(file = "align_AA_BB_ABB_gp13.pdf",width=12,height=8)
p3
dev.off()







data <- read.csv("align_stats_allv4Letter_13gp.csv")
str(data)
dim(data)

### boxplot of alignment stats
p<-ggplot(data,aes(our_group,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10))

pdf(file = "align_gp13.pdf",width=10,height=6)
p2
dev.off()



balb <- subset(data, data$reference == "balb_DH_PKW")
dim(balb)
table(balb$our_group)


### boxplot of alignment stats
p<-ggplot(data,aes(reference,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 35, hjust = 1, size=10))

pdf(file = "align_4ref.pdf",width=8,height=6)
p2
dev.off()

### boxplot of alignment stats by genotype
p<-ggplot(data,aes(MGIS_AGROSAVIA,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 35, hjust = 1, size=10))

pdf(file = "align_3ref_geno.pdf",width=8,height=6)
p2
dev.off()

### boxplot of alignment stats by species
p<-ggplot(data,aes(SPECIES,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 35, hjust = 1, size=10))

pdf(file = "align_3ref_species.pdf",width=8,height=6)
p2
dev.off()


### boxplot of alignment stats by subtype

p<-ggplot(data,aes(Sub_ref,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 35, hjust = 1, size=10))

pdf(file = "align_subcatv3.pdf",width=8,height=6)
p2
dev.off()

### boxplot for all samples in a group

plantain <- subset(data,our_group == "l_cl11_Plantain")

p <-ggplot(plantain,aes(SEQUENCING_ID,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90)

pdf(file = "align_cl11_Plantain.pdf",width=12,height=6)
p2
dev.off()


plantain <- subset(data,our_group == "a_cl1_Sucrier")

p <-ggplot(plantain,aes(SEQUENCING_ID,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90)

pdf(file = "align_a_cl1_Sucrier.pdf",width=12,height=6)
p2
dev.off()



plantain <- subset(data,our_group == "b_cl2_Cavendish")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl2_Cavendish.pdf",width=6,height=6)
p2
dev.off()


plantain <- subset(data,our_group == "f_cl6_AAAA")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl6_AAAA.pdf",width=3,height=6)
p2
dev.off()



plantain <- subset(data,our_group == "c_cl3_GrosMichel")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl3_GrosMichel.pdf",width=3,height=6)
p2
dev.off()


plantain <- subset(data,our_group == "c_cl4_GrosMichel")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl4_GrosMichel.pdf",width=3,height=6)
p2
dev.off()




plantain <- subset(data,our_group == "d_cl5_Red")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl5_Red.pdf",width=4,height=6)
p2
dev.off()



plantain <- subset(data,our_group == "g_cl7_Mutika_Lujugira")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl7_Mutika_Lujugira.pdf",width=3,height=6)
p2
dev.off()


plantain <- subset(data,our_group == "i_cl9_AAAB")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl9_AAAB.pdf",width=3,height=6)
p2
dev.off()


plantain <- subset(data,our_group == "k_cl10_MaiaMaoli_Popoulu")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl10_MaiaMaoli_Popoulu.pdf",width=3,height=6)
p2
dev.off()

plantain <- subset(data,our_group == "p_cl13_ABB_Pelipita")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_cl13_ABB_Pelipita.pdf",width=3,height=6)
p2
dev.off()


plantain <- subset(data,our_group == "n_gp12_ABB_Bluggoe")

p <-ggplot(plantain,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_gp12_ABB_Bluggoe.pdf",width=3,height=6)
p2
dev.off()





plantaino <- subset(data,our_group == "h_c8_plantain-like")

p <-ggplot(plantaino,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.05,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(60,90) + xlab("") + ylab("")

pdf(file = "align_c8_plantain-like.pdf",width=3,height=6)
p2
dev.off()

unknown <- subset(data,our_group == "wild_species")

write.csv(unknown, "unknown.csv")

## change_order

unknown <- read.csv("unknown_order.csv")

p <-ggplot(unknown,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=1, width = 0.1,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(50,90) + xlab("") + ylab("")

pdf(file = "wild_species.pdf",width=6,height=6)
p2
dev.off()



p <-ggplot(unknown,aes(SEQUENCING_ID ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=1, width = 0.1,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(50,90) + xlab("") + ylab("")

pdf(file = "wild_species2.pdf",width=6,height=6)
p2
dev.off()



unknown <- subset(data,our_group == "x_gp_unknown")
write.csv(unknown,"unknown.csv")
## order unclustered


unknown <- read.csv("unknown_align.csv")
str(unknown)
dim(unknown)

balb <- subset(unknown,unknown$reference=="balb_DH_PKW")

unknown$Taxa <- factor(unknown$Taxa, levels = balb$Taxa)

p <-ggplot(unknown,aes(Taxa ,percent_pp))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2, width = 0.1,aes(colour=reference)) + theme(text = element_text(size=10))

p2 <- p1 + theme(axis.text.x=element_text(angle = 90, hjust = 1, size=10)) + ylim(55,95) + xlab("") + ylab("")

pdf(file = "align_unknown.pdf",width=10,height=6)
p2
dev.off()

