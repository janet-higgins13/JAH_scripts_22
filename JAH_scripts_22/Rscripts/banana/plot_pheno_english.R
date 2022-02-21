library("GGally")
library("lattice")
library(reshape)
library(ggplot2)

library(dplyr)
library(tidyr)

library(broom)
library(dplyr)
library(purrr)


data <- read.csv("gp_pheno_english_cl.csv")

str(data)

table(data$our_group)


dataNA <- cbind(data,"nas" = rowSums(is.na(data)))

write.csv (dataNA,"dataNA.csv")


cdataNA <- colSums(is.na(data))

write.csv (cdataNA,"col_dataNA.csv")


## plot subpops
p<-ggplot(data,aes(our_group,pseudostem_height))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))

p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))


pdf(file = "pseudostem_height.pdf",width=8,height=8)
p3
dev.off()

## plot subpops
p<-ggplot(data,aes(our_group,pseudostem_circumference))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "pseudostem_circumference.pdf",width=8,height=8)
p3
dev.off()


## plot subpops
p<-ggplot(data,aes(our_group,petiole_margin_width))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "petiole_margin_width..pdf",width=8,height=8)
p3
dev.off()


## plot subpops
p<-ggplot(data,aes(our_group,leaf_length))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "leaf_length.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,leaf_width))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "leaf_width.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,petiole_length))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "petiole_length.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,peduncle_length))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "peduncle_length.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,scars_on_rachis))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "scars_on_rachis.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,peduncle_circumference))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "peduncle_circumference.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,male_bud_circumference))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "male_bud_circumference.pdf",width=8,height=8)
p3
dev.off()



p<-ggplot(data,aes(our_group,Fruit_length))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "Fruit_length.pdf",width=8,height=8)
p3
dev.off()



p<-ggplot(data,aes(our_group,Fruit_pedicel_width))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "Fruit_pedicel_width.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,skin_thickness))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "skin_thickness.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,pulp_dry_matter))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "pulp_dry_matter.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,skin_dry_matter))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "skin_dry_matter.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,flowering_time))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "flowering_time.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,pH))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "pH.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,brix ))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "brix.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,acid))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "acid.pdf",width=8,height=8)
p3
dev.off()


p<-ggplot(data,aes(our_group,ash))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "ash.pdf",width=8,height=8)
p3
dev.off()


p<-ggplot(data,aes(our_group,racimo_weight ))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "racimo_weight .pdf",width=8,height=8)
p3
dev.off()


p<-ggplot(data,aes(our_group,peduncle_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "peduncle_weight.pdf",width=8,height=8)
p3
dev.off()


p<-ggplot(data,aes(our_group,rachis_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "rachis_weight.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,male_flower_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "male_flower_weight.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,all_bunches_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))

pdf(file = "all_bunches_weight.pdf",width=8,height=8)
p1
dev.off()

p<-ggplot(data,aes(our_group,bunch_number))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "bunch_number.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,Fruits_per_bunch))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "Fruits_per_bunch.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,bunch_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "bunch_weight.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,bunch_pulp_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "bunch_pulp_weight.pdf",width=8,height=8)
p3
dev.off()


p<-ggplot(data,aes(our_group,percent_pulp_bunch))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "percent_pulp_bunch.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,one_banana_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "one_banana_weight.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,one_banana_pulp_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "one_banana_pulp_weight.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,one_banana_skin_weight))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "one_banana_skin_weight.pdf",width=8,height=8)
p3
dev.off()

p<-ggplot(data,aes(our_group,one_banana_width))
p1 <- p + geom_boxplot(outlier.shape = NA) + geom_jitter(size=2,width = 0.05,aes(colour=our_group)) + theme(text = element_text(size=12,angle=90))
p3 <- p1 + scale_color_manual(values=c("#f032e6","#3cb44b","#9A6324","orange","#e6194B","#911eb4","#42d4f4","darkgreen","#800000","orangered3","blue3","darkblue","#469990","orchid3","grey40"))

pdf(file = "one_banana_width.pdf",width=8,height=8)
p3
dev.off()


