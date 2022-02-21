library(ggplot2)
library("lattice")
library(reshape)
library(dplyr)
library(tidyr)
library(plyr)

pheno <- read.csv("abb_final_viet672_GWAS_web_irri_addweb_may2019_pop.csv",row.names = 1)
table(pheno$subpops)

ji <- subset(pheno,pheno$Japonica.Indica.pop == "Indica" | pheno$Japonica.Indica.pop == "Japonica")

ind15 <- subset(pheno,pheno$subpops == "I1" | pheno$subpops == "I5")

# Plot grain length
# calculate mean
mu <- ddply(ji, "Japonica.Indica.pop", summarise, grp.mean=mean(GrL,na.rm = TRUE))
head(mu)

# Plot grain length
p <- ggplot(ji, aes(x=GrL, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.2)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Grain Length(mm)", y = "Density")+
  xlim(6,12) +
  theme(legend.position = "none")

pdf(file = "jap_ind_grain_length.pdf",width=6,height=6)
p
dev.off()




# Plot grain length with legend
p <- ggplot(ji, aes(x=GrL, color=Japonica.Indica.pop, fill=Japonica.Indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.2)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Japonica.Indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Grain Length(mm)", y = "Density")+
  xlim(6,12) +
  theme(legend.position = "right")

pdf(file = "legend_jap_ind_grain_length.pdf",width=6,height=6)
p
dev.off()




# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(GrL,na.rm = TRUE))
head(mu15)

# Plot grain length
p <- ggplot(ind15, aes(x=GrL, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.2)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Grain Length(mm)", y = "Density")+
  xlim(6,12) +
  theme(legend.position = "none")

pdf(file = "ind15_grain_length.pdf",width=6,height=6)
p
dev.off()

pdf(file = "legendind15_grain_length.pdf",width=6,height=6)
p
dev.off()

# Plot grain width
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(GrW,na.rm = TRUE))
head(mu)

# Plot grain width
p <- ggplot(ji, aes(x=GrW, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.05)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Grain Width(mm)", y = "Density")+
  xlim(1.5,4.5) +
  theme(legend.position = "none")

pdf(file = "jap_ind_grain_width.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(GrW,na.rm = TRUE))
head(mu15)

# Plot grain width
p <- ggplot(ind15, aes(x=GrW, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.05)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Grain Width(mm)", y = "Density")+
  xlim(1.5,4.5) +
  theme(legend.position = "none")

pdf(file = "ind15_grain_width.pdf",width=6,height=6)
p
dev.off()


************************Plot HD
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(HD,na.rm = TRUE))
head(mu)

# Plot grain width
p <- ggplot(ji, aes(x=HD, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=1.0)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Heading Date (days)", y = "Density")+
  xlim(70,220) +
  theme(legend.position = "none")

pdf(file = "jap_ind_headingDate.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(HD,na.rm = TRUE))
head(mu15)

# Plot grain width
p <- ggplot(ind15, aes(x=HD, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=2)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Heading Date (days)", y = "Density")+
  xlim(70,220) +
  theme(legend.position = "none")

pdf(file = "ind15_headingDate.pdf",width=6,height=6)
p
dev.off()




#**********************Plot Culm Strengh
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(CmS,na.rm = TRUE))
head(mu)

# Plot Culm Strengh
p <- ggplot(ji, aes(x=CmS, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Culm Strengh(*)", y = "Density")+
  xlim(1,9) +
  theme(legend.position = "none")

pdf(file = "jap_ind_Culm_Strength.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(CmS,na.rm = TRUE))
head(mu15)

# Plot Culm Strengh
p <- ggplot(ind15, aes(x=CmS, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Culm Strengh(*)", y = "Density")+
  xlim(1,9) +
  theme(legend.position = "none")

pdf(file = "ind15_Culm_Strength.pdf",width=6,height=6)
p
dev.off()


#******************* Plot Leaf Length
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(LL,na.rm = TRUE))
head(mu)

# Plot Leaf Length
p <- ggplot(ji, aes(x=LL, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Leaf Length(cm)", y = "Density")+
  xlim(20,90) +
  theme(legend.position = "none")

pdf(file = "jap_ind_Leaf_Length.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(LL,na.rm = TRUE))
head(mu15)

# Plot Leaf Length
p <- ggplot(ind15, aes(x=LL, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=1)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Leaf Length(cm)", y = "Density")+
  xlim(20,90) +
  theme(legend.position = "none")

pdf(file = "ind15_Leaf_Length.pdf",width=6,height=6)
p
dev.off()



***********************# Plot Leaf Width
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(LW,na.rm = TRUE))
head(mu)

# Plot Leaf Width
p <- ggplot(ji, aes(x=LW, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.05)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Leaf Width(cm)", y = "Density")+
  xlim(0.5,2.2) +
  theme(legend.position = "none")

pdf(file = "jap_ind_Leaf_Width.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(LW,na.rm = TRUE))
head(mu15)

# Plot Leaf Width
p <- ggplot(ind15, aes(x=LW, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.05)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Leaf Width(cm)", y = "Density")+
  xlim(0.5,2.2) +
  theme(legend.position = "none")

pdf(file = "ind15_Leaf_Width.pdf",width=6,height=6)
p
dev.off()



*****************# Plot Leaf Blade
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(LBP,na.rm = TRUE))
head(mu)

# Plot Leaf Blade
p <- ggplot(ji, aes(x=LBP, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.1)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Leaf Blade(cm)", y = "Density")+
  xlim(0.5,4.5) +
  theme(legend.position = "none")

pdf(file = "jap_ind_Leaf_Blade.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(LBP,na.rm = TRUE))
head(mu15)

# Plot Leaf Blade
p <- ggplot(ind15, aes(x=LBP, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.1)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Leaf Blade(cm)", y = "Density")+
  xlim(0.5,4.5) +
  theme(legend.position = "none")

pdf(file = "ind15_Leaf_Blade.pdf",width=6,height=6)
p
dev.off()




****************# Plot Culm Number
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(CmN,na.rm = TRUE))
head(mu)

# Plot Culm Number
p <- ggplot(ji, aes(x=CmN, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.1)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Culm Number(cm)", y = "Density")+
  xlim(1,15) +
  theme(legend.position = "none")

pdf(file = "jap_ind_Culm_Number.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(CmN,na.rm = TRUE))
head(mu15)

# Plot Culm Number
p <- ggplot(ind15, aes(x=CmN, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.2)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Culm Number(cm)", y = "Density")+
  xlim(1,15) +
  theme(legend.position = "none")

pdf(file = "ind15_Culm_Number.pdf",width=6,height=6)
p
dev.off()




*********************# Plot Culm Length
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(CmL,na.rm = TRUE))
head(mu)

# Plot Culm Length
p <- ggplot(ji, aes(x=CmL, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=2)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Culm Length(cm)", y = "Density")+
  xlim(20,220) +
  theme(legend.position = "none")

pdf(file = "jap_ind_Culm_Length.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(CmL,na.rm = TRUE))
head(mu15)

# Plot Culm Length
p <- ggplot(ind15, aes(x=CmL, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=2)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Culm Length(cm)", y = "Density")+
  xlim(20,220) +
  theme(legend.position = "none")

pdf(file = "ind15_Culm_Length.pdf",width=6,height=6)
p
dev.off()





******************# Plot Panicle Length
# calculate mean
mu <- ddply(ji, "japonica.indica.pop", summarise, grp.mean=mean(PnL,na.rm = TRUE))
head(mu)

# Plot Panicle Length
p <- ggplot(ji, aes(x=PnL, color=japonica.indica.pop, fill=japonica.indica.pop)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=japonica.indica.pop),
             linetype="dashed")+
  scale_color_manual(values=c("green4", "purple1"))+
  scale_fill_manual(values=c("green4", "purple1"))+
  labs(x="Panicle Length(cm)", y = "Density")+
  xlim(15,40) +
  theme(legend.position = "none")

pdf(file = "jap_ind_Panicle_Length.pdf",width=6,height=6)
p
dev.off()

# calculate mean
mu15 <- ddply(ind15, "subpops", summarise, grp.mean=mean(PnL,na.rm = TRUE))
head(mu15)

# Plot Panicle Length
p <- ggplot(ind15, aes(x=PnL, color=subpops, fill=subpops)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu15, aes(xintercept=grp.mean, color=subpops),
             linetype="dashed")+
  scale_color_manual(values=c( "royalblue","red"))+
  scale_fill_manual(values=c( "royalblue","red"))+
  labs(x="Panicle Length(cm)", y = "Density")+
  xlim(15,40) +
  theme(legend.position = "none")

pdf(file = "ind15_Panicle_Length.pdf",width=6,height=6)
p
dev.off()


