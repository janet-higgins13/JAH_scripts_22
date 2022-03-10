library("RColorBrewer")
library("ggplot2")

library(dplyr)
library(tidyr)
library(stringr)

# script to read in coverage files  - median coverage over 100,000bp windows for A adn B genome
# normalise by mean
# calculate mean coverage for A and B genome - store in list and output as dataframe
# calculate mean coverage for A and B genome per chromsome - output as csv
# do AB plots for each sample

# read in list of samples 
pop <- read.table("cl7_Mutika.txt")

slist <- pop$V1
str(slist)

# initialise empty lists
mean_list <- c()
normA_list <- c()
normB_list <- c()
Amean_list <- c()
Bmean_list <- c()

# read in coverage file for each sample
# output mean coverage over each chromosoem as csv
# plot normalised coverage by A genome
# plot normalised coverage by B genome

for (sam in slist) {
  name <- paste("../../individual_median_cov/",sam,"_median_cov.txt",sep = "")
  print (name)
  all<- read.delim (name, header = FALSE)
  colnames(all) <- c("chrom","start","end","cov")
  head(all)
  # to set window with no coverage of 0, these are input as "." 
  all$cov <- as.numeric(all$cov)
  all$cov[is.na(all$cov)] <- 0
  print (sam)
  colnames(all) <- c("chrom","start","end","cov")
  cov_chr <- all %>% group_by(chrom) %>%
  summarize(cov = mean(cov))
  ccname <- paste(sam,"_chr_cov.csv",sep = "")
  write.csv(cov_chr,ccname)
  ma <- mean(all$cov)
  print(ma)
  mean_list <- append(mean_list,c(ma))
  all$covNormByMean <- all$cov/mean(all$cov)  
  check <- mean(all$covNormByMean)
  print(check)
  # get A genome chromosomes
  Achr01<- all %>% filter(grepl('^chr', chrom))
  maA <-  mean(Achr01$cov)
  normA_list <- append(normA_list,c(maA))
  maAn <- mean(Achr01$covNormByMean)
  Amean_list <- append(Amean_list,c(maAn))
  # get B genome chromosomes
  Bchr01<- all %>% filter(grepl('^Bchr', chrom))
  maB <- mean(Bchr01$cov)
  normB_list <- append(normB_list,c(maB))
  maBn <- mean(Bchr01$covNormByMean)
  Bmean_list <- append(Bmean_list,c(maBn))
  
  ## make B negative and remove B from chromosome
  all.neg <- function(x) -1*abs(x)
  
  Bchr01neg <- cbind(Bchr01,"cov_neg" = all.neg(Bchr01$covNormByMean))
  
  Bnes <- Bchr01neg %>%
    mutate(chrom = str_replace(chrom,"^Bchr","chr"))
  
  #print (head(Bnes))
  #print (head(Bchr01))

  #######################
  ###replot with syntheny
  #######################
  #read minimap alignments:
  minimap <- read.delim("../../BB_100kbwindows-over-AAv4.longest_alignmentotal.paf", header=F)
  minimap <- minimap[,c(1,6,8,9)]
  colnames(minimap) <- c("uniqID","inAA_chr","inAA_start","inAA_stop")
  #create id field in Bchr01neg
  Bchr01neg$uniqID <- paste0(Bchr01neg$chrom,":",Bchr01neg$start,"-",Bchr01neg$end)
  
  #join left both dataframes
  Bchr01negJOIN <- merge(x=Bchr01neg, y=minimap, by="uniqID", all.x = FALSE) #false to ignore BBs without AA homologous
  
  Achr01$facet <- Achr01$chrom
  Bchr01negJOIN$facet <- Bchr01negJOIN$inAA_chr
  
  aname <- paste(sam,"cov_byA.pdf",sep = "")
  
  print (aname)
  print (head(Achr01))
  
  p <- ggplot() + geom_bar(data=Achr01,aes(x=start,y=covNormByMean), stat = "identity",color="dodgerblue")
  pg <- p + geom_bar(data=Bchr01negJOIN,aes(x=inAA_start,y=cov_neg), stat = "identity",color="firebrick3")
  pgx <- pg + coord_cartesian(ylim = c(-2,2)) + theme(axis.text = element_text(size = 5))  
  pgxl <- pgx + geom_hline(yintercept=0, linetype="solid", color = "gray40")
  pgxk <- pgxl + geom_hline(yintercept=c(1,2), linetype="solid", color = "gray30", alpha=0.3)
  pgxm <- pgxk + geom_hline(yintercept=c(-1,-2), linetype="solid", color = "gray30", alpha=0.3)
  pgsw <- pgxm + facet_wrap(~facet,ncol=1,strip.position = "right")
  pgs <- pgsw + theme_classic()
  
  pdf(file = aname,width =8, height = 12)
  plot(pgs)
  dev.off()
  

  ###SAME WITH BB AS REFERENCE (ALSO SWAPPING TOP AND BOTTOM BUT NOT COLOURS):
  
  Achr01neg <- cbind(Achr01,"cov_neg" = all.neg(Achr01$covNormByMean))
  
  minimapBB <- read.delim("../../AAv4_100kbwindows-over-BB.paf.longest_alignmentotal.paf", header=F)
  minimapBB <- minimapBB[,c(1,6,8,9)]
  colnames(minimapBB) <- c("uniqID","inBB_chr","inBB_start","inBB_stop")
  #create id field in Bchr01neg
  Achr01neg$uniqID <- paste0(Achr01neg$chrom,":",Achr01neg$start,"-",Achr01neg$end)
  
  #join left both dataframes
  Achr01negJOIN <- merge(x=Achr01neg, y=minimapBB, by="uniqID", all.x = FALSE) #false to ignore BBs without AA homologous
  Achr01negJOIN$facet <- Achr01negJOIN$inBB_chr
  
  Bchr01$facet <- Bchr01$chrom
  
  bname <- paste(sam,"cov_byB.pdf",sep = "")
  
  p <- ggplot() + geom_bar(data=Bchr01,aes(x=start,y=covNormByMean), stat = "identity",color="firebrick3")
  pg <- p + geom_bar(data=Achr01negJOIN,aes(x=inBB_start,y=cov_neg), stat = "identity",color="dodgerblue")
  pgx <- pg + coord_cartesian(ylim = c(-2,2)) + theme(axis.text = element_text(size = 5))  
  pgxl <- pgx + geom_hline(yintercept=0, linetype="solid", color = "gray40")
  pgxk <- pgxl + geom_hline(yintercept=c(1,2), linetype="solid", color = "gray30", alpha=0.3)
  pgxm <- pgxk + geom_hline(yintercept=c(-1,-2), linetype="solid", color = "gray30", alpha=0.3)
  pgsw <- pgxm + facet_wrap(~facet,ncol=1,strip.position = "right")
  pgs <- pgsw + theme_classic()
  
  pdf(file = bname,width =8, height = 12)
  plot(pgs)
  dev.off()

}



# make dataframe from lists and output as csv
new_df <- as.data.frame(slist)
results <- cbind(new_df,mean_list,normA_list,Amean_list,normB_list,Bmean_list)
write.csv(results,"Mutika_meanAB_results.csv")

# check lists are all the same length
length(slist)
length(mean_list)
length(normA_list)
length(normB_list)
length(Amean_list)
length(Bmean_list)
