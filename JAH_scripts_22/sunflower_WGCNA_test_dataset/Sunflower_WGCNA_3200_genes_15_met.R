library(WGCNA)
options(stringsAsFactors = FALSE);
# input expression data

#**************************************************************
## normalise kallisto counts using deseq
#library(DESeq2)

#countsTable<-read.csv("sunflower_3200genes.csv",header = TRUE , row.names=1)
#dim(countsTable)

#colData <- read.csv("colDat_kallisto.csv")

#filter <- apply(countsTable, 1, function(x) length(x[x>200])>=2) ##filter out non expressed genes by requiring more than 20 reads in at least 2 samples for each gene.
#filtered <- countsTable[filter,]
#dim(filtered)

#dds <- DESeqDataSetFromMatrix(countData = filtered, colData = colData, design = ~ condition)
#dds <- DESeq(dds)

#vsd <- assay(vst(dds, blind=FALSE))
#write.csv(vsd, file="sunflower_3200_vst.csv")
#********************************************************************

#START OF WGCNA ANALYSIS
  
#Read in the data set
sundata<-read.csv("sunflower_3200_vst.csv",header = TRUE , row.names=1)

# Take a quick look at what is in the data set:
dim(sundata)
names(sundata)
 
datExpr <- as.data.frame(t(sundata))

dim(datExpr)

gsg <- goodSamplesGenes(datExpr, verbose = 3);
gsg$allOK

#remove bad genes
if (!gsg$allOK)
{if (sum(!gsg$goodGenes)>0)
  printFlush(paste("Removing genes:", paste(names(datExpr)[!gsg$goodGenes], collapse= ", ")));
  if (sum(!gsg$goodSamples)>0)
    printFlush(paste("Removing samples:", paste(rownames(datExpr)[!gsg$goodSamples], collapse=", ")))
  datExpr= datExpr[gsg$goodSamples, gsg$goodGenes]
}

#input trait data
allTraits <- read.csv("Sunflower_small_met_traits.csv",header=TRUE)
Samples = rownames(datExpr);
traitRows = match(Samples, allTraits$SAMPLENAME)
datTraits = allTraits[traitRows, -1];
rownames(datTraits) = allTraits[traitRows, 1];
collectGarbage();

# Re-cluster samples
sampleTree2 = hclust(dist(datExpr), method = "average")

# Convert traits to a color representation: white means low, red means high, grey means missing entry
traitColors = numbers2colors(datTraits, signed = FALSE);
# Plot the sample dendrogram and the colors underneath.
pdf("orderedtraitClustering.pdf",width = 10, height = 12)
plotDendroAndColors(sampleTree2, traitColors,
groupLabels = names(datTraits),
main = "Sample dendrogram and trait heatmap")
dev.off()

save(datExpr, datTraits, file = "Sunflower_3200_met_Input.RData")

###load data from Data_Input.R
lnames = load(file = "Sunflower_3200_met_Input.RData")
lnames

# Choose a set of soft-thresholding powers
powers = c(c(1:10), seq(from = 12, to=20, by=2))

# Call the network topology analysis function
sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)

# Plot the results:
pdf(file = "SoftThreshold.pdf", width = 12, height = 9);
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
main = paste("Scale independence"));
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
labels=powers,cex=1,col="red");
# this line corresponds to using an R^2 cut-off of h
abline(h=0.90,col="red")
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=1,col="red")
dev.off()


##############

# make the network

net = blockwiseModules(datExpr, power = 10,
                       networkType = "signed",TOMType = "signed", minModuleSize = 30,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "TOM", 
                       verbose = 3)


moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];
save(MEs, moduleLabels, moduleColors, geneTree, 
     file = "sunflower_3200_met_networkConstruction-auto.RData")



pdf(file = "dendrogram_met.pdf", width = 12, height = 9)
# Convert labels to colors for plotting
mergedColors = labels2colors(net$colors)
# Plot the dendrogram and the module colors underneath
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
"Module colors",
dendroLabels = FALSE, hang = 0.03,
addGuide = TRUE, guideHang = 0.05)
dev.off()

table(net$colors)
colors <- table(net$colors)
write.table(colors,"colours.txt",quote = FALSE,sep="\t")

moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];
save(MEs, moduleLabels, moduleColors, geneTree,file = "sunflower_3200_met_networkConstruction-auto.RData")


hubs<-chooseTopHubInEachModule(datExpr,moduleColors,omitColors = "grey",power = 6)
hubsdf<-as.data.frame(hubs)
write.table (hubsdf,"tophubs.txt",sep="\t")

nGenes = ncol(datExpr);
nSamples = nrow(datExpr);
# Recalculate MEs with color labels
MEs0 = moduleEigengenes(datExpr, moduleColors)$eigengenes
MEs = orderMEs(MEs0)
moduleTraitCor = cor(MEs, datTraits, use = "p");
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples);


pdf(file = "ordered_relateToTraits_met.pdf", width = 12, height = 8)
# Will display correlations and their p-values
textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
signif(moduleTraitPvalue, 1), ")", sep = "");
dim(textMatrix) = dim(moduleTraitCor)
#par(mar = c(8, 10, 3, 3));
par(mar = c(6, 8.5, 3, 3));
# Display the correlation values within a heatmap plot
labeledHeatmap(Matrix = moduleTraitCor,
xLabels = names(datTraits),
yLabels = names(MEs),
ySymbols = names(MEs),
colorLabels = FALSE,
#colors = greenWhiteRed(50),
colors = blueWhiteRed(50),
textMatrix = textMatrix,
setStdMargins = FALSE,
cex.text = 0.6,
zlim = c(-1,1),
main = paste("Module-trait relationships"))
dev.off()


## Find module trait relationships with a low P value
## plot module membership vs gene significance for these traits

## Trait - metabolite = alanine
weight = as.data.frame(datTraits$alanine);
names(weight) = "weight"
# names (colors) of the modules
modNames = substring(names(MEs), 3)
geneModuleMembership = as.data.frame(cor(datExpr, MEs, use = "p"));
MMPvalue = as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples));
names(geneModuleMembership) = paste("MM", modNames, sep="");
names(MMPvalue) = paste("p.MM", modNames, sep="");
geneTraitSignificance = as.data.frame(cor(datExpr, weight, use = "p"));
GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples));
names(geneTraitSignificance) = paste("GS.", names(weight), sep="");
names(GSPvalue) = paste("p.GS.", names(weight), sep="");

#output the number of genes in each module
colours<- table(moduleColors)
write.table(colours,"colours.txt",sep="\t")

#output list of genes and module colours
outMC<-cbind(GSPvalue,moduleColors)
outMC<-outMC[-1]
write.table(outMC,"module_colours.txt",quote = FALSE,sep="\t")

#Choose a module
module = "blue"
column = match(module, modNames);
moduleGenes = moduleColors==module;
pdf(file = "alanineVSblue.pdf", width = 8, height = 8)
par(mfrow = c(1,1));
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for alanine",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)
dev.off()



#Choose a module
module = "yellow"
column = match(module, modNames);
moduleGenes = moduleColors==module;
pdf(file = "alanineVSyellow.pdf", width = 8, height = 8)
par(mfrow = c(1,1));
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for alanine",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)
dev.off()

## Trait - metabolite = glucose
weight = as.data.frame(datTraits$glucose);
names(weight) = "weight"
# names (colors) of the modules
modNames = substring(names(MEs), 3)
geneModuleMembership = as.data.frame(cor(datExpr, MEs, use = "p"));
MMPvalue = as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples));
names(geneModuleMembership) = paste("MM", modNames, sep="");
names(MMPvalue) = paste("p.MM", modNames, sep="");
geneTraitSignificance = as.data.frame(cor(datExpr, weight, use = "p"));
GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples));
names(geneTraitSignificance) = paste("GS.", names(weight), sep="");
names(GSPvalue) = paste("p.GS.", names(weight), sep="");

#Choose a module
module = "purple"
column = match(module, modNames);
moduleGenes = moduleColors==module;
pdf(file = "glucoseVSpurple.pdf", width = 8, height = 8)
par(mfrow = c(1,1));
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for glucose",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)
dev.off()


#Choose a module
module = "turquoise"
column = match(module, modNames);
moduleGenes = moduleColors==module;
pdf(file = "Ant_R463VSturquoise.pdf", width = 8, height = 8)
par(mfrow = c(1,1));
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for Ant_R463",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)
dev.off()


## Trait - metabolite =  asparagine
weight = as.data.frame(datTraits$asparagine);
names(asparagine) = "asparagine"
# names (colors) of the modules
modNames = substring(names(MEs), 3)
geneModuleMembership = as.data.frame(cor(datExpr, MEs, use = "p"));
MMPvalue = as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples));
names(geneModuleMembership) = paste("MM", modNames, sep="");
names(MMPvalue) = paste("p.MM", modNames, sep="");
geneTraitSignificance = as.data.frame(cor(datExpr, weight, use = "p"));
GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples));
names(geneTraitSignificance) = paste("GS.", names(weight), sep="");
names(GSPvalue) = paste("p.GS.", names(weight), sep="");

#Choose a module
module = "blue"
column = match(module, modNames);
moduleGenes = moduleColors==module;
pdf(file = "asparagineVSblue.pdf", width = 8, height = 8)
par(mfrow = c(1,1));
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for asparagine",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)
dev.off()



###OPTIONAL
###### plot module membership, eigengenes for each condition

traitData2 <- read.delim ("Sunflower_traits_cond.txt",header=TRUE)

# Get module menbership, eigengenes etc.
moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];

moduleTraitCor = cor(MEs, traitData2, use = "p")
Warning message:
In cor(MEs, traitData2, use = "p") : NAs introduced by coercion 
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples)
moduleTraitPvalue

textMatrix = paste(signif(moduleTraitCor, 2), "\n(",signif(moduleTraitPvalue, 1), ")", sep = "") 
dim(textMatrix) = dim(moduleTraitCor)

pdf("power10_network_module_trait_cor.pdf",width = 10, height = 7)
par(mar = c(6, 8.5, 3, 3))
labeledHeatmap(Matrix = moduleTraitCor,xLabels = names(traitData2),yLabels = names(MEs),ySymbols = names(MEs),colorLabels = FALSE,colors = greenWhiteRed(50),textMatrix = textMatrix,setStdMargins = FALSE,cex.text = 0.5,zlim = c(-1,1),main = paste("Module-trait relationships"))
dev.off()

distance = 1-(1+cor(MEs,use="p"))/2 
MDS = cmdscale(as.dist(distance),2)
pdf("power10_network_module_MDS.pdf", width=7, height=7)
plot(MDS, col=moduleColors,cex=3,pch = 19) 
#plot(MDS,cex=3,pch = 19) 
dev.off()

# Calulate eigengenes for each module
MEs0 = moduleEigengenes(datExpr, moduleColors)$eigengenes # Order by correlation between eigengenes
MEs = orderMEs(MEs0)
# Organise data to plot eignegenes
library(reshape) 

as.data.frame(MEs) -> MEs_frame
MEs_frame$ID <- traitData2$SAMPLENAME
MEs_frame$condition <- traitData2$condition
melt(MEs_frame) -> MEs_frame_melt

head(MEs_frame_melt)

library(ggplot2)
pdf("power10_module_condition.pdf",width = 10, height = 7)
g1 <- ggplot(MEs_frame_melt,aes(y=value,x=ID,colour=condition)) + geom_point()+ xlab("Sample") + facet_wrap(~variable) 
g1 + theme(axis.text.x = element_blank()) + guides(colour = guide_legend(title = "Senescence"))
dev.off()

## output MEs
write.csv(MEs_frame,"MES_frame.csv")

######## end of eigengenes plot
###OPTIONAL


### more plots from WGCNA tutorial
 
 lnames = load(file = "sunflower_3200_met_networkConstruction-auto.RData")
 lnames
 
 Code chunk 2
 #
 #=====================================================================================
 
 
 # Calculate topological overlap anew: this could be done more efficiently by saving the TOM
 # calculated during module detection, but let us do it again here.
 dissTOM = 1-TOMsimilarityFromExpr(datExpr, power = 10);
 # Transform dissTOM with a power to make moderately strong connections more visible in the heatmap
 plotTOM = dissTOM^7;
 # Set diagonal to NA for a nicer plot
 diag(plotTOM) = NA;
 # Call the plot function
 #sizeGrWindow(9,9)
 pdf(file = "TOM.pdf", width = 8, height = 8)
 TOMplot(plotTOM, geneTree, moduleColors, main = "Network heatmap plot, all genes")
 dev.off()
 
 #=====================================================================================
 #
 #  Code chunk 3
 #
 #=====================================================================================
 
 
 nSelect = 400
 # For reproducibility, we set the random seed
 set.seed(10);
 select = sample(nGenes, size = nSelect);
 selectTOM = dissTOM[select, select];
 # There's no simple way of restricting a clustering tree to a subset of genes, so we must re-cluster.
 selectTree = hclust(as.dist(selectTOM), method = "average")
 selectColors = moduleColors[select];
 # Open a graphical window
 sizeGrWindow(9,9)
 # Taking the dissimilarity to a power, say 10, makes the plot more informative by effectively changing 
 # the color palette; setting the diagonal to NA also improves the clarity of the plot
 plotDiss = selectTOM^7;
 diag(plotDiss) = NA;
 pdf(file = "selectTOM.pdf", width = 8, height = 8)
 TOMplot(plotDiss, selectTree, selectColors, main = "Network heatmap plot, selected genes")
 dev.off()
 
 #=====================================================================================
 #
 #  Code chunk 4
 #
 #=====================================================================================
 
 
 # Recalculate module eigengenes
 MEs = moduleEigengenes(datExpr, moduleColors)$eigengenes
 # Isolate weight from the clinical traits
 weight = as.data.frame(datTraits$glucose);
 names(weight) = "weight"
 # Add the weight to existing module eigengenes
 MET = orderMEs(cbind(MEs, weight))
 # Plot the relationships among the eigengenes and the trait
 pdf(file = "modules.pdf", width = 8, height = 8)
 #sizeGrWindow(5,7.5);
 par(cex = 0.9)
 plotEigengeneNetworks(MET, "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2), cex.lab = 0.8, xLabelsAngle= 90)
 dev.off()
 
 
 MEs = moduleEigengenes(datExpr, moduleColors)$eigengenes
 # Isolate weight from the clinical traits
 asparagine = as.data.frame(datTraits$asparagine);
 names(asparagine) = "asparagine"
 # Add the weight to existing module eigengenes
 MET = orderMEs(cbind(MEs, asparagine))
 # Plot the relationships among the eigengenes and the trait
 pdf(file = "modules_asparagine.pdf", width = 8, height = 8)
 #sizeGrWindow(5,7.5);
 par(cex = 0.9)
 plotEigengeneNetworks(MET, "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2), cex.lab = 0.8, xLabelsAngle= 90)
 dev.off()
 
 
 
 MEs = moduleEigengenes(datExpr, moduleColors)$eigengenes
 # Isolate weight from the clinical traits
 glucose = as.data.frame(datTraits$glucose);
 names(glucose) = "glucose"
 # Add the weight to existing module eigengenes
 MET = orderMEs(cbind(MEs, glucose))
 # Plot the relationships among the eigengenes and the trait
 pdf(file = "modules_glucose.pdf", width = 8, height = 8)
 #sizeGrWindow(5,7.5);
 par(cex = 0.9)
 plotEigengeneNetworks(MET, "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2), cex.lab = 0.8, xLabelsAngle= 90)
 dev.off()
 
 #=====================================================================================
 #
 #  Code chunk 5
 #
 #=====================================================================================
 
 
 # Plot the dendrogram
 sizeGrWindow(6,6);
 par(cex = 1.0)
 plotEigengeneNetworks(MET, "Eigengene dendrogram", marDendro = c(0,4,2,0),
                       plotHeatmaps = FALSE)
 # Plot the heatmap matrix (note: this plot will overwrite the dendrogram plot)
 par(cex = 1.0)
 pdf(file = "eigengene_heatmap.pdf", width = 8, height = 8)
 plotEigengeneNetworks(MET, "Eigengene adjacency heatmap", marHeatmap = c(3,4,2,2),
                       plotDendrograms = FALSE, xLabelsAngle = 90)
 dev.off()
 
 
 table(moduleColors)
 
   
# Summary output of network analysis results
names(datExpr)
names(datExpr)[moduleColors=="tan"]
names(datExpr)[moduleColors=="red"]

# to find size of module
length(names(datExpr)[moduleColors=="magenta"])

  
#########   export network and visualise in Cytoscape
https://cytoscape.org/

lnames = load(file = "sunflower_3200_met_networkConstruction-auto.RData")
lnames


turquoise <- datExpr[moduleColors=="turquoise"]
blue <- datExpr[moduleColors=="blue"]
brown <- datExpr[moduleColors=="brown"]
yellow <- datExpr[moduleColors=="yellow"]
green <- datExpr[moduleColors=="green"]
red <- datExpr[moduleColors=="red"]
black <- datExpr[moduleColors=="black"]
pink <- datExpr[moduleColors=="pink"]
magenta <- datExpr[moduleColors=="magenta"]
purple <- datExpr[moduleColors=="purple"]
greenyellow <- datExpr[moduleColors=="greenyellow"]
salmon <- datExpr[moduleColors=="salmon"]
tan <- datExpr[moduleColors=="tan"]

## output network for selected modules

selected_modules<-cbind(salmon,black,pink,purple)
TOM = TOMsimilarityFromExpr(selected_modules, power = 10);
probes = names(selected_modules)
modules = c("salmon_black_pink_purple");


# Export the network into edge and node list files Cytoscape can read
# change weight threshold to output networks of different size
cyt = exportNetworkToCytoscape(TOM,
edgeFile = paste("CytoscapeInput-edges-", paste(modules,collapse="-"), ".txt", sep=""),
weighted = TRUE,
threshold = 0.25,
nodeNames = probes);




#####Import network into cytoscape
 
 File>Import>Network>File
 select to and from node and add weight
 
 # change layout
 Layout>yfile>organic
 
 # import modulecolors file to colour nodes by module colour
 File > Import > Table > modulecolurs.txt
 
 Style
 Fill colour  - module colour
 Mapping type - Discrete mapping
 
## networks can be very large and difficult to visualise so select smaller networks
either select manually or start with highly connected node and add connected nodes
 
 ## input ID list of nodes - such as transcription factors
 Select > nodes > ID list  
 
 File > new > network > selected nodes all edges

 # To analyze network and colour by degree and weight
 Tool >  Analyze network
 
 
