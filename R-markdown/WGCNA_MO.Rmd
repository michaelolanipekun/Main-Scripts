---
title: "WGCNA"
author: "Michael Olanipekun"
date: "28/09/2021"
output:
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Required Packages

```{r, echo=TRUE}
.libPaths(new = c('C:/icnas1.cc.ic.ac.uk/mo316/R/win-library/3.5', 
                  'C:/Users/mo316/Documents/1. Meso PhD/5. Scripts/R/win-library/3.5',
                  'C:/icnas1.cc.ic.ac.uk/mo316/R/win-library/3.6', 
                  'C:/Program Files/R/R-4.0.2/library'))

# Package for WGCNA
library(WGCNA)

# Visualising Networks
library(igraph)
library(RColorBrewer)

# Correlations
library(corrplot)

# package for dummy data
library(FactoMineR)
```

# Import dataset
# dataframe should have variables as columns and samples as rows

```{r, echo=TRUE}
data(poulet)
dat <- poulet[,-1]
dat2<- dat
head(dat[,1:5])
ar <- poulet[,1] # metadata for analysis
ar
```

# Run WGCNA
## 1) Preamble, set up for data output later
```{r, echo=TRUE}
options(stringsAsFactors = FALSE)
dataType = "Genes"
subset = "F" # F (full), T, I, N
subsetIndex = NA
fileName = paste("_WGCNA_",dataType,sep = "","_",subset)
network = list()
network$fileName <- fileName
```

## 2) Set up parameters for building adjacency matrix
```{r, echo=TRUE}
sampleTree = hclust(dist(dat), method = "average")
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 0.1,
     cex.axis = 0.5, cex.main = 1)
height = 0.022 # change accordingly
abline(h = height, col = "red")
clust = cutreeStatic(sampleTree, cutHeight = height, minSize = 1)
table(clust)

keepSamples = (clust==1)
nSamples = nrow(dat)

sampleTree2 = hclust(dist(dat), method = "average")
# Convert traits to a color representation: white means low, red means high, grey means missing entry
traitColors = numbers2colors(as.numeric(ar), signed = T); # signed set to True
# Plot the sample dendrogram and the colors underneath.
plotDendroAndColors(sampleTree2, traitColors,
                    groupLabels = "Sample type",
                    main = "Sample dendrogram and trait heatmap")
# Choose soft threshold power beta
# Choose a set of soft-thresholding powers
powers = c(c(1:10), seq(from = 12, to=26, by=2))
sft = pickSoftThreshold(dat, powerVector = powers, 
                        verbose = 5,networkType = "signed",
                        corOptions = list(method="spearman",use="complete.obs"))

```

## 3) Plot to determine soft threshold
```{r, echo=TRUE}
cex1 = 0.9;
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"));
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red");
abline(h=0.90,col="red")
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
```

```{r, echo=TRUE}
# Pick soft threshold
sft$powerEstimate # = 16, meaning the soft power threshold should be set to 16

```

## 4) Plot trees using soft threshold
```{r, echo=TRUE}
softPower = 16
adj = adjacency(dat, power = softPower,type = "signed",corOptions = "method='spearman',use='complete.obs'")

# Turn adjacency to topology overlap
TOM = TOMsimilarity(adj,TOMType="signed")
dissTOM = 1-TOM

# Use TOM to build matrix
# Call the hierarchical clustering function
featTree = hclust(as.dist(dissTOM), method = "average");
# Plot the resulting clustering tree (dendrogram)
sizeGrWindow(12,9)
plot(featTree, xlab="", sub="", main = "Metabolite clustering on TOM-based dissimilarity",
     labels = FALSE, hang = 0.04);

# 2. Create modules from the clustering
minModuleSize = 10
# Module identification using dynamic tree cut:
dynamicMods = cutreeDynamic(dendro = featTree, distM = dissTOM,
                            deepSplit = 2, pamRespectsDendro = TRUE,
                            minClusterSize = minModuleSize);
table(dynamicMods)
# Convert numeric lables into colors
dynamicColors = labels2colors(dynamicMods)
table(dynamicColors)
# Plot the dendrogram and colors underneath

sizeGrWindow(8,6)
plotDendroAndColors(featTree, dynamicColors, "Dynamic Tree Cut",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05,
                    main = "Metabolite dendrogram and module colors")


dynamicColors=NULL
for (ds in 0:3){
  dynamicMods = cutreeHybrid(dendro = featTree, distM = dissTOM,
                             pamStage=TRUE, pamRespectsDendro = TRUE,
                             minClusterSize = minModuleSize, deepSplit = ds)
  dynamicColors=cbind(dynamicColors,labels2colors(dynamicMods$labels)); 
}
plotDendroAndColors(featTree, dynamicColors, paste("dpSplt =",0:3), main = "",dendroLabels=FALSE)

dynamicColors = dynamicColors[,3]
table(dynamicColors)
```

## 5) Merge highly correlated modules
```{r, echo=TRUE}
# Calculate eigengenes
MEList = moduleEigengenes(dat, colors = dynamicColors)
MEs = MEList$eigengenes
# Calculate dissimilarity of module eigengenes
MEDiss = 1-cor(MEs);
# Cluster module eigengenes
METree = hclust(as.dist(MEDiss), method = "average");
# Plot the result
plot(METree, main = "Clustering of module eigengenes",
     xlab = "", sub = "")
MEDissThres = 0.25
# Plot the cut line into the dendrogram
abline(h=MEDissThres, col = "red")
# Call an automatic merging function
merge = mergeCloseModules(dat, dynamicColors, cutHeight = MEDissThres, verbose = 3)
# The merged module colors
mergedColors = merge$colors;
# Eigengenes of the new merged modules:
mergedMEs = merge$newMEs;
```
# Visualise effect of merging
```{r, echo=TRUE}
plotDendroAndColors(featTree, cbind(dynamicColors, mergedColors),
                    c("Dynamic Tree Cut", "Merged dynamic"),
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)
```

## 6) Acquire module eigenvectors/eigengenes
```{r, echo=TRUE}
# Rename to moduleColors
moduleColors = mergedColors
network$moduleColors <- moduleColors
# get MEs
MEs0 = moduleEigengenes(dat, network$moduleColors,excludeGrey=T)$eigengenes
MEs = orderMEs(MEs0)
grey <- dat[,moduleColors=="grey"]
ME <- cbind(MEs,grey)

network$ME <- ME

# Name the list file according to the type
#save(network,file = paste(network$fileName,".Rdata"))

# Visualise dendrogram and module assignemnt
plotDendroAndColors(network$featTree, network$moduleColors, "Dynamic Tree Cut",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05,
                    main = "Metabolite dendrogram and module colors")
```


## 7) Collect data into single list
```{r, echo=TRUE}
moduleColors = mergedColors
network$moduleColors <- moduleColors
network$featTree <- featTree
network$dat <- dat
network$adj <- adj
network$TOM <- TOM
network$softPower <- softPower
network$minModuleSize <- minModuleSize
network$MEs <- MEs
network$ar <- ar
```


## 8) Run statistical tests
```{r, echo=TRUE}
# Wilcox test + Kriskal Wallis test
class <- as.numeric(network$ar)
modNames = substring(names(network$MEs), 3)
moduleP<-sapply(1:ncol(network$MEs),function(i){
  wilcox.test(as.numeric(network$MEs[class,i]),class,na.action=na.omit,exact=FALSE)$p.value})
modulePadj<-p.adjust(moduleP,method="BH")
moduleQ <- qvalue::qvalue(moduleP, pi0 = 1)$qvalues
modulekwP<-sapply(1:ncol(network$MEs),function(i){
  kruskal.test(as.numeric(network$MEs[,i])~as.factor(network$ar),na.action=na.omit)$p.value
})
modulekwPadj<-p.adjust(modulekwP,method="BH")

moduleStat <- data.frame(moduleP = moduleP,modulePadj = modulePadj, moduleQ = moduleQ,modulekwP = modulekwP,modulekwPadj = modulekwPadj)
rownames(moduleStat)<- modNames

network$moduleStat <- moduleStat

# Correlations (Spearman)
ME_net1 <- network$MEs
MD_net1 <- as.numeric(network$ar)
cor <- cor(ME_net1,MD_net1,method = "spearman",use="complete.obs")
cor.p <- apply(data.matrix(MD_net1),2,function(y){
  apply(data.matrix(ME_net1),2,function(x){
  cor.test(x,y,method="spearman",exact=NULL)$p.value})})
cor.q <- qvalue::qvalue(cor.p, pi0 = 1)$qvalues #BH correction

# plot correlations
corrplot(cor, method = 'color', addgrid.col = 'black', tl.col='black',
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = cor.q, insig = "label_sig",
         pch.col = "black", pch.cex = 1.1, col = NULL)

```

## 9) Visualse TOM plot
```{r, echo=TRUE}
# Plot TOM heatmap
diag(dissTOM) = NA
TOMplot(dissTOM^7, featTree, mergedColors, col = rev(colorRampPalette(brewer.pal(8, 'YlOrRd'))(25)), 
        main = 'TOM Network Heatmap of All Metabolites')

```


## 10) Visualise network using igraph
```{r, echo=TRUE}
# Build network
modNames = substring(names(network$MEs2), 3) 
moduleMembership = as.data.frame(cor(network$dat, network$MEs, use = "p", method = "spearman"));
MMPvalue = as.data.frame(corPvalueStudent(as.matrix(moduleMembership), nSamples))

names(moduleMembership) = paste("MM", modNames, sep="");
names(MMPvalue) = paste("p.MM", modNames, sep="");

# write these info into a file
moduleSignificanceQ = as.vector(moduleP[match(network$moduleColors,modNames)])

# Create the starting data frame
networkOut = data.frame(feature = colnames(network$dat),
                        moduleColor = network$moduleColors,
                        moduleSignificanceQ = moduleSignificanceQ)


# Order modules by their significance for class
modOrder = order(moduleStat$moduleP);
# Add module membership information in the chosen order
for (mod in 1:ncol(moduleMembership))
{
  oldNames = names(networkOut)
  networkOut = data.frame(networkOut, moduleMembership[, modOrder[mod]],
                          MMPvalue[, modOrder[mod]])
  names(networkOut) = c(oldNames, paste("MM.", modNames[modOrder[mod]], sep=""),
                        paste("p.MM.", modNames[modOrder[mod]], sep=""))
}

# NETWORK CONCEPTS
kIN <-intramodularConnectivity(network$adj,network$moduleColors)
networkOut = data.frame(networkOut,kIN)
network$networkOut <- networkOut

# NetworkOut
ndat = ncol(network$dat)
modNames = substring(names(network$MEs), 3)
MMneat <-matrix(NA,ndat)
for (ii in 1:ndat){
  if (network$moduleColors[ii]=="grey"){
    MMneat[ii] <- 1
  } else {
    MMneat[ii] <- network$networkOut[ii,match(paste("MM.",network$moduleColors[ii],sep=""), 
                                              paste("MM.",network$networkOut$moduleColor[ii],sep=""))]
  }
}
grey <- network$dat[,network$moduleColors=="grey"]
network$MEsG <- cbind(network$MEs,grey)
sig <-matrix(NA,ncol(network$dat))
nME <- ncol(network$MEs)
gStat <-  network$moduleStat[(nME+1):nrow(network$moduleStat),2]
g = 1
for (ii in 1:ncol(network$dat)){
  if (network$moduleColors[ii]=="grey"){
    sig[ii] <- gStat[g]
    g = g+1
  } else {
    sig[ii] <- network$moduleStat[match(network$moduleColors[ii],modNames),2]
  }
}
network$networkOut$moduleSig <- sig
network$networkOut = data.frame(network$networkOut,MMneat)


# Necessary functions
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/150 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,fg = NA,
          stars=cbind(vertex.size, 2*vertex.size, vertex.size,0),
          add=TRUE, inches=FALSE)
}
add_shape("triangle", #clip=shapes("circle")$clip,
          plot=mytriangle)

weight.community=function(graph,weight.within,weight.between){
  el <- get.edgelist(graph)
  ew <- apply(el,1,function(i){
    if(V(graph)$type[match(i[1],V(graph)$name)] == V(graph)$type[match(i[2],V(graph)$name)]) {
      return (weight.within)
    } else { 
      return (weight.between)
    }
  })
  E(graph)$ew <- ew
  return(graph)
}

# Visualise full network
network<-network
adj <- network$adj
adj[adj<(((0.2+1)/2)^network$softPower)]<-0
diag(adj)<-0
graph <- graph.adjacency(adj,mode = "upper",weighted = T)
V(graph)$color <- network$moduleColors
E(graph)$color <- "grey90"
V(graph)$size <- (ifelse(V(graph)$color=="grey",0.2,(network$networkOut$kTotal/max(network$networkOut$kTotal))^0.2)*2)
set.seed(111)
lo <- layout.fruchterman.reingold(graph,weight = (E(graph)$weight))
graph=delete.vertices(graph,which(degree(graph)<1))
graph <- delete_edges(graph,which(E(graph)$weight<(((0.4+1)/2)^network$softPower)))
plot.igraph(graph,layout = lo,
            # VERTEX
            vertex.color=V(graph)$color,
            vertex.frame.color=NA,
            vertex.size=V(graph)$size,
            # EDGE
            edge.width = (E(graph)$weight)^2,
            edge.color = E(graph)$color,
            # LABEL
            vertex.label = NA,
            vertex.label.family="sans", vertex.label.cex=0.3,
            vertex.label.dist=0.7, vertex.label.degree=pi/2,
            vertex.label.color="black")
```

## Module conservation 
```{r, eval=FALSE}
# Example datasets called Poulet and Chicken
colorsPoulet <- network$moduleColors

setLabels = c("Poulet", "Chicken");
multiExpr = list(Poulet = list(data = dat), Chicken = list(data = dat2)); # Bring in new data as dataframe 'Chicken'
multiColor = list(Poulet = colorsPoulet);

system.time( {
  mp = modulePreservation(multiExpr, multiColor,
  referenceNetworks = 1,
  nPermutations = 200,
  randomSeed = 1,
  quickCor = 0,
  verbose = 3)
} );

#save(mp, file = "modulePreservation.RData");

# Acquire stats for conservation
ref = 1
test = 2
statsObs = cbind(mp$quality$observed[[ref]][[test]][, -1], mp$preservation$observed[[ref]][[test]][, -1])
statsZ = cbind(mp$quality$Z[[ref]][[test]][, -1], mp$preservation$Z[[ref]][[test]][, -1]);

print(cbind(statsObs[, c("medianRank.pres", "medianRank.qual")],
signif(statsZ[, c("Zsummary.pres", "Zsummary.qual")], 2)) )

# Visualise
# Module labels and module sizes are also contained in the results
modColors = rownames(mp$preservation$observed[[ref]][[test]])
moduleSizes = mp$preservation$Z[[ref]][[test]][, 1];
# leave grey and gold modules out
plotMods = !(modColors %in% c("grey", "gold"));
# Text labels for points
text = modColors[plotMods];
# Auxiliary convenience variable
plotData = cbind(mp$preservation$observed[[ref]][[test]][, 2], mp$preservation$Z[[ref]][[test]][, 2])
# Main titles for the plot
mains = c("Preservation Median rank", "Preservation Zsummary");
# Start the plot
sizeGrWindow(10, 5);
#pdf(fi="Plots/BxHLiverFemaleOnly-modulePreservation-Zsummary-medianRank.pdf", wi=10, h=5)
par(mfrow = c(1,2))
par(mar = c(4.5,4.5,2.5,1))
for (p in 1:2)
{
min = min(plotData[, p], na.rm = TRUE);
max = max(plotData[, p], na.rm = TRUE);
# Adjust ploting ranges appropriately
if (p==2)
{
if (min > -max/10) min = -max/10
ylim = c(min - 0.1 * (max-min), max + 0.1 * (max-min))
} else
ylim = c(max + 0.1 * (max-min), min - 0.1 * (max-min))
plot(moduleSizes[plotMods], plotData[plotMods, p], col = 1, bg = modColors[plotMods], pch = 21,
main = mains[p],
cex = 2.4,
ylab = mains[p], xlab = "Module size", log = "x",
ylim = ylim,
xlim = c(10, 2000), cex.lab = 1.2, cex.axis = 1.2, cex.main =1.4)
labelPoints(moduleSizes[plotMods], plotData[plotMods, p], text, cex = 1, offs = 0.08);
# For Zsummary, add threshold lines
if (p==2)
{
abline(h=0)
abline(h=2, col = "blue", lty = 2)
abline(h=10, col = "darkgreen", lty = 2)
}
}
# If plotting into a file, close it
#dev.off();
```

```{r, eval = FALSE}
# Re-initialize module color labels and sizes
modColors = rownames(statsZ)
moduleSizes = mp$quality$Z[[ref]][[test]][, 1];
# Exclude improper modules
plotMods = !(modColors %in% c("grey", "gold"));
# Create numeric labels for each module
labs = match(modColors[plotMods], standardColors(50));
# Start the plot: open a suitably sized graphical window and set sectioning and margins.
sizeGrWindow(12, 9);
par(mfrow = c(3,5))
par(mar = c(3,3,2,1))
par(mgp = c(1.6, 0.4, 0));
# Plot each Z statistic in a separate plot.
for (s in 1:ncol(statsZ))
{
min = min(statsZ[plotMods, s], na.rm = TRUE);
max = max(statsZ[plotMods, s], na.rm = TRUE);
if (min > -max/5) min = -max/5
plot(moduleSizes[plotMods], statsZ[plotMods, s], col = 1, bg = modColors[plotMods], pch = 21,
main = colnames(statsZ)[s],
cex = 1.7,
ylab = colnames(statsZ)[s], xlab = "Module size", log = "x",
ylim = c(min - 0.1 * (max-min), max + 0.1 * (max-min)),
xlim = c(20, 1000))
labelPoints(moduleSizes[plotMods], statsZ[plotMods, s], labs, cex = 0.7, offs = 0.04);
abline(h=0)
abline(h=2, col = "blue", lty = 2)
abline(h=10, col = "darkgreen", lty = 2)
}
data.frame(color = modColors[plotMods], label = labs)

```
## Get new modules and plot them
```{r, eval = FALSE}


```
