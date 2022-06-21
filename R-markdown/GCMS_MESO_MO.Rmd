---
title: "GCMS_Mesothelioma_analysis"
author: "Michael Olanipekun"
date: "16/06/2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Setup

```{r, echo=TRUE}
setwd("c:/Users/mo316/Documents/1. Meso PhD/1. Year 1/1. Experiments/3. Metabolite extraction_cells/181121_Cell metabolite extraction GCMS_180921_181114/Data")

load("210405_GCMS analysis environment_Aug2021.RData")
.libPaths(new = c('C:/icnas1.cc.ic.ac.uk/mo316/R/win-library/3.5', 
                  'C:/Users/mo316/Documents/1. Meso PhD/5. Scripts/R/win-library/3.5',
                  'C:/icnas1.cc.ic.ac.uk/mo316/R/win-library/3.6', 
                  'C:/Program Files/R/R-4.0.2/library'))

# Library ####
library(ropls)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggforce)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(scales)
library(gridExtra)
library(ggplotgui)
library(ggpmisc)
library(ggrepel)
library(plyr)
library(dplyr)
library(tidyverse)
library(goeveg)
library(data.table)
library(reshape2)
library(varhandle)
library(corrplot)
library(gtools)
library(reshape2)

#glm
#library(car)
library(heplots)
#library(tidyverse)
library(lm.beta)
library(openxlsx)
library(groupedstats)

library(effectsize)

library(MWASTools)
library(ComplexHeatmap)
library(circlize)
library(MetaboSignal)
#tsne
library(M3C)

library(MetaboAnalystR)
library(curl)

library(WGCNA)
library(igraph)
library(corrplot)

library(shapper)

library(variancePartition)
library(mixOmics)
library(RColorBrewer)

library(RColorBrewer)
library(scales)
colorme <- c('#FDAE61', '#80B1D3', '#FB8072', '#BEBADA', '#FDE0EF', '#B3E2CD', '#FDCDAC', '#80CDC1', '#FFF2AE','#AA9486')
colorme2 <- c('#82B886', '#E9BF7C', '#E6637A', '#8C6EB8', 
              '#628BEB', '#838536', '#EBA29C', '#99A0B6', 
              '#B8887B','#B5A991' ,'#61B8A5', '#366F85', '#F09870')
blackpancol <- c('#B7A4EE', '#664EAE','#1A0554', '#969696', '#252525', 'black', '#FFCA36', '#9ebcda', 'sienna4')
#show_col
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(axis.line=element_line(colour = 'black'))
  
  # Format the legend, but hide by default
  
  theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
Theme.PCA <- function() {
  theme(axis.line=element_line(colour = 'black')) + 
    theme(panel.background=element_rect(fill='white', color='black')) +
    theme(plot.background=element_rect(fill='white', color='white')) +
    theme(panel.grid.major=element_line(color=color.grid.major)) +
    theme(panel.grid.minor=element_line(color=color.grid.major)) +
    theme(axis.title.x=element_text(size=12,color='black')) +
    theme(axis.title.y=element_text(size=12,color='black')) +
    theme(legend.title = element_text(size = 12)) +
    theme(legend.text = element_text(size = 10))
  
}
Theme.PCA.Blank <- function() {
  theme(axis.line=element_line(colour = 'black')) + 
    theme(panel.background=element_rect(fill='white', color='black')) +
    theme(plot.background=element_rect(fill='white', color='white')) +
    theme(panel.grid.major=element_line(color='white')) +
    theme(panel.grid.minor=element_line(color='white')) +
    theme(axis.title.x=element_text(size=12,color='black')) +
    theme(axis.title.y=element_text(size=12,color='black')) +
    theme(legend.title = element_text(size = 12)) +
    theme(legend.text = element_text(size = 10))
  
}
Theme.Box.Blank <- function() {
  theme(axis.line=element_line(colour = 'black')) + 
    theme(panel.background=element_rect(fill='white', color='White')) +
    theme(plot.background=element_rect(fill='white', color='white')) +
    theme(panel.grid.major=element_line(color='white')) +
    theme(panel.grid.minor=element_line(color='white')) +
    theme(axis.title.x=element_text(size=12,color='black')) +
    theme(axis.title.y=element_text(size=12,color='black')) +
    theme(axis.text.x = element_text(size = 10, color = 'black')) +
    theme(legend.title = element_text(size = 12)) +
    theme(legend.text = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1, 
                                     vjust = 0.4))
  
}
plot.pls <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@scoreMN[,2] #PC2
  ab <- data.frame(a,b,G2)
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = "P1(18%)", y = "P2(18%)", fill = 'Timepoints') +
    geom_point(data = ab, aes(a, b, fill = reorder(G2)), size = 7, color = 'black', shape = 21) +
    scale_fill_manual(values = c(brewer.pal(7, 'Greens'))) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank()
}
plot.opls <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@orthoScoreMN[,1] #Ort1
  ab <- data.frame(a,b,as.character(MD.tran$time))
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = paste0('t1 ', '(',x@modelDF$R2X[1]*100, '%', ')'), y = paste0('to1 ', '(',x@modelDF$R2X[1]*100, '%', ')'), fill = 'Timepoints') +
    geom_point(data = ab, aes(a, b, fill = reorder(as.character(MD.tran$time), MD.tran$time)), 
               size = 7, color = 'black', shape = 21) +
    scale_fill_manual(values = c(brewer.pal(7, 'Spectral'))) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank()
}
plot.opls2 <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@orthoScoreMN[,1] #Ort1
  ab <- data.frame(a,b,G2)
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = paste0('t1 ', '(',x@modelDF$R2X[1]*100, '%', ')'), y = paste0('to1 ', '(',x@modelDF$R2X[1]*100, '%', ')'), 
           fill = 'Timepoint', color = 'Treatment') +
    geom_point(data = ab, aes(a, b, fill = G2, color = Tr1), 
               size = 7, shape = 21, stroke = 1.5) +
    scale_fill_manual(values = c(colMAMBA[5], brewer.pal(8, 'Blues')[3], brewer.pal(8, 'Blues')[6])) +
    scale_color_manual(values = c(colMAMBA[1], colMAMBA[7])) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank()
}
plot.opls.weights <- function(x){
  m <- x@weightStarMN[,1]
  n <- x@orthoWeightMN[,1]
  mn <- data.frame(rownames(x@weightStarMN),m,n)
  q <- ggplot(data = mn, mapping = aes (m, n, label = mn[,1]))
  q + labs(x = "w*c1", y = "w*c2") +
    geom_text() +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank()
}
plot.opls.weights2 <- function(x){
  mo <- data.frame(rownames(x@weightStarMN),x@weightStarMN, x@orthoWeightMN)
  ggplot(data = mo, mapping = aes(mo[,2], mo[,3], label = mo[,1])) + 
    labs(x = "w*c1", y = "w*c2") +
    geom_point(color = colorme[8], size = 2) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank() +
    stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.9) #filters for where the data is sparsest
}

col2rowname <- function(x){
  rownames(x) <- x[,1]
  x <- x[,-1]
}

```

# Pilot experiment
## OPLS
```{r, echo=TRUE}
aim1 <-	as.matrix(read.csv("190129_180906_GCMS_R FORMAT_ALL_PQN_MO.csv",	header	=	TRUE,	check.names	=	FALSE))
aim1 <- col2rowname(aim1)
a1_count <- aim1[,1]
aim1.1 <- apply(unlist(aim1[,-1]), 2, as.numeric)
rownames(aim1.1) <- rownames(aim1)
aim1.2 <- aim1.1[1:12,]

a1_count <- a1_count[1:12]
aim1.MD <-	as.data.frame(read.csv("190129_180906_GCMS_R FORMAT_ALL_PQN_metadata_MO.csv",	header	=	TRUE,	check.names	=	FALSE))

a1_opls <- opls(aim1.2, as.numeric(a1_count), parAsColFcVn = aim1.MD$CellLine, pre=1, ort=1, log10L = TRUE, permI = 10000)
plot(a1_opls, typeVc = 'x-score')
plot(a1_opls, typeVc = 'permutation') 
plot(a1_opls, typeVc = 'x-loading')
a <- a1_opls@scoreMN[,1] #PC1
b <- a1_opls@orthoScoreMN[,1] #Ort1
ab <- data.frame(a,b,as.numeric(a1_count))

p <- ggplot(data = ab, mapping = aes (a, b))
p + labs(x = paste0('t1 ', '(',a1_opls@modelDF$R2X[1]*100, '%', ')', 
                    "\n", 'R2Y: ', a1_opls@summaryDF$`R2Y(cum)`, '     ', 
                    'Q2Y: ', a1_opls@summaryDF$`Q2(cum)`, '     ', 
                    'pR2Y = ', a1_opls@summaryDF$pR2Y, '     ', 
                    'pQ2 = ', a1_opls@summaryDF$pQ2), 
         y = paste0('to1 ', '(',a1_opls@modelDF$R2X[2]*100, '%', ')'), color = 'Cell Line', fill = 'Cell Count (x10^5 cells/ml)') +
  geom_point(aes(color = aim1.MD$CellLine, fill = ab[,3]), size = 9, shape = 21, stroke = 1.3) +
  scale_fill_gradient(low = 'white', high = blackpancol[2], aesthetics = 'fill') +
  scale_color_manual(values = c('firebrick1', 'royalblue4')) +
  stat_ellipse(type = "norm", color = 'grey') +
  geom_text(label = aim1.MD$CellSeed) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  Theme.PCA.Blank()

#plot weights
m <- a1_opls@weightMN
n <- a1_opls@orthoWeightMN
mn <- data.frame(colnames(aim1.2),m,n)
q <- ggplot(data = mn, mapping = aes (m, n, label = mn[,1]))
q + labs(x = "t1", y = "to1") +
  geom_text() +
  stat_ellipse(type = "t", color = 'white') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  Theme.PCA.Blank()
```

## Growth curves

```{r, echo=TRUE}
setwd("c:/users/mo316/Documents/1. Meso PhD/5. Scripts/R/work-directory")
curveV2	<-	as.data.frame(read.csv("growth_curve_V2.csv",	header	=	TRUE,	check.names	=	FALSE))
setwd("c:/Users/mo316/Documents/1. Meso PhD/1. Year 1/1. Experiments/3. Metabolite extraction_cells/181121_Cell metabolite extraction GCMS_180921_181114/Data")
curveV2 <- curveV2[-c(11:14), ]
ms=curveV2[,2]
h2=curveV2[,3]
x3 = curveV2[,1]
y3 = data.frame(ms,h2)
x3y3 = data.frame(x3,y3)
library(dplyr)

#Plot:
grw1 <- ggplot(data = x3y3, aes(x3,ms)) 
grw1 + geom_point(data = x3y3, aes(x3, h2), color = 'firebrick1',  size = 3) +
  geom_point(data = x3y3, aes(x3, ms), color = 'royalblue4', size = 3) +  
  geom_smooth(data = x3y3, aes(x3, h2, color = 'firebrick1'), size = 2, method = 'lm', se = FALSE) + 
  geom_smooth(data = x3y3, aes(x3, ms, color = 'royalblue4'), size = 2, method = 'lm', se = FALSE) +
  labs(x='Time (hours)', y='Cell count (cells/ml)') + 
  scale_x_continuous(breaks = c(x3)) +
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank() + theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 14),
                            axis.text.y = element_text(color = 'black', size = 14),
                            axis.title.x = element_text(color = 'black', size = 20),
                            axis.title.y = element_text(color = 'black', size = 20),
                            legend.text = element_text(size = 14),
                            legend.title =  element_text(size = 16))
```

# Optimisation experiments (Method 1-3)
## Setup (Methods 1-4)
```{r, echo=TRUE}
#Datasets
aim2_72 <- read.xlsx('181217_intTable_PQN_CV filtered_V2.xlsx', 3)
aim2_24 <- read.xlsx('181217_intTable_PQN_CV filtered_V2.xlsx', 4)

#72h data prep
QC.index <- which(aim2_72$X2 == 'QC')
aim2_72.noQC <- aim2_72[-c(QC.index),]
M1_72 <- aim2_72.noQC[which(aim2_72.noQC$Exp == '1'),]
M2_72 <- aim2_72.noQC[which(aim2_72.noQC$Exp == '2'),]
M3_72 <- aim2_72.noQC[which(aim2_72.noQC$Exp == '3'),]
#24h data prep
QC.index.24 <- which(aim2_24$X2 == 'QC')
aim2_24.noQC <- aim2_24[-c(QC.index.24),]
aim2_24.noQC.CellLine.bin <- to.dummy(aim2_24.noQC$X2, '.dum')[,1] # MSTO = 0, H2052 = 1
M4_24 <- aim2_24.noQC[,-c(1:4)]
rownames(M4_24) <- aim2_24.noQC$X3

#Metadata
aim2_72_MD <- aim2_72.noQC[,1:5]; aim2_24_MD <- aim2_24.noQC[,1:4]
seed1 <- aim2_72_MD$Seed[1:18]
seed2 <- aim2_72_MD$Seed[19:36]
seed3 <- aim2_72_MD$Seed[37:54]
aim2_72_MD.1 <- data.frame('sample' = as.numeric(gsub('S', '', aim2_72_MD$X1)), aim2_72_MD)
aim2_72_MD.ord <- aim2_72_MD.1[order(aim2_72_MD.1$sample),]
```

## OPLS and TSNE
```{r, echo=TRUE}
#PCAs and PLS
M123_72.dat <- aim2_72.noQC[,-c(1:4)]
M1_72.oplsda <- opls(M1_72[,-c(1:4)], M1_72$X2, parAsColFcVn = M1_72$X2, pre=1, ort=1, log10L = TRUE, permI = 10000, crossvalI = 17)
plot.opls1 <- function(x){ #change so this has the correct annotations as with aim 1
  a <- x@scoreMN[,1] #PC1
  b <- x@orthoScoreMN[,1] #Ort1
  ab <- data.frame(a,b,as.numeric(seed1))
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = paste0('t1 ', '(',x@modelDF$R2X[1]*100, '%', ')',
                      "\n", 'R2Y: ', x@summaryDF$`R2Y(cum)`, '     ', 
                      'Q2Y: ', x@summaryDF$`Q2(cum)`, '     ', 
                      'pR2Y = ', x@summaryDF$pR2Y, '     ', 
                      'pQ2 = ', x@summaryDF$pQ2), 
           y = paste0('to1 ', '(',x@modelDF$R2X[2]*100, '%', ')'), 
           color = 'Cell Line', fill = 'Seeding density\n(x10^5 cells/ml)') +
    geom_point(data = ab, aes(color = M1_72$X2, fill = ab[,3]), 
               size = 7, shape = 21, stroke = 1.3) +
    scale_fill_gradient(low = 'white', high = blackpancol[2], aesthetics = 'fill') +
    scale_color_manual(values = c('firebrick1', 'royalblue4')) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_text(label = seed1) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank()
}
plot.opls1(M1_72.oplsda) + ggtitle('Method 1')

M2_72.oplsda <- opls(M2_72[,-c(1:4)], M2_72$X2, parAsColFcVn = M2_72$X2, pre=1, ort=1, log10L = TRUE, permI = 10000, crossvalI = 17)
plot.opls2 <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@orthoScoreMN[,1] #Ort1
  ab <- data.frame(a,b,as.numeric(count2))
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = paste0('t1 ', '(',x@modelDF$R2X[1]*100, '%', ')',
                      "\n", 'R2Y: ', x@summaryDF$`R2Y(cum)`, '     ', 
                      'Q2Y: ', x@summaryDF$`Q2(cum)`, '     ', 
                      'pR2Y = ', x@summaryDF$pR2Y, '     ', 
                      'pQ2 = ', x@summaryDF$pQ2), 
           y = paste0('to1 ', '(',x@modelDF$R2X[2]*100, '%', ')'),
           color = 'Cell Line', fill = 'Final cell density\n(x10^5 cells/ml)') +
    geom_point(data = ab, aes(color = M2_72$X2, fill = ab[,3]), 
               size = 7, shape = 21, stroke = 1.3) +
    scale_fill_gradient(low = 'white', high = blackpancol[2], aesthetics = 'fill') +
    scale_color_manual(values = c('firebrick1', 'royalblue4')) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_text(label = seed2) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    guides(col = 'none') +
    Theme.PCA.Blank()
}
plot.opls2(M2_72.oplsda) + ggtitle('Method 2')
plot.opls.weights2(M2_72.oplsda)

M3_72.oplsda <- opls(M3_72[,-c(1:4)], M3_72$X2, parAsColFcVn = M3_72$X2, pre=1, ort=1, log10L = TRUE, permI = 10000, crossvalI = 17)
plot.opls3 <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@orthoScoreMN[,1] #Ort1
  ab <- data.frame(a,b,as.numeric(count3))
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = paste0('t1 ', '(',x@modelDF$R2X[1]*100, '%', ')',
                      "\n", 'R2Y: ', x@summaryDF$`R2Y(cum)`, '     ', 
                      'Q2Y: ', x@summaryDF$`Q2(cum)`, '     ', 
                      'pR2Y = ', x@summaryDF$pR2Y, '     ', 
                      'pQ2 = ', x@summaryDF$pQ2), 
           y = paste0('to1 ', '(',x@modelDF$R2X[2]*100, '%', ')'),
           color = 'Cell Line', fill = 'Final cell density\n(x10^5 cells/ml)') +
    geom_point(data = ab, aes(color = M3_72$X2, fill = ab[,3]), 
               size = 7, shape = 21, stroke = 1.3) +
    scale_fill_gradient(low = 'white', high = blackpancol[2], aesthetics = 'fill') +
    scale_color_manual(values = c('firebrick1', 'royalblue4')) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_text(label = seed3) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    guides(col = 'none') +
    Theme.PCA.Blank()
}
plot.opls3(M3_72.oplsda) + ggtitle('Method 3')

ggarrange(plot.opls.weights2(M1_72.oplsda) + ggtitle('Method 1'),
          plot.opls.weights2(M2_72.oplsda) + ggtitle('Method 2'),
          plot.opls.weights2(M3_72.oplsda) + ggtitle('Method 3'),  
          common.legend = F, legend = 'right')

# Try TSNE
tsne(t(M123_72.dat), labels = as.character(aim2_72.noQC$Exp), dotsize = 4, axistextsize = 12, perplex = 15, 
     legendtextsize = 14, legendtitle = 'Extraction Method') + 
  scale_colour_manual(values = c(blackpancol[4], blackpancol[2], blackpancol[3]))
```

## Barplot
```{r, echo=TRUE}
# raw intenisty totals
#RAW.DATA1.QC, RAW.DATA2.QC, RAW.DATA3.QC
Raw1.melt <- melt(RAW.DATA1.QC)
Raw1.sum <- ddply(data.frame('Exp' = rep('Exp1', length(Raw1.melt[,2])),Raw1.melt), 
                  c('Exp'), summarise,
                   N = length(value),
                   mean = mean(value),
                   sd = sd(value),
                   se = sd/sqrt(N))
Raw2.melt <- melt(RAW.DATA2.QC)
Raw2.sum <- ddply(data.frame('Exp' = rep('Exp2', length(Raw2.melt[,2])),Raw2.melt), 
                  c('Exp'), summarise,
                  N = length(value),
                  mean = mean(value),
                  sd = sd(value),
                  se = sd/sqrt(N))
Raw3.melt <- melt(RAW.DATA3.QC)
Raw3.sum <- ddply(data.frame('Exp' = rep('Exp3', length(Raw3.melt[,2])),Raw3.melt), 
                  c('Exp'), summarise,
                  N = length(value),
                  mean = mean(value),
                  sd = sd(value),
                  se = sd/sqrt(N))
met.data2 <- rbind(Raw1.sum, Raw2.sum, Raw3.sum)
  
Raw4.melt <- melt(RAW.DATA4.QC)
Raw4.sum <- ddply(data.frame('Exp' = rep('Exp4', length(Raw4.melt[,2])),Raw4.melt), 
                  c('Exp'), summarise,
                  N = length(value),
                  mean = mean(value),
                  sd = sd(value),
                  se = sd/sqrt(N))
met.data3 <- rbind(Raw1.sum, Raw2.sum, Raw3.sum, Raw4.sum)

met.data1 <- as.data.frame(met.data1)

# Barplot with error bars:
met.data2[,1] <- met.data1[,1]
met.bar2 <- ggplot(data = met.data2, mapping = aes(`Exp`, as.numeric(`mean`))) 
met.bar2 + geom_col(aes(fill = `Exp`), colour = 'black') + 
  geom_errorbar(aes(ymin = `mean`-`se`, ymax = `mean`+`se`), width = 0.4, col = 'black', size = 1) +
  scale_fill_manual(values = c(blackpancol[4], blackpancol[2], blackpancol[3])) + 
  labs(x = " ", y = "Mean raw metabolite intensity (AU)", fill = '') +
  Theme.Box.Blank() + theme(panel.grid.major.y = element_line(colour = blackpancol[1]), 
                            axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
                            legend.position = 'none')

# Methods 1-4 plotted
# met.data3[,1] <- met.data1[,1]
# met.bar3 <- ggplot(data = met.data3, mapping = aes(`Exp`, as.numeric(`mean`))) 
# met.bar3 + geom_col(aes(fill = `Exp`), colour = 'black') + 
#   geom_errorbar(aes(ymin = `mean`-`se`, ymax = `mean`+`se`), width = 0.4, col = 'black', size = 1) +
#   scale_fill_manual(values = c(blackpancol[4], blackpancol[2], blackpancol[3], blackpancol[9])) + 
#   labs(x = " ", y = "Mean raw metabolite intensity (AU)", fill = '') +
#   Theme.Box.Blank() + theme(panel.grid.major.y = element_line(colour = blackpancol[1]), 
#                             axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
#                             legend.position = 'none')

# Sig test:
Raw.QC.all <- rbind(data.frame('Exp' = rep('Exp1', nrow(RAW.DATA1.QC)),RAW.DATA1.QC),
                    data.frame('Exp' = rep('Exp2', nrow(RAW.DATA2.QC)),RAW.DATA2.QC),
                    data.frame('Exp' = rep('Exp3', nrow(RAW.DATA3.QC)),RAW.DATA3.QC),
                    data.frame('Exp' = rep('Exp4', nrow(RAW.DATA4.QC)),RAW.DATA4.QC))
Raw.QC.all.melt <- melt(Raw.QC.all)
pairwise.t.test(Raw.QC.all.melt$value,Raw.QC.all.melt$Exp, 
                p.adjust.method = 'BH', paired = FALSE)
```

## Quantification methods
```{r, echo=TRUE}
## seed count vs final count - Methods 2+3
f.count.exp23 <- count[19:54,]
s.count.exp23 <- c(seed2, seed3)
M23_count.df <- data.frame('Cell.Line' = c(M3_72$X2, M2_72$X2), 'final' = f.count.exp23, 'seed' = s.count.exp23, 'Exp' = aim2_72_MD.ord$Exp[19:54])
M23_count.df.M <- M23_count.df[M23_count.df$Cell.Line == 'MSTO',]
M23_count.df.H <- M23_count.df[M23_count.df$Cell.Line != 'MSTO',]
M23_count.df.M[,2:3] <- apply(M23_count.df.M[,2:3], 2, as.numeric)
M23_count.df.H[,2:3] <- apply(M23_count.df.H[,2:3], 2, as.numeric)

SvF <- ggplot(data = M23_count.df.M, aes(`seed`,`final`)) + 
  geom_point(data = M23_count.df.H, aes(`seed`,`final`), color = 'firebrick1',  size = 2) +
  geom_point(data = M23_count.df.M, aes(`seed`,`final`), color = 'royalblue4', size = 2) +  
  geom_smooth(data = M23_count.df.H, aes(`seed`,`final`, color = 'firebrick1'), size = 1, method = 'lm', se = TRUE) + 
  geom_smooth(data = M23_count.df.M, aes(`seed`,`final`, color = 'royalblue4'), size = 1, method = 'lm', se = TRUE) +
  labs(x='Seeding density (x10^5 cells/ml)', 
       y='Final cell count (x10^5 cells/ml)') + 
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank() + theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 12),
                            axis.text.y = element_text(color = 'black', size = 12),
                            axis.title.x = element_text(color = 'black', size = 16),
                            axis.title.y = element_text(color = 'black', size = 16),
                            legend.text = element_text(size = 12),
                            legend.title =  element_text(size = 14))
# cor tests:
cor.test(M23_count.df.M$seed, M23_count.df.M$final)$estimate
cor.test(M23_count.df.M$seed, M23_count.df.M$final)$p.value
cor.test(M23_count.df.H$seed, M23_count.df.H$final)$estimate
cor.test(M23_count.df.H$seed, M23_count.df.H$final)$p.value

# seed count vs final count - Methods 2+3 separated:
SvF.sep <- ggplot(data = M23_count.df.M, aes(`seed`,`final`, group = `Exp`)) + 
  geom_point(data = M23_count.df.H, aes(`seed`,`final`), color = 'firebrick1',  size = 2) +
  geom_point(data = M23_count.df.M, aes(`seed`,`final`), color = 'royalblue4', size = 2) +  
  geom_smooth(data = M23_count.df.H, aes(`seed`,`final`, color = 'firebrick1'), size = 1, method = 'lm', se = F) + 
  geom_smooth(data = M23_count.df.M, aes(`seed`,`final`, color = 'royalblue4'), size = 1, method = 'lm', se = F) +
  labs(x='Seeding density (x10^5 cells/ml)', 
       y='Final cell count (x10^5 cells/ml)') + 
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank() + theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 12),
                            axis.text.y = element_text(color = 'black', size = 12),
                            strip.text.x = element_text(size = 16, color = 'black'),
                            strip.background = element_rect(fill = "white", colour = 'black'),
                            panel.background = element_rect(fill = "white", colour = NA),
                            panel.border = element_rect(colour = 'black', fill = NA),
                            axis.title.x = element_text(color = 'black', size = 16),
                            axis.title.y = element_text(color = 'black', size = 16),
                            legend.text = element_text(size = 12),
                            legend.title =  element_text(size = 14)) +
  facet_wrap(.~`Exp`, labeller = labeller(`Exp` = c('2' = 'Method 2', '3' = 'Method 3')))

# cor tests:
# M2
cor.test(M23_count.df.M$seed[1:9], M23_count.df.M$final[1:9])$estimate
cor.test(M23_count.df.M$seed[1:9], M23_count.df.M$final[1:9])$p.value
cor.test(M23_count.df.H$seed[1:9], M23_count.df.H$final[1:9])$estimate
cor.test(M23_count.df.H$seed[1:9], M23_count.df.H$final[1:9])$p.value
# M3
cor.test(M23_count.df.M$seed[10:18], M23_count.df.M$final[10:18])$estimate
cor.test(M23_count.df.M$seed[10:18], M23_count.df.M$final[10:18])$p.value
cor.test(M23_count.df.H$seed[10:18], M23_count.df.H$final[10:18])$estimate
cor.test(M23_count.df.H$seed[10:18], M23_count.df.H$final[10:18])$p.value

## s.count v prot
prot123 <- prot[1:54,]
count.prot.weight.df <- data.frame('Cell.Line' = aim2_72_MD.ord$X2, 'prot' = prot123, 'seed' = aim2_72_MD.ord$Seed, 
                            'final' = count[1:54,], 'weight' = All.quant[,2][1:54])
count.prot.df <- data.frame('Cell.Line' = aim2_72_MD.ord$X2, 'prot' = prot123, 'seed' = aim2_72_MD.ord$Seed, 
                            'final' = count[1:54,], 'Exp' = aim2_72_MD.ord$Exp)
count.prot.df.M <- count.prot.df[count.prot.df$Cell.Line == 'MSTO',]
count.prot.df.H <- count.prot.df[count.prot.df$Cell.Line != 'MSTO',]
count.prot.df.M[,2:3] <- apply(count.prot.df.M[,2:3], 2, as.numeric)
count.prot.df.H[,2:3] <- apply(count.prot.df.H[,2:3], 2, as.numeric)

CvP <- ggplot(data = count.prot.df.M, aes(`seed`,`prot`)) + 
  geom_point(data = count.prot.df.H, aes(`seed`,`prot`), color = 'firebrick1',  size = 2) +
  geom_point(data = count.prot.df.M, aes(`seed`,`prot`), color = 'royalblue4', size = 2) +  
  geom_smooth(data = count.prot.df.H, aes(`seed`,`prot`, color = 'firebrick1'), size = 1, method = 'lm', se = TRUE) + 
  geom_smooth(data = count.prot.df.M, aes(`seed`,`prot`, color = 'royalblue4'), size = 1, method = 'lm', se = TRUE) +
  labs(x='Seeding density (x10^5 cells/ml)', 
       y='Total protein concentration (mg/ml)') + 
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank() + theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 12),
                            axis.text.y = element_text(color = 'black', size = 12),
                            axis.title.x = element_text(color = 'black', size = 16),
                            axis.title.y = element_text(color = 'black', size = 14),
                            legend.text = element_text(size = 12),
                            legend.title =  element_text(size = 14))

# cor tests:
cor.test(count.prot.df.M$seed, count.prot.df.M$prot)$estimate
cor.test(count.prot.df.M$seed, count.prot.df.M$prot)$p.value
cor.test(count.prot.df.H$seed, count.prot.df.H$prot)$estimate
cor.test(count.prot.df.H$seed, count.prot.df.H$prot)$p.value

CvP.sep <- ggplot(data = count.prot.df.M, aes(`seed`,`prot`, group = `Exp`)) + 
  geom_point(data = count.prot.df.H, aes(`seed`,`prot`), color = 'firebrick1',  size = 2) +
  geom_point(data = count.prot.df.M, aes(`seed`,`prot`), color = 'royalblue4', size = 2) +  
  geom_smooth(data = count.prot.df.H, aes(`seed`,`prot`, color = 'firebrick1'), size = 1, method = 'lm', se = F) + 
  geom_smooth(data = count.prot.df.M, aes(`seed`,`prot`, color = 'royalblue4'), size = 1, method = 'lm', se = F) +
  labs(x='Seeding density (x10^5 cells/ml)', 
       y='Total protein concentration (mg/ml)') + 
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank.facet() +
  theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 12),
          axis.text.y = element_text(color = 'black', size = 12),
          strip.text.x = element_text(size = 16, color = 'black'),
          strip.background = element_rect(fill = "white", colour = 'black'),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(colour = 'black', fill = NA),
          axis.title.x = element_text(color = 'black', size = 16),
          axis.title.y = element_text(color = 'black', size = 16),
          legend.text = element_text(size = 12),
          legend.title =  element_text(size = 14)) +
  facet_wrap(.~`Exp`, labeller = labeller(`Exp` = c('1' = 'Method 1','2' = 'Method 2', '3' = 'Method 3')))

# cor tests:
cor.test(count.prot.df.M$seed, count.prot.df.M$prot)$estimate
cor.test(count.prot.df.M$seed, count.prot.df.M$prot)$p.value
cor.test(count.prot.df.H$seed, count.prot.df.H$prot)$estimate
cor.test(count.prot.df.H$seed, count.prot.df.H$prot)$p.value


# repeat using final cell count (exp 2 and 3)
f.count.prot.df <- count.prot.df[19:54,]
f.count.prot.df.M <- f.count.prot.df[f.count.prot.df$Cell.Line == 'MSTO',]
f.count.prot.df.H <- f.count.prot.df[f.count.prot.df$Cell.Line != 'MSTO',]
f.count.prot.df.M[,2:4] <- apply(f.count.prot.df.M[,2:4], 2, as.numeric)
f.count.prot.df.H[,2:4] <- apply(f.count.prot.df.H[,2:4], 2, as.numeric)
f.CvP <- ggplot(data = f.count.prot.df.M, aes(`final`,`prot`)) + 
  geom_point(data = f.count.prot.df.H, aes(`final`,`prot`), color = 'firebrick1',  size = 2) +
  geom_point(data = f.count.prot.df.M, aes(`final`,`prot`), color = 'royalblue4', size = 2) +  
  geom_smooth(data = f.count.prot.df.H, aes(`final`,`prot`, color = 'firebrick1'), size = 1, method = 'lm', se = TRUE) + 
  geom_smooth(data = f.count.prot.df.M, aes(`final`,`prot`, color = 'royalblue4'), size = 1, method = 'lm', se = TRUE) +
  labs(x='Final cell count (x10^5 cells/ml)', 
       y='Total protein concentration (mg/ml)') + 
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank() + theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 12),
                            axis.text.y = element_text(color = 'black', size = 12),
                            axis.title.x = element_text(color = 'black', size = 16),
                            axis.title.y = element_text(color = 'black', size = 16),
                            legend.text = element_text(size = 12),
                            legend.title =  element_text(size = 14))

# cor tests:
cor.test(f.count.prot.df.M$final, f.count.prot.df.M$prot)$estimate
cor.test(f.count.prot.df.M$final, f.count.prot.df.M$prot)$p.value
cor.test(f.count.prot.df.H$final, f.count.prot.df.H$prot)$estimate
cor.test(f.count.prot.df.H$final, f.count.prot.df.H$prot)$p.value

# seeding count v prot (exp 2 and 3)
s.CvP.23 <- ggplot(data = f.count.prot.df.M, aes(`seed`,`prot`)) + 
  geom_point(data = f.count.prot.df.H, aes(`seed`,`prot`), color = 'firebrick1',  size = 2) +
  geom_point(data = f.count.prot.df.M, aes(`seed`,`prot`), color = 'royalblue4', size = 2) +  
  geom_smooth(data = f.count.prot.df.H, aes(`seed`,`prot`, color = 'firebrick1'), size = 1, method = 'lm', se = TRUE) + 
  geom_smooth(data = f.count.prot.df.M, aes(`seed`,`prot`, color = 'royalblue4'), size = 1, method = 'lm', se = TRUE) +
  labs(x='Seeding density (x10^5 cells/ml)', 
       y='Total protein concentration (mg/ml)') + 
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank() + theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 12),
                            axis.text.y = element_text(color = 'black', size = 12),
                            axis.title.x = element_text(color = 'black', size = 16),
                            axis.title.y = element_text(color = 'black', size = 16),
                            legend.text = element_text(size = 12),
                            legend.title =  element_text(size = 14))

# cor tests:
cor.test(f.count.prot.df.M$seed, f.count.prot.df.M$prot)$estimate
cor.test(f.count.prot.df.M$seed, f.count.prot.df.M$prot)$p.value
cor.test(f.count.prot.df.H$seed, f.count.prot.df.H$prot)$estimate
cor.test(f.count.prot.df.H$seed, f.count.prot.df.H$prot)$p.value

# cell count v weight
count.prot.weight.df <- data.frame('Cell.Line' = aim2_72_MD.ord$X2, 'prot' = prot123, 'seed' = aim2_72_MD.ord$Seed, 
                                   'final' = count[1:54,], 'weight' = All.quant[,2][1:54])
count.weight.df <- count.prot.weight.df[37:54,]
count.weight.df.M <- count.weight.df[count.weight.df$Cell.Line == 'MSTO',]
count.weight.df.H <- count.weight.df[count.weight.df$Cell.Line != 'MSTO',]
count.weight.df.M[,2:3] <- apply(count.weight.df.M[,2:5], 2, as.numeric)
count.weight.df.H[,2:3] <- apply(count.weight.df.H[,2:5], 2, as.numeric)

s.CvW <- ggplot(data = count.weight.df.M, aes(`seed`,`weight`)) + 
  geom_point(data = count.weight.df.H, aes(`seed`,`weight`), color = 'firebrick1',  size = 2) +
  geom_point(data = count.weight.df.M, aes(`seed`,`weight`), color = 'royalblue4', size = 2) +  
  geom_smooth(data = count.weight.df.H, aes(`seed`,`weight`, color = 'firebrick1'), size = 1, method = 'lm', se = TRUE) + 
  geom_smooth(data = count.weight.df.M, aes(`seed`,`weight`, color = 'royalblue4'), size = 1, method = 'lm', se = TRUE) +
  labs(x='Seeding density (x10^5 cells/ml)', 
       y='Pellet weight (mg)') + 
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank() + theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 12),
                            axis.text.y = element_text(color = 'black', size = 12),
                            axis.title.x = element_text(color = 'black', size = 16),
                            axis.title.y = element_text(color = 'black', size = 16),
                            legend.text = element_text(size = 12),
                            legend.title =  element_text(size = 14))

# cor tests:
cor.test(count.weight.df.M$seed, count.weight.df.M$weight)$estimate
cor.test(count.weight.df.M$seed, count.weight.df.M$weight)$p.value
cor.test(count.weight.df.H$seed, count.weight.df.H$weight)$estimate
cor.test(count.weight.df.H$seed, count.weight.df.H$weight)$p.value


f.CvW <- ggplot(data = count.weight.df.M, aes(`final`,`weight`)) + 
  geom_point(data = count.weight.df.H, aes(`final`,`weight`), color = 'firebrick1',  size = 2) +
  geom_point(data = count.weight.df.M, aes(`final`,`weight`), color = 'royalblue4', size = 2) +  
  geom_smooth(data = count.weight.df.H, aes(`final`,`weight`, color = 'firebrick1'), size = 1, method = 'lm', se = TRUE) + 
  geom_smooth(data = count.weight.df.M, aes(`final`,`weight`, color = 'royalblue4'), size = 1, method = 'lm', se = TRUE) +
  labs(x='final cell count (x10^5 cells/ml)', 
       y='Pellet weight (mg)') + 
  scale_color_identity(name = 'Cell Line', labels = c('H2052','MSTO'), guide = 'legend') +
  Theme.Box.Blank() + theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0, size = 12),
                            axis.text.y = element_text(color = 'black', size = 12),
                            axis.title.x = element_text(color = 'black', size = 16),
                            axis.title.y = element_text(color = 'black', size = 16),
                            legend.text = element_text(size = 12),
                            legend.title =  element_text(size = 14))
# cor tests:
cor.test(count.weight.df.M$final, count.weight.df.M$weight)$estimate
cor.test(count.weight.df.M$final, count.weight.df.M$weight)$p.value
cor.test(count.weight.df.H$final, count.weight.df.H$weight)$estimate
cor.test(count.weight.df.H$final, count.weight.df.H$weight)$p.value

# M3 quant H v M:
#plot weight and count
count.prot.weight.df_M3 <- count.prot.weight.df[37:54,]
barM3count <- ggplot(data = count.prot.weight.df_M3, mapping = aes(`Cell.Line`, as.numeric(`final`))) + 
  geom_boxplot(aes(fill = `Cell.Line`), colour = 'black') + 
  scale_fill_manual(values = c(colorme2[3], colorme2[5])) + 
  labs(x = " ", y = "Final cell density (x10^6 cells)", fill = '') +
  Theme.Box.Blank() + theme(panel.grid.major.y = element_line(colour = blackpancol[1]), 
                            axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
                            legend.position = 'none')

barM3weight <- ggplot(data = count.prot.weight.df_M3, mapping = aes(`seed`, as.numeric(`weight`))) + 
  geom_boxplot(aes(fill = `Cell.Line`), colour = 'black') + 
  scale_fill_manual(values = c(colorme2[3], colorme2[5])) + 
  labs(x = " ", y = "Cell pellet weight (mg)", fill = '') +
  Theme.Box.Blank() + theme(panel.grid.major.y = element_line(colour = blackpancol[1]), 
                            axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
                            legend.position = 'none')

ggarrange(bar24count, bar24weight, 
          barM3count, barM3weight,
          ncol = 2, nrow = 2)
```

# Method 3 versus Method 4 PLS-DA
## PLS and TSNE
```{r, echo=TRUE}
# match variables
M4_M3_match <- M4_24[,which(colnames(M4_24) %in% colnames(M3_72))]
M3_M4_match <- M3_72[,which(colnames(M3_72) %in% colnames(M4_24))]
rownames(M3_M4_match) <- M3_72$SampID
M3M4_matched <- rbind(M3_M4_match, M4_M3_match)
M3M4_matched.exp <- c(rep('M3', each = 18), rep('M4', each = 18))
M3M4_matched.group <- c(M3_72$X2, aim2_24_MD$X2)
M3M4_MD <- data.frame('Exp' = M3M4_matched.exp,
                      'CellLine' = M3M4_matched.group)
M3M4_MD.bin <- data.frame('Exp.bin' = to.dummy(M3M4_matched.exp, 'dum')[,2],
                          'CellLine.bin' = to.dummy(M3M4_matched.group, 'dum')[,2])

# import O2PLS outputs (MATLAB): scores, weights, model coefs (UV)
temp.patho <- list.files(path = "C:/Users/mo316/Documents/1. Meso PhD/1. Year 1/1. Experiments/3. Metabolite extraction_cells/181121_Cell metabolite extraction GCMS_180921_181114/Data/OPLS/",
                         pattern = "_UV", full.names = TRUE)
# [1] "R2Q2.csv"        "scores.csv"      "weights.csv"        
O2PLSout <- lapply(temp.patho, function(x) as.data.frame(read.csv(x,	header	=	FALSE,	check.names	=	FALSE)))
# Plot results (1PC 2OC)
P2Ouv_coefs <- O2PLSout[[1]]; colnames(P2Ouv_coefs)[1:3] <- c('R2Y', 'Q2Y', 'R2X'); colnames(P2Ouv_coefs)[ncol(P2Ouv_coefs)] <- c('Pval')
P2Ouv_scores <- O2PLSout[[2]]; rownames(P2Ouv_scores) <- rownames(M3M4_matched) 
P2Ouv_weight <- O2PLSout[[3]]; rownames(P2Ouv_weight) <- colnames(M3M4_matched)

# P2Ouv_scores plot
pC <- ggplot(data = P2Ouv_scores, mapping = aes(P2Ouv_scores[,1], P2Ouv_scores[,2])) + 
  labs(x = paste0('t1 ', '(',sprintf(0.6991*100, fmt = '%#.1f'), '%', ')', 
                  "\n", 'R2Y: ', sprintf(P2Ouv_coefs$R2Y, fmt = '%#.3f'), '     ', 
                  'Q2Y: ', sprintf(P2Ouv_coefs$Q2Y, fmt = '%#.3f'), '     ', 
                  'P = ', sprintf(P2Ouv_coefs$Pval, fmt = '%#.4f')), 
       y = paste0('t2 ', '(',sprintf((P2Ouv_coefs[3]-0.6991)*100, fmt = '%#.1f'), '%', ')'), 
       fill = 'Method', color  = 'Cell Line') +
  geom_point(data = P2Ouv_scores, aes(P2Ouv_scores[,1], P2Ouv_scores[,2], fill = M3M4_MD$Exp, color = M3M4_MD$CellLine), # Change fill object
             size = 7, shape = 21, stroke = 1.5) +
  scale_fill_manual(breaks = c('M3', 'M4'), 
                    values = c(colorme2[4], colorme[10])) +
  scale_color_manual(breaks = c('H2052', 'MSTO'), 
                     values = c('firebrick1', 'royalblue4')) +
  stat_ellipse(type = "norm", color = 'grey') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  Theme.PCA.Blank()

# P2Ouv_weights plot
mnC <- data.frame(rownames(P2Ouv_weight),P2Ouv_weight)
qC <- ggplot(data = mnC, mapping = aes(mnC[,2], mnC[,3], label = mnC[,1])) + 
  labs(x = "w*c1", y = "w*c2") +
  geom_point(color = colorme[8], size = 2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 1) +
  stat_ellipse(type = "norm", color = 'grey') +
  Theme.PCA.Blank()

#tSNE:
tsne(t(M3M4_matched), labels = M3M4_matched.exp, dotsize = 4, axistextsize = 12, perplex = 10, 
     legendtextsize = 14, legendtitle = 'Method 3 vs 4') + 
  scale_colour_manual(values = c(blackpancol[3], blackpancol[9]))

```

## Barplot
```{r, echo=TRUE}
# plot total yields betwen M3 and M4
M3M4_summary <- data.frame(rbind(summary(rowSums(M3M4_matched[1:18,])),summary(rowSums(M3M4_matched[19:36,]))))
rownames(M3M4_summary)[] <- c('M3','M4')

met.bar4 <- ggplot(data = M3M4_summary, mapping = aes(rownames(M3M4_summary), as.numeric(`Mean`))) 
met.bar4 + geom_col(aes(fill = rownames(M3M4_summary)), colour = 'black') + 
  geom_errorbar(aes(ymin = `Min.`, ymax = `Max.`), width = 0.4, col = 'black', size = 1) +
  scale_fill_manual(values = c(colorme2[4], colorme[10])) + 
  labs(x = " ", y = "Mean normalised metabolite intensity (AU)", fill = '') +
  Theme.Box.Blank() + theme(panel.grid.major.y = element_line(colour = blackpancol[1]), 
                            axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
                            legend.position = 'none')
M3M4_matched.melt <- melt(data.frame(M3M4_MD, M3M4_matched))

pairwise.t.test(M3M4_matched.melt$value,M3M4_matched.melt$Exp, 
                p.adjust.method = 'BH', paired = FALSE) #5.3e-09
```

## Quantification methods
```{r, echo=TRUE}
aim2_24_quant <- data.frame('X2' = aim2_24_MD$X2, All.quant_24)
rownames(aim2_24_quant) <- aim2_24_MD$X3
#plot weight and count
bar24count <- ggplot(data = aim2_24_quant, mapping = aes(`X2`, as.numeric(`count`))) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(aes(fill = `X2`), colour = 'black') + 
  scale_fill_manual(values = c(colorme2[3], colorme2[5])) + 
  labs(x = " ", y = "Final cell density (x10^6 cells/ml)", fill = '') +
  Theme.Box.Blank() + theme(panel.grid.major.y = element_line(colour = blackpancol[1]), 
                            axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
                            legend.position = 'none')

bar24weight <- ggplot(data = aim2_24_quant, mapping = aes(`X2`, as.numeric(`weight`))) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(aes(fill = `X2`), colour = 'black') + 
  scale_fill_manual(values = c(colorme2[3], colorme2[5])) + 
  labs(x = " ", y = "Cell pellet weight (mg)", fill = '') +
  Theme.Box.Blank() + theme(panel.grid.major.y = element_line(colour = blackpancol[1]), 
                            axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
                            legend.position = 'none')

ggarrange(bar24count, bar24weight, ncol = 2)
aim2_24_quant.melt <- melt(aim2_24_quant)
pairwise.t.test(aim2_24_quant.melt$value[1:18],aim2_24_quant.melt$X2[1:18], 
                p.adjust.method = 'BH', paired = FALSE) #0.01
pairwise.t.test(aim2_24_quant.melt$value[19:36],aim2_24_quant.melt$X2[19:36], 
                p.adjust.method = 'BH', paired = FALSE) #0.09
```
