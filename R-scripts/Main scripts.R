#######################################################################################################
# MAIN SCRIPTS IS A CONSTANT WORK IN PROGRESS, PLEASE CHANGE ARGUMENTS AS APPROPRIATE TO YOUR SCRIPTS #
#                          BY MICHAEL OLANIPEKUN (Updated:27/01/2023)                                 #           
#######################################################################################################
######################
####    SETUP     ####
######################
getwd()
setwd("C:/Users/mo316/Documents/1. Meso PhD/5. Scripts/R/work-directory")   # change working directory

#Adding library paths
.libPaths(new = c('\\icnas1.cc.ic.ac.uk/mo316/R/win-library/3.5', 'C:/Users/mo316/Documents/1. Meso PhD/5. Scripts/R/win-library/3.5'))
.libPaths()

load("R environment.RData")

#read file
DS <- as.matrix(read.csv("EXAMPLE.csv",	header	=	TRUE, row.names = 1,	check.names	=	FALSE))
# read.csv(... skip = 1) skips the first row to make the second row the header
xldf <- read.xlsx('Datadatadata.xlsx', 2) #number shows the sheet of the xl workbook

#####################
#### Plot Themes ####
#####################
library(RColorBrewer)
library(scales)
colorme <- c('#FDAE61', '#80B1D3', '#FB8072', '#BEBADA', '#FDE0EF', '#B3E2CD', '#FDCDAC', '#80CDC1', '#FFF2AE','#AA9486')
colorme2 <- c('#82B886', '#E9BF7C', '#E6637A', '#8C6EB8', 
              '#628BEB', '#838536', '#EBA29C', '#99A0B6', 
              '#B8887B','#B5A991' ,'#61B8A5', '#366F85', '#F09870')
colMAMBA <- c('#8E5ED1','#5A3C85', '#49306BFF',
              '#BAA2DE','#FFFFFFFF', '#E6C962',
              '#E6AC34', '#C28C21', '#8A6417')
blackpancol <- c('#B7A4EE', '#664EAE','#1A0554', 
                 '#969696', '#252525', 'black', 
                 '#FFCA36', '#9ebcda', 'sienna4')
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

Theme.Box.Blank.facet <- function() {
  Theme.Box.Blank() + 
    theme(axis.text.x = element_text(color = 'black', hjust = 0.5, angle = 0),
          axis.text.y = element_text(color = 'black'),
          strip.text.x = element_text(size = 11, color = 'black'),
          strip.background = element_rect(fill = "white", colour = 'black'),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(colour = 'black', fill = NA))
}

########################
#### EXTRA FUNTIONS ####
########################
col2rowname <- function(x){
  rownames(x) <- x[,1]
  x <- x[,-1]
}
plot.RNA.CNV <- function(zzy){
  gg <- ggplot(data = zzy, aes(reorder(as.character(zzy[,1]), zzy[,1]), zzy[,2]))
  gg + geom_boxplot(fill = colorme[7], color = 'black') +
    ylab('mRNA Expression') + 
    xlab('Gene Copy Number') + 
    Theme.Box.Blank() +
    theme(axis.text.x = element_text(size = 10, color = 'black', angle = 0, hjust = 0.4), 
          axis.text.y = element_text(size = 10, color = 'black'),
          plot.title = element_text(size = 15, hjust = 0.5)) +
    theme(legend.position = "none")
}
plot.pls.age <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@scoreMN[,2] #PC2
  ab <- data.frame(a,b,as.numeric(ages))
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = "P1(28%)", y = "P2(16%)", fill = 'Age') +
    geom_point(data = ab, aes(a, b, fill = as.numeric(ages)), size = 7, color = 'black', shape = 21) +
    scale_fill_gradient(low = colorme[9], high = brewer.pal(4, 'Spectral')[1]) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank()
}
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}


######################
####   LIBRARY    ####
######################
library(ropls)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(wesanderson)
library(gridExtra)
library(MWASTools)
library(ComplexHeatmap)
library(circlize)
library(MetaboSignal)
library(MetaboAnalystR)
library(reshape2)
#glm
library(car)
library(heplots)
library(tidyverse)
library(lm.beta)
library(openxlsx)
library(varhandle)
library(WGCNA)



##############################
####    DATA HANDLING     ####
##############################
# Quick matrix maker to test functions and strings
x = matrix(data = c(1:9), nrow = 3, ncol = 3)
x1 = apply(as.array(x), 1, as.character)
class(x1[1])

# Convert to numeric
ds=apply(ds, 2, as.numeric)
KO2_iso_gn2.3 <- transform(KO2_iso_gn2.2, name = as.numeric(name)) #specify columns to make numeric

# Convert to binary
levels(CSI2.2$BBB) <- c(1,0) # 1) did not work but could for other data
library(varhandle)
to.dummy(CSI2.2$BBB, "csi2_") # 2) Creates new vectors of dummy (binary) columns for each category


# Sort data table to get most abundant variables (matching across multiple columns/datasets)
abund = function(x) as(names(which.max(table(x))), mode(x)) # Returns most abundant.
VVIP = as.matrix(sort(table(ds1), decreasing = TRUE)[1:10]) # Returns range of most abundant

# Get frequency of value
count(Variables_info_672, `SUPER PATHWAY`)

# Add suffix to column names
colnames(tab) <- paste(colnames(tab), "x", sep = "_")

# Remove column
GCMS_data <- GCMS_data[,-(55), drop = FALSE]  

# Remove row by name
ix <- which(rownames(CSI1) %in% c("IMPE-00936", "IMPE-00962")) 
CSIR <- CSI1[-ix, ] 
CSIR['IMPE-00936',]

# Insert column of rownames
'Compounds' <- row.names(CSI3)
CSIy <- cbind(Compounds, CSI3)

# Insert row as header (names)
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

# Change single colname
names(Serum.Abs)[1] <- 'MCid'

# Matching
Matched <- match_df(A, B, on = "V1") #matched samples - use on = "" to explain which column to match on

# Filter dataframe based on column variable
CSI2.3.N <- CSI2.3[CSI2.3$csi2_.SC_N == '1',]
md.filt <- md2[md2$BMI_C > 25 & md2$BMI_C < 30 & md2$GENDER == 'Male' | 
              md2$BMI_C > 22 & md2$BMI_C < 28 & md2$GENDER == 'Female' , ]
#Search for part of a string
library(data.table)
df[,1] %like% '9.72'

#Melt a table for visualisation
melt.pq <- melt(pq, id.vars = 'CDKN2A')

#Assign which rows are NA
Meso_MD.NA <- which(is.na(Meso_MD3$CDKN2A))
#Remove rows with NA in a corresponding df
DrBrG60LGP2 <- DrBrG60LGP1[-c(Meso_MD.NA),]

#Remove duplicate rows based on single column
GCMS_serum1 <- GCMS_serum[!duplicated(GCMS_serum[,1]), ]

#Order df alphabetically
temp2 <- temp[order(temp$Gene),]
AAm.san.top1 <- AAm.san.top[order(AAm.san.top), , drop = FALSE]

#paste part of a string of characters
substring(paste(Variable_IDs_HMDB$HMDB), first = 1, last = 4)

#remove spaces from a string
gsub('[[:space:]]', '', origin1$KEGG)
# Remove text from a string to split into individual elements:
str_split(temp, ",")

# Summary of table: mean and sd
sum.df <- ddply(df, c("group"), summarise,
              N = length(value),
              mean = mean(value),
              sd = sd(value),
              se = sd/sqrt(N))

# split df into list by groups
library(tidyverse)
CC.3_ls <- data.frame(G1,Tr1,CC.3) %>% group_split(G1)

# Unlist but keep list index
KO2_iso_gn2 <- KO2_iso_gn %>% 
  set_names(seq_along(.)) %>%
  enframe %>%
  unnest(cols = c(value))
# > # A tibble: 176,107 x 2
# name  KO         
# <chr> <chr>      
#   1 1     ""         
# 2 1     "ko:K00003"
# 3 1     "ko:K00008"
# 4 1     "ko:K00014"
# 5 1     "ko:K00016"
# (...)

unique() # finds what is unique in an object
intersect() # finds the overlap between two objects
sym_diff <- function(a,b) unique(c(setdiff(a,b), setdiff(b,a)))
Tmp3 <- sym_diff(Tmp1, Tmp2) # opposite of intersect

###################################
####    DATA PREPROCESSING     ####
###################################
# PQN normalisation with MetaboMate ### WIP! 
# install: devtools::install_github('tkimhofer/MetaboMate', build_vignettes=TRUE)
library(MetaboMate)
# Perform PQN correction using the QCs as a 'reference.idx', and assigning the dilution factor to the object 'dilf'
# ds = data, an = sample classifiers, dn = data norm
# examples:
dn=pqn(ds, reference.idx = NA, TArea = F, add.DilF = NULL)
dn=pqn(ds, reference.idx = which(an=='QC'), add.DilF = 'dilf')   

#divide cols of df by single col repeated (e.g. dividing by cell density to normalise)
df2 <- df1/rep(A, times = length(df1[1,])) 

# Replace all NAs with the smallest detected value
for(i in 1:length(df[1,])){ 
  set = df[,i]
  set2 = set[!is.na(set)]
  rep.val = min(set2)
  set[is.na(set)] <- rep.val
  df[,i] = set
}

# imputation methods: ####
# LOESS
library(spatialEco)
test <- impute.loess(B3[,5], s = 0.2, smooth = F)
View(cbind(B3[,5], test))

library(imputeLCMD)
test_LCMD <- impute.MAR.MNAR(Ori2, 
                             model.selector = model.Selector(Ori2),
                             method.MNAR = 'QRILC')


######################
####    LOOPS     ####
######################
## ANOVA loop:
a_vector = vector(length = (ncol(CellNorm2) - 1))  

for (i in 1:ncol(CellNorm2)) {                     
  print(i)                                        
  y <- CellLine                               
  aov.test <- try(aov(CellNorm2[,i] ~ y, data=as.data.frame(CellNorm2)), silent = TRUE) # change this line to change the function to loop
  #print(aov.test)
  
  if(class(aov.test)[1] != "try-error") {
    a_vector[i] = summary(aov.test)[[1]][["Pr(>F)"]]       
    
  }
}
View(a_vector)

## PLS loop (from Andrea)
library('ropls')
r_vector = vector(length = (ncol(Matched2) - 1))  # Make empty vectors of the correct dimensions
q_vector = vector(length = (ncol(Matched2) -1))

for (i in 2:ncol(Matched2)) {                     # For i at any position in our dataset, the following will be executed:
  print(i)                                        # i will be shown, revealing the current position
  y <- Matched2[,i]                               # i in our dataset is assigned to y
  pls1 <- try(opls(x, y), silent = TRUE)          # The opls function is performed and the results are assigned to "pls1", x has been assigned previously
  #print(pls1)
  
  if(class(pls1)[1] != "try-error") {             # The pls result will only be printed in our vector if there is no error. the loop will continue to the next one
    r_vector[i] = pls1@summaryDF$`R2Y(cum)`       # and the values for the R2Y and Q2 will be placed in our empty vector
    q_vector[i] = pls1@summaryDF$`Q2(cum)`
    
  }
}

## PLS loop optimised
r_vector = vector(length = (ncol(Matched2) - 1))  # Make empty vectors of the correct dimensions
q_vector = vector(length = (ncol(Matched2) -1))

for (i in 2:ncol(Matched2)) {                        # For i at any position in our dataset, the following will be executed:
  print(i)                                           # i will be shown, revealing the current position
  y <- Matched2[,i]                                  # i in our dataset is assigned to y
  pls1 <- try(opls(x, y), silent = TRUE)             # The opls function is performed and the results are assigned to "pls1", x has been assigned previously
  #print(pls1)
  
  if(class(pls1)[1] != "try-error") {                # The pls result will only be printed in our vector if there is no error. the loop will continue to the next one
    r_vector[i] = pls1@summaryDF$`R2Y(cum)`
    q_vector[i] = pls1@summaryDF$`Q2(cum)`
  }                                                     
  else if(class(pls1)[1] == "try-error")             # Only if the pls fails and comes up with an error..
    pls1 <- try(opls(x,y, ortho = 1), silent = TRUE) # the opls will be performed with 1 orthogonal component
  r_vector [i] = pls1@summaryDF$`R2Y(cum)`           # and the values for the R2Y and Q2 will be placed in our empty vector
  q_vector [i] = pls1@summaryDF$`Q2(cum)`
}

#Convert p to q values
Q.val <- qvalue::qvalue(P.val, lambda = 0, fdr.level = 0.05)$qvalues #equivalent to BH adj

## loop through object depending on i in the name
for (i in 1:15){
  PAlist_HvM[[i]] <- MS_findMappedNodes(nodes = HvM_PAquery_KEGG, get(paste0('MS_network', i)))[['mapped_nodes']]
  PAlist_MvI[[i]] <- MS_findMappedNodes(nodes = MvI_PAquery_KEGG, get(paste0('MS_network', i)))[['mapped_nodes']]
  PAlist_HvI[[i]] <- MS_findMappedNodes(nodes = HvI_PAquery_KEGG, get(paste0('MS_network', i)))[['mapped_nodes']]
}

########################
####    GGPLOT2     ####
########################
library(ggplot2)
#GGPLOT GUI
library("ggplotgui")
ggplot_shiny(xy)

# Scatterplot
library(ggplot2)
x <- as.numeric(unlist(Matched2))                                                 # Assign variables to x and y, to be plotted. Here, two forms of matched data are assigned as lists to each
y <- as.numeric(unlist(Matched3))
all.matched <- data.frame(x, y)                                                   # Combine the data to be plotted into a single vector, as a data frame
scatter_plot <- ggplot(data = all.matched, mapping = aes (x, y))                  # Assign the plot to a vector. 'data' should use the single data frame created and the aesthetics (aes) should be your variables to be plotted
scatter_plot + geom_point() + labs(x = "Batch 1", y = "Batch 2") + geom_smooth()  # Affect the formatting of the plot to be made

# Boxplot
CellNorm3_672[,'5-dodecenoylcarnitine (C12:1)']
Meso_MD3$NF2
xy <- data.frame(CellNorm3_672[,'5-dodecenoylcarnitine (C12:1)'], Meso_MD3$NF2)
xy <- na.omit(xy)
gg.5doc.nf2 <- ggplot(data = xy, aes(as.character(xy[,2]), xy[,1]))
gg.5doc.nf2 + geom_boxplot(fill = "#D9D0D3", color = 'black') +
  ylab('Normalised instensity') + 
  xlab('NF2 Copy Number Variation') + 
  ggtitle('5-dodecenoylcarnitine (C12:1)') +
  Theme.Box.Blank() +
  theme(axis.text.x = element_text(size = 10, color = 'black', angle = 0, hjust = 0.4), 
        axis.text.y = element_text(size = 10, color = 'black'),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  theme(legend.position = "none")
#for colouring jitter: geom_jitter(aes(color = melt.qr[,2])) +

# Barchart (geom col)
xy <- meso_anova3
bp <- ggplot(xy, aes(reorder(rownames(xy), -xy[,3]), xy[,3], color = NULL, fill = xy[,3]))
bp + geom_col(fill = "#66C2A5", color = "black") + #to make all bars one colour it must be specified in the geom_col()
  ylab('-Log(p)') + 
  xlab('Compounds') + 
  Theme.Box.Blank()  +
  theme(axis.text.x = element_text(size = 6, color = 'black')) +
  #scale_fill_continuous(low="darkred", high="darkgreen") +
  theme(legend.position = "none")

#barchart coloured by p values (numeric cut)
bp <- ggplot(xy, aes(xy[,1], xy[,2], color = NULL, fill = cut(xy[,3], c(-Inf, 0.05, Inf))))
bp + geom_col(color = "black") + 
  ylab('Pearson correlation coef') + 
  xlab('Genes') + 
  Theme.Box.Blank()  +
  theme(axis.text.x = element_text(size = 6, color = 'black')) +
  labs() +
  scale_fill_manual(name = 'P value',
                    values = c('(-Inf,0.05]' = colorme[10],
                               '(0.05, Inf]' = 'white'),
                    labels = c('<0.05', '>0.05'))

# Barchart flip axis
glm.gg <- ggplot(data = as.data.frame(GLM_output), aes(reorder(rownames(GLM_output), GLM_output[,"TurqNF2_P.Eta.Sq."]), GLM_output[,"TurqNF2_P.Eta.Sq."]))
glm.gg + 
  geom_bar(stat = 'identity', fill = "#9986A5", color = "black") + #to make all bars one colour it must be specified in the geom_col()
  ylab('Effect Size (Eta squared)') + 
  xlab('') + 
  Theme.Box.Blank()  +
  theme(axis.text.x = element_text(size = 10, color = 'black', angle = 0, hjust = 0.4), axis.text.y = element_text(size = 10, color = 'black')) +
  #scale_fill_continuous(low="darkred", high="darkgreen") +
  theme(legend.position = "none") +
  #geom_hline(yintercept = 1.3, linetype = 1, color = 'blue') #+
  coord_flip()

# Lollipop plot
lp3 <- ggplot(def, aes(reorder(def[,1], -def[,2]), def[,2]))
lp3 + 
  geom_segment(aes(x = reorder(def[,1], def[,2]), xend = reorder(def[,1], def[,2]), y = 0, yend = def[,2]), color='#1C1718',
               size = 1) +
  geom_point(color = "#3B9AB2", size = (def[,3]*2)) +
  Theme.Box.Blank() +
  xlab('') + ylab('-Log(P)') +
  theme(axis.text.x = element_text(size = 7, color = 'black', hjust = 0.5, angle = 0),
        axis.text.y = element_text(size = 10, hjust = 1, vjust = 0.3)) +
  geom_hline(yintercept = 1.3, linetype = 1, color = '#EBCC2A', alpha = 0.8) +
  coord_flip()

# Cleveland plot
zy <- cbind(CAPLS.man.min, CAPLS.man.max)
zy.melt <- melt(as.matrix(zy), id.vars = rownames(zy), measure.vars = c(zy[,1], zy[,2]))

manhattan.cleveplot <- ggplot(zy.melt, aes(x = zy.melt[,3], y= zy.melt[,1], group = zy.melt[,1])) +
  geom_point(aes(color = zy.melt[,2]), size = 2) +
  geom_line(color = colorme[2]) + 
  scale_color_manual(breaks = c('Min', 'Max'), values = c(colorme[1], colorme[3])) +
  xlab('-Log(P) x (Beta/|Beta|)') + ylab('') +
  Theme.Box.Blank() + 
  theme(axis.text.x = element_text(size = 10, color = 'black', hjust = 0.4, angle = 0),
        axis.text.y = element_text(size = 10, hjust = 1, vjust = 0.3)) +
  geom_vline(aes(xintercept = -1.3), color = colorme[4], linetype = 'longdash') +
  geom_vline(aes(xintercept = 1.3), color = colorme[4], linetype = 'longdash')
manhattan.cleveplot

# Plot functions [WIP]
plot.opls <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@orthoScoreMN[,1] #Ort1
  ab <- data.frame(a,b,G2)
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = "P1(18%)", y = "Ort1(18%)", fill = 'Timepoints') +
    geom_point(data = ab, aes(a, b, fill = reorder(G2)), size = 7, color = 'black', shape = 21) +
    scale_fill_manual(values = c(brewer.pal(7, 'Greens'))) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank()
}

A = CellNorm3.opls@scoreMN[,1]
B = CellNorm3.opls@orthoScoreMN[,1]
an = Sample_Groups2$CANCER_STATUS
an = as.factor(an)
class(an) = "factor"
ABC <- data.frame(A, B, an)
scat2 <- ggplot(data = ABC, mapping = aes (A, B))
scat2 + 
  labs(x = "T1 (8%)", y = "TO1 (8%)", fill = "Cancer status") +
  geom_point(data = ABC, aes(A, B, fill = an), size = 7, shape = 21, color = 'black') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_fill_manual(labels = c('Normal','Tumour'), values = c(colorme)) +
  Theme.PCA.Blank() +
  stat_ellipse(type = "norm", color = 'grey')

# ggplot themes
library(RColorBrewer)
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

# layouts
pls <- ggarrange(pda1, pda2, pda3, ncol = 3)
ggarrange(ggarrange(TpTs3, TpTs2, ncol = 2), TpTs1, nrow = 2)

######################
####    ROPLS     ####
######################
library(ropls)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(gridExtra)

# bread and butter (PCA)
sus=All.dat[-(1:24),]
PCA <- opls(sus, y= NULL, parAsColFcVn = an, pre=2, ort=0, log10L = FALSE) 
sus.an=an[-(1:24)]
plot(PCA, typeVc = 'x-score', parAsColFcVn = sus.an, parLabVc = sus.an, parEllipsesL = FALSE, parTitleL = FALSE, permI = 1000, crossvalI=47)
plot(PCA, typeVc = 'x-loading')
plot(PCA, typeVc = 'permutation')
# PLSR with ropls
ds.plsr <- opls(MSTO.df[,2:82], MSTO.df[,1], predI = NA, crossval = 5)  # perform PLSR by setting the whole dataset to x and the continuous variable to y. PredI = NA allows autofit, crossval must be set to less than the No. of samples
plot(ds.plsr, typeVc = 'x-score', line = TRUE)

# PCA or PLS and visualisation
Patho.opls <- opls(CellNorm3, y= Meso_MD2$PATHOLOGY, pre=2, ort=0, log10L = FALSE, permI= 1000)
plot(Patho.opls, typeVc = 'x-score', parAsColFcVn = Meso_MD2$PATHOLOGY, parLabVc = CellLine2, 
     parEllipsesL = FALSE, parTitleL = FALSE, crossvalI= NA)
plot(Patho.opls, typeVc = 'x-loading')
A = Patho.opls@scoreMN[,1]
B = Patho.opls@scoreMN[,2]
an = Meso_MD2$PATHOLOGY
an = as.factor(an)
class(an) = "factor"
ABC <- data.frame(A, B, an)
scat2 <- ggplot(data = ABC, mapping = aes (A, B))
sp <- scat2 + labs(x = "PC1 (14%)", y = "PC2 (14%)") + geom_point(data = ABC, aes(A, B, fill = ABC$an), size = 7, shape = 21, color = 'black')
sp + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(fill = "Cell Line Pathology") + 
  scale_fill_manual(breaks = c('Biphasic', 'Epitheliod'), values = c(brewer.pal(6, 'Spectral'))) +
  Theme.PCA.Blank() +
  stat_ellipse(type = "norm", color = 'grey')

# OPLS and visualisation
Passage.opls <- opls(CellNorm3, y= Meso_MD2$PASSAGE, pre=1, ort=1, log10L = FALSE, permI= 1000)
plot(Passage.opls, typeVc = 'x-score', parAsColFcVn = Meso_MD2$PASSAGE, parLabVc = CellLine2, 
     parEllipsesL = FALSE, parTitleL = FALSE, crossvalI= NA)
plot(Passage.opls, typeVc = 'x-loading')
A = Passage.opls@scoreMN[,1]
B = Passage.opls@orthoScoreMN[,1]
an =Meso_MD2$PASSAGE
an = as.factor(an)
class(an) = "factor"
ABC <- data.frame(A, B, an)
scat2 <- ggplot(data = ABC, mapping = aes (A, B))
sp <- scat2 + labs(x = "P1 (9%)", y = "O1 (9%)") + geom_point(data = ABC, aes(A, B, fill = ABC$an), size = 7, shape = 21, color = 'black')
sp + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  labs(fill = "Passage No.") + 
  scale_fill_manual(values = c(brewer.pal(6, 'Spectral'))) +
  Theme.PCA.Blank() +
  stat_ellipse(type = "norm", color = 'grey')

#BEST VISUALISATION so far:
a <- CerA.pls@scoreMN[,1] #PC1
b <- CerA.pls@orthoScoreMN[,1] #Ort1
ab <- data.frame(a,b,'ALI' = Cer_MD$ALI, 'Matrix' = Cer_MD$Matrix)
p <- ggplot(data = ab, mapping = aes(a, b, group = `Matrix`))
p +  geom_point(data = ab, aes(a, b, fill = `ALI`, shape = `Matrix`), 
                size = 7, stroke = 1.5, color = 'black') +
  labs(x = paste0('t1 ', '(', CerA.pls@modelDF$R2X[1]*100, '%', ')', # add text annotations of model params
                  "\n", 'R2Y: ', sprintf(CerA.pls@summaryDF$R2Y, fmt = '%#.3f'), '     ', 
                  'Q2Y: ', sprintf(CerA.pls@summaryDF$Q2, fmt = '%#.3f'), '     ', 
                  'pR2Y = ', sprintf(CerA.pls@summaryDF$pR2Y, fmt = '%#.3f'), '     ',
                  'pQ2 = ', sprintf(CerA.pls@summaryDF$pQ2, fmt = '%#.3f')),
       y = paste0('to1 ', '(', CerA.pls@modelDF$R2X[2]*100, '%', ')'), 
       fill = '', shape = '') +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  scale_fill_manual(breaks = c('Differentiated', 'Undifferentiated'), 
                    values = c(colorme[4], colorme[1])) +
  scale_shape_manual(values = c(21, 24)) +
  stat_ellipse(type = "norm", color = 'grey') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  Theme.PCA.Blank() 



#plot.functions:
plot.pls <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@scoreMN[,2] #PC2
  ab <- data.frame(a,b,Groups3$PATGROUP)
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = paste('t1', x@modelDF$R2X[1]*100, '%'), y = paste('t2', x@modelDF$R2X[2]*100, '%'), fill = ab[,3]) +
    geom_point(data = ab, aes(a, b, fill = ab[,3]), size = 4, color = 'black', shape = 21) +
    #scale_fill_manual(values = c(brewer.pal(8, 'Spectral'))) +
    #scale_fill_gradient2(low= colorme[2], mid='snow3', high=colorme[3]) +
    stat_ellipse(type = "norm", color = 'grey') +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    Theme.PCA.Blank()
}
plot.pls2 <- function(x){
  a <- x@scoreMN[,1] #PC1
  b <- x@scoreMN[,2] #PC2
  ab <- data.frame(a,b,Groups.IHD4$Group)
  p <- ggplot(data = ab, mapping = aes (a, b))
  p + labs(x = paste('t1', x@modelDF$R2X[1]*100, '%'), y = paste('t2', x@modelDF$R2X[2]*100, '%'), fill = 'PATGROUP_C') +
    geom_point(data = ab, aes(a, b, fill = ab[,3]), size = 4, color = 'black', shape = 21) +
    #scale_fill_manual(values = c(brewer.pal(8, 'Spectral'))) +
    #scale_fill_gradient2(low= colorme[2], mid='snow3', high=colorme[3]) +
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

#plot weights
plot.pls.weights <- function(x){
m <- x@weightStarMN[,1]
n <- x@weightStarMN[,2]
mn <- data.frame(rownames(x@weightStarMN),m,n)
q <- ggplot(data = mn, mapping = aes (m, n, label = mn[,1]))
q + labs(x = "w*c1", y = "w*c2") +
  geom_text() +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  Theme.PCA.Blank()
}
# Updated weights plot (from O2PLS output)
library(ggrepel)
library(ggpmisc)
mn <- data.frame(rownames(BB_O2_weight),BB_O2_weight)
qB <- ggplot(data = mn, mapping = aes(mn[,2], mn[,3], label = mn[,1]))
qB + labs(x = "w*c1", y = "w*c2") +
  geom_point(color = colorme[8], size = 2) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  Theme.PCA.Blank() +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.1) #filters for where the data is sparsest

# layout plots
lls <- get_legend(pB)
grid.arrange(pB + theme(legend.position = 'none') + ggtitle('Differentiated'), 
             pC + theme(legend.position = 'none') + ggtitle('Undifferentiated'), 
             lls,
             ncol = 3, nrow = 1, widths = c(3,3,1)) #plot both

#3D visualisation: ####
library(plotly)
x11()
MC.BMI.opls.p <- plot_ly(x = MC.BMI.opls@scoreMN[,1], y = MC.BMI.opls@scoreMN[,2], z = MC.BMI.opls@scoreMN[,3], 
                         type = 'scatter3d', mode = 'markers', color = Groups.IHD4$Group, colors = 'Spectral')
MC.BMIDIAB.opls.p <- plot_ly(x = MC.BMIDIAB.opls@scoreMN[,1], y = MC.BMIDIAB.opls@scoreMN[,2], z = MC.BMIDIAB.opls@scoreMN[,3], 
                             type = 'scatter3d', mode = 'markers', color = Groups.IHD4$Group, colors = 'Spectral')
MC.BMIDIABIHD.opls.p <- plot_ly(x = MC.BMIDIABIHD.opls@scoreMN[,1], y = MC.BMIDIABIHD.opls@scoreMN[,2], z = MC.BMIDIABIHD.opls@scoreMN[,3], 
                                type = 'scatter3d', mode = 'markers', color = Groups.IHD4$Group, colors = 'Spectral')

htmlwidgets::saveWidget(as_widget(MC.BMI.opls.p), 'IHDpls_GCMSserum_BMI.html')
htmlwidgets::saveWidget(as_widget(MC.BMIDIAB.opls.p), 'IHDpls_GCMSserum_BMI_DIAB.html')
htmlwidgets::saveWidget(as_widget(MC.BMIDIABIHD.opls.p), 'IHDpls_GCMSserum_BMI_DIAB_IHD.html')


# extras - VIP
VIP <- getVipVn(AA3.opls); #View(VIP)
pvaVn <- apply(AA.3, 2, function(feaVn) cor.test(G1, feaVn)[["p.value"]])
quantVn <- qnorm(1 - pvaVn / 2)
rmsQuantN <- sqrt(mean(quantVn^2))

opar <- par(font = 1, font.axis = 1, font.lab = 2,
            las = 1,
            mar = c(5.1, 4.6, 4.1, 2.1),
            lwd = 2, pch = 16)

plot(pvaVn, VIP,
     col = colorme[3],
     pch = 16,
     xlab = "p-value", ylab = "VIP", xaxs = "i", yaxs = "i")
box(lwd = 2)
curve(qnorm(1 - x / 2) / rmsQuantN, 0, 1, add = TRUE, col = colorme[3], lwd = 3)
abline(h = 1, col = colorme[8])
abline(v = 0.05, col = colorme[8])
##
#Plot with labels/simple barchart?
VIP2 <- data.frame(VIP, pvaVn)
VIP2.filt <- VIP2[VIP2[,2] <= 0.05,]
'sigcormet.opls' <- as.data.frame(rownames(VIP2.filt))
xy <- data.frame(sigcormet.opls,VIP2.filt[,1])
bp <- ggplot(xy, aes(reorder(xy[,1], xy[,2]), xy[,2], fill = xy[,2]))
bp + geom_col() +
  ylab('Variable importance') + 
  xlab('') +
  theme(axis.text.x = element_text(angle = 0, 
                                   hjust = 0.4, 
                                   vjust = 0.4),
        legend.position = 'none') +
  scale_fill_gradient2(low= colorme[2], mid='snow3', high=colorme[3]) +
  coord_flip() +
  Theme.Box.Blank() +
  ggsave(filename = 'Spearmans_pval0.05.pdf', device = 'pdf', width = 9,
         height = 12, units = "in", dpi = 300, limitsize = FALSE)

# ROC curves:
# Plot Y against Ypred
#plot(AA3.opls@suppLs$y, AA3.opls@suppLs$yPreMN)
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}
L <- AA3.opls@suppLs$y
S <- AA3.opls@scoreMN
test <- simple_roc(L,S)
plot(test[,2], test[,1], type = 'l')


#######################
####    CORRPLOT   ####
#######################
library(corrplot)
library(ComplexHeatmap)
#BNB
cor.test(as.numeric(CSI[7,]), as.numeric(CSI[6,]), alternative = c('two.sided', 'greater', 'less'),
           method = c('pearson', 'spearman', 'kendall'), conf.level = 0.95)
#Example
cor <- cor(nmr.hipp,gcms.hipp,method = "spearman",use="complete.obs")
cor.p <- apply(data.matrix(gcms.hipp),2,function(y){apply(data.matrix(nmr.hipp),2,function(x){cor.test(x,y,method="spearman",exact=NULL)$p.value})})
cor.q <- qvalue::qvalue(cor.p, pi0 = 1)$qvalues

# Correlate multiple variables
All_cor= matrix(nrow= ncol(a), ncol= ncol(b)); rownames(All_cor)= colnames(a); colnames(All_cor)= colnames(b)
All_p= matrix(nrow= ncol(a), ncol= ncol(b)); rownames(All_p)= colnames(a);colnames(All_p)= colnames(b)

for (i in 1:ncol(a)){
  for (j in 1:ncol(b)){
    res<-cor.test(a[,i], 
                  b[,j],  method= "spearman")
    All_cor[i,j]= res$estimate
    All_p[i,j]= res$p.value
  }
}


#visualise
#1
Heatmap(corME, name = "module cor",
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.2f", corMEp[i, j]), x, y, gp = gpar(fontsize = 10))
        })
#2
x11();
corrplot(cor, method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = cor.q, insig = "label_sig", pch.col = "black", pch.cex = 1.1)

#template:
#Metabolites <- mergedMEs
#MD_Mut <- network$ar
corME_MD <- cor(Metabolites,MD_Mut,method = "spearman",use="complete.obs") # run spearman correlation between x and y
corME_MDp <- apply(data.matrix(MD_Mut),2,function(y){apply(data.matrix(Metabolites),2,function(x){cor.test(x,y,method="spearman",exact=NULL)$p.value})}) # extract p values
corME_MDq <- qvalue::qvalue(corME_MDp, pi0 = 1)$qvalues # perform BH correction on p values to get q values

# plot correlation as heatmap
library(corrplot)
x11(); 
corrplot(corME_MD, method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = corME_MDq, 
         insig = "label_sig", pch.col = "black", pch.cex = 1.1)


############################
####    Linear Models   ####
############################
library(car)
library(heplots)
library(tidyverse)
library(lm.beta)
library(openxlsx)
library(groupedstats)
# A = response or dependent variable
# B = predictor
# C = covariate or confounder (optional)
A = df[,1] # A is a single column of dataframe 'df'
B = md[,1] # B and C are columns of metadata dataframe 'md'
C = md[,2]

# df should be numeric:
df <- apply(df, 2, as.numeric)

# Run lm:
lm1 <- lm(A ~ B + C)
plot(A~B) +
  abline(lm(A ~ B + C))

# Calculate Effect Sizes:
# std beta => output: the lower the number the greater the effect size
lm.beta(lm1)
# partial eta sq => output: the higher the number the greater the effect size. Known to overestimate
lm_effsize_ci(lm1)
# partial omega sq => output: the higher the number the greater the effect size. Good of small sample sizes
lm_effsize_ci(lm1, effsize = 'omega')

## Consecutive linear models/glm ####
# df should be numeric:
df <- apply(df, 2, as.numeric)

## run glm:
consec_GLM_model <- lapply(colnames(df), function(x) {
  lm(substitute(i ~ B, 
                list(i = as.name(x))), data = as.data.frame(df))})

## Extract outputs from models:
consec_GLM_model_pval <- sapply(consec_GLM_model, function(f) summary(f)$coefficients[,4])
colnames(consec_GLM_model_pval) <- names(df)
# now we traspose it to have one model per row with each p value from variables the same model in a different column
consec_GLM_model_pval <- t(consec_GLM_model_pval)
# to avoid confusion, we add a suffix to the column names
colnames(consec_GLM_model_pval) <- paste(colnames(consec_GLM_model_pval), "pval", sep = "_")

## To extract Estimates coefficients:
consec_GLM_model_coef <- sapply(consec_GLM_model, function(f) summary(f)$coefficients[,1])
colnames(consec_GLM_model_coef) <- names(df)
consec_GLM_model_coef <- t(consec_GLM_model_coef)
colnames(consec_GLM_model_coef) <- paste(colnames(consec_GLM_model_coef), "Est.", sep = "_")

#extract beta standardised coefficients ## NOT ALWAYS WORKING
consec_GLM_model_beta <- sapply(consec_GLM_model, function(f) lm.beta(f)$standardized.coefficients)
colnames(consec_GLM_model_beta) <- names(df)
consec_GLM_model_beta <- t(consec_GLM_model_beta)
colnames(consec_GLM_model_beta) <- paste(colnames(consec_GLM_model_beta), "Beta", sep = "_")

# consec_GLM_model_eta <- sapply(consec_GLM_model, function(f) etasq(f)[,1])
# colnames(consec_GLM_model_eta) <- names(GreenMod1)
# rownames(consec_GLM_model_eta) <- c("Age_P.Eta Sq.", "?")
# consec_GLM_model_eta <- t(consec_GLM_model_eta)
# consec_GLM_model_eta <- consec_GLM_model_eta[,-4]

consec_GLM_model_eta <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f)$partial.etasq))
colnames(consec_GLM_model_eta) <- c("P.Eta Sq.")
rownames(consec_GLM_model_eta) <- names(df)
#consec_GLM_model_eta <- t(consec_GLM_model_eta)

consec_GLM_model_etaLL <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f)$conf.low))
colnames(consec_GLM_model_etaLL) <- c("EtaCIlower")
rownames(consec_GLM_model_etaLL) <- names(df)
#consec_GLM_model_etaLL <- t(consec_GLM_model_etaLL)

consec_GLM_model_etaUL <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f)$conf.high))
colnames(consec_GLM_model_etaUL) <- c("EtaCIupper")
rownames(consec_GLM_model_etaUL) <- names(df)
#consec_GLM_model_etaUL <- t(consec_GLM_model_etaUL)

consec_GLM_model_omega <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f, effsize = 'omega')$partial.omegasq))
colnames(consec_GLM_model_omega) <- c("P.omega Sq.")
rownames(consec_GLM_model_omega) <- colnames(DrBrG60LGP2)
#consec_GLM_model_eta <- t(consec_GLM_model_eta)

consec_GLM_model_omegaLL <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f, effsize = 'omega')$conf.low))
colnames(consec_GLM_model_omegaLL) <- c("omegaCIlower")
rownames(consec_GLM_model_omegaLL) <- colnames(DrBrG60LGP2)
#consec_GLM_model_etaLL <- t(consec_GLM_model_etaLL)

consec_GLM_model_omegaUL <- as.data.frame(sapply(consec_GLM_model, function(f)lm_effsize_ci(f, effsize = 'omega')$conf.high))
colnames(consec_GLM_model_omegaUL) <- c("omegaCIupper")
rownames(consec_GLM_model_omegaUL) <- colnames(DrBrG60LGP2)

consec_GLM_model_effsizeCI <- cbind(consec_GLM_model_etaLL, consec_GLM_model_etaUL, consec_GLM_model_omegaLL, consec_GLM_model_omegaUL)
consec_GLM_model_effsizeCI <- consec_GLM_model_effsizeCI[,order(colnames(consec_GLM_model_effsizeCI))]
consec_GLM_model_effsizeCI <- cbind(consec_GLM_model_eta, consec_GLM_model_omega, consec_GLM_model_effsizeCI)


GLM_output <- cbind(consec_GLM_model_coef, consec_GLM_model_pval)
#GLM_output <- data.frame(GLM_output, consec_GLM_model_beta[,-1])
GLM_output <- data.frame(GLM_output, consec_GLM_model_effsizeCI)

GLM_output <- GLM_output[,order(colnames(GLM_output))]

# V2

consec_GLM_model <- lapply(colnames(df), function(x) {
  lm(substitute(i ~ na.omit(Yvar), 
                list(i = as.name(x))), data = as.data.frame(df))})

# To extract Estimates coefficients:
consec_GLM_model_pval <- sapply(consec_GLM_model, function(f) summary(f)$coefficients[,4])
colnames(consec_GLM_model_pval) <- colnames(df)
consec_GLM_model_pval <- t(consec_GLM_model_pval)
colnames(consec_GLM_model_pval) <- paste(colnames(consec_GLM_model_pval), "pval", sep = "_")

consec_GLM_model_coef <- sapply(consec_GLM_model, function(f) summary(f)$coefficients[,1])
colnames(consec_GLM_model_coef) <- colnames(df)
consec_GLM_model_coef <- t(consec_GLM_model_coef)
colnames(consec_GLM_model_coef) <- paste(colnames(consec_GLM_model_coef), "Est.", sep = "_")

#Extract effect sizes as eta sq and omega sq:
consec_GLM_model_eta <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f)$eta.sq.partial))
colnames(consec_GLM_model_eta) <- c("P.Eta Sq.")
rownames(consec_GLM_model_eta) <- colnames(df)
#consec_GLM_model_eta <- t(consec_GLM_model_eta)

consec_GLM_model_etaLL <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f)$conf.low))
colnames(consec_GLM_model_etaLL) <- c("CIlower")
rownames(consec_GLM_model_etaLL) <- colnames(df)
#consec_GLM_model_etaLL <- t(consec_GLM_model_etaLL)

consec_GLM_model_etaUL <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f)$conf.high))
colnames(consec_GLM_model_etaUL) <- c("CIupper")
rownames(consec_GLM_model_etaUL) <- colnames(df)
#consec_GLM_model_etaUL <- t(consec_GLM_model_etaUL)

consec_GLM_model_omega <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f, effsize = 'omega')$omega.sq.partial))
colnames(consec_GLM_model_omega) <- c("P.omega Sq.")
rownames(consec_GLM_model_omega) <- colnames(df)
#consec_GLM_model_eta <- t(consec_GLM_model_eta)

consec_GLM_model_omegaLL <- as.data.frame(sapply(consec_GLM_model, function(f) lm_effsize_ci(f, effsize = 'omega')$conf.low))
colnames(consec_GLM_model_omegaLL) <- c("omegaCIlower")
rownames(consec_GLM_model_omegaLL) <- colnames(df)
#consec_GLM_model_etaLL <- t(consec_GLM_model_etaLL)

consec_GLM_model_omegaUL <- as.data.frame(sapply(consec_GLM_model, function(f)lm_effsize_ci(f, effsize = 'omega')$conf.high))
colnames(consec_GLM_model_omegaUL) <- c("omegaCIupper")
rownames(consec_GLM_model_omegaUL) <- colnames(df)

consec_GLM_model_effsizeCI <- cbind(consec_GLM_model_etaLL, consec_GLM_model_etaUL, consec_GLM_model_omegaLL, consec_GLM_model_omegaUL)
consec_GLM_model_effsizeCI <- consec_GLM_model_effsizeCI[,order(colnames(consec_GLM_model_effsizeCI))]
consec_GLM_model_effsizeCI <- cbind(consec_GLM_model_eta, consec_GLM_model_omega, consec_GLM_model_effsizeCI)


consec_GLM_model.out <- cbind(consec_GLM_model_coef, consec_GLM_model_pval)
#consec_GLM_model.out <- data.frame(consec_GLM_model.out, consec_GLM_model_beta[,-1])
consec_GLM_model.out <- data.frame(consec_GLM_model.out, consec_GLM_model_effsizeCI)

consec_GLM_model.out <- consec_GLM_model.out[,order(colnames(consec_GLM_model.out))]
#consec_GLM_model.out <- consec_GLM_model.out[,order('Mod.CDKN2A_P.omega.Sq.')]

#######################
####    PIECHART   ####
#######################
slices <- Superpathways$`No. of Compounds`
lbls <- Superpathways$`SUPER  PATHWAY`
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, slices) # add percents to labels
lbls <- paste(lbls, sep="") # ad % to labels
x11()
pie(slices,labels = lbls, col = brewer.pal(8, "Pastel1"),
    main="Pie Chart of 674 Compounds and their Super Pathways")

####################
#### RIVER PLOT ####
####################
#K. Riverplot for Module correspondance (12>7>14)
# Starting with a matrix of values/correlations
#library(networkD3)
#library(dplyr)
# cell line (full) to cell line (subset):
# filter for sig
CellCompcorME.filt <- CellCompcorME
CellCompcorME.filt[which(CellCompcorMEq > 0.05)] <- 0

CellCompcorME.melt <- melt(CellCompcorME.filt) #or just CellCompcorME for full data
CellCompcorME.links <- CellCompcorME.melt
colnames(CellCompcorME.links) <- c("target", "source", "value") #links

CellCompcorME.nodes <- data.frame(
  name=c(as.character(CellCompcorME.links$source), 
         as.character(CellCompcorME.links$target)) %>% unique()) #nodes

CellCompcorME.links$IDsource <- match(CellCompcorME.links$source, CellCompcorME.nodes$name)-1 
CellCompcorME.links$IDtarget <- match(CellCompcorME.links$target, CellCompcorME.nodes$name)-1

# Make the Network
p1 <- sankeyNetwork(Links = CellCompcorME.links, Nodes = CellCompcorME.nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    sinksRight=FALSE)
p1

# cell line specific to cons mod:
CountTbl.1 <- CountTbl
rownames(CountTbl.1) <- CellModules
colnames(CountTbl.1) <- ConsModAnno
CountTbl.links <- melt(CountTbl.1)
colnames(CountTbl.links) <- c("source", "target", "value") #links

CountTbl.nodes <- data.frame(
  name=c(as.character(CountTbl.links$source), 
         as.character(CountTbl.links$target)) %>% unique()) #nodes

CountTbl.links$IDsource <- match(CountTbl.links$source, CountTbl.nodes$name)-1 
CountTbl.links$IDtarget <- match(CountTbl.links$target, CountTbl.nodes$name)-1

# Make the Network
p2 <- sankeyNetwork(Links = CountTbl.links, Nodes = CountTbl.nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    sinksRight=FALSE)
p2

# combine:
# 1. scale correspondances similarly
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
CountTbl.links.scale <- CountTbl.links
CountTbl.links.scale$value <- range01(CountTbl.links$value)
CellCompcorME.links.scale <- CellCompcorME.links
# CellCompcorME.links.scale$value <- range01(CellCompcorME.links$value)
# CellCompcorME.links.scale[which(CellCompcorME.links.scale$value == 0.3499813)] <- 0
# CountTbl.links.scale$value <- scale(CountTbl.links.scale$value, center = 1, scale = median(CountTbl.links.scale$value))
# View((CountTbl.links.scale$value/sum(CountTbl.links.scale$value)*100))

# 2. combine
CountTbl.links.1 <- CountTbl.links.scale[order(colnames(CountTbl.links.scale))]
CellCompcorME.links.1 <- CellCompcorME.links[order(colnames(CellCompcorME.links))]
Corre.links <- data.frame(rbind(CellCompcorME.links.1[-c(1:2)],CountTbl.links.1[-c(1:2)]))
Corre.nodes <- data.frame(
  name=c(as.character(Corre.links$source), 
         as.character(Corre.links$target)) %>% unique())

Corre.links$IDsource <- match(Corre.links$source, Corre.nodes$name)-1 
Corre.links$IDtarget <- match(Corre.links$target, Corre.nodes$name)-1

# colours?
Corre.nodes$group <- as.factor(c("black", "green", "salmon", "lightyellow", "royalblue","brown", "grey60", "darkred", "lightgreen","pink", "turquoise", "grey", 
                                 "turquoise", "green", "brown", "red","blue", "yellow", "grey",
                                 "salmon", "pink", "greenyellow", "red","purple", "black", "yellow", "green","magenta", "turquoise", "brown", "blue","tan", "grey"))
my_color <- 'd3.scaleOrdinal() .domain([cat(paste(shQuote(unique(Corre.nodes$group), type = "cmd), collapse = ", "))] 
.range([paste(unique(Corre.nodes$group), sep = "")]))'

# 3. plot
p3 <- sankeyNetwork(Links = Corre.links, Nodes = Corre.nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    sinksRight=T, fontSize = 11, fontFamily = 'arial',
                    width = 850, height = 500, margin = NULL, nodeWidth = 20,
                    NodeGroup = "group")
p3
library(htmlwidgets)
saveWidget(p3, file=paste0(getwd(), "/Results/ModuleLinkage_SankeyDiagram.html"))
######################
####  tSNE (MLA)  ####
######################
# tSNE is a machine learning technique which can be completely unsupervised 
# and can cluster samples or variables to reduce dimensions like in PCA.
library(M3C)
library(NMF)
library(gplots)
library(ggsci)
library(doParallel)
library(Hmisc)

tsne(CellNorm3_672_t) #variables as rows, samples as cols
pca(CellNorm3_672_t, labels = c(CellLine2_t), dotsize = 4, axistextsize = 12, legendtextsize = 10, scaler = TRUE)
#res <- M3C(CellNorm3_672_t, cores = 1)
trace(tsne, edit = TRUE) # change 'PC1' and 'PC2' to X1 and X2
tsne(CellNorm3_672_t, K = FALSE, labels = c(CellLine2_t), dotsize = 4, axistextsize = 12, perplex = 15, legendtextsize = 14)
# clustering metabolites? Does not work
#res <- M3C(CellNorm3_672, cores = 1)
tsne(CellNorm3_672, K = FALSE, labels = c(CellLine2), dotsize = 4, axistextsize = 12, perplex = 15, legendtextsize = 14) #variables as cols, samples as rows

tsne(t(M123_72.dat), labels = as.character(aim2_72.noQC$Exp), dotsize = 4, axistextsize = 12, perplex = 15, 
     legendtextsize = 14, legendtitle = 'Extraction Method') + 
  scale_colour_manual(values = c(blackpancol[4], blackpancol[2], blackpancol[3]))

###################
####  K-means  #### 
###################
library(factoextra)
library(ggsignif)
CellNorm3_672_t <- t(CellNorm3_672)
CellLine2_t <- t(CellLine2)
kmns <- kmeans(CellNorm3_672_t, centers = 8, iter.max = 10, nstart = 1,
               algorithm = c("Hartigan-Wong"), trace=FALSE)
#fitted(kmns, method = c("centers"))
fviz_nbclust(CellNorm3_672_t, kmeans, method = 'wss') +
  geom_vline(xintercept = 6, linetype = 2)
kmns <- kmeans(CellNorm3_672_t, centers = 6, iter.max = 10, nstart = 25,
               algorithm = c("Hartigan-Wong"), trace=FALSE) #nstart is default 1 usually but is advised to set to 25 or 50 for a more stable result
Meso_kmns <- cbind(CellNorm3_672_t, cluster = kmns$cluster)
head(Meso_kmns)

fviz_cluster(kmns, data = CellNorm3_672_t,
             palette = c(brewer.pal(6, 'Set2')),
             ggtheme = theme_minimal(),
             main = 'K-means Cluster Plot',
             geom = c('point', 'text')
)

# Correlate sample clusters to metadata (Pathology, age, ..)
cent <- t(kmns$centers)
cor.kmns <- cor(cent,Meso_MD3,method = "spearman",use="complete.obs")
cor.kmnsp <- apply(data.matrix(Meso_MD3),2,function(y){apply(data.matrix(cent),2,function(x){cor.test(x,y,method="spearman",exact=NULL)$p.value})})
cor.kmnsq <- qvalue::qvalue(cor.kmnsp)$qvalues

cor.kmns_T <- cor(cent,Meso_MD3$CANCER_STATUS,method = "spearman",use="complete.obs")
cor.kmns_Tp <- apply(data.matrix(Meso_MD3$CANCER_STATUS),2,function(y){apply(data.matrix(cent),2,function(x){cor.test(x,y,method="spearman",exact=NULL)$p.value})})
cor.kmns_Tq <- qvalue::qvalue(cor.kmns_Tp, pi0 = 1)$qvalues

cor.kmns[,1] <- cor.kmns_T
cor.kmnsp[,1] <- cor.kmns_Tp
cor.kmnsq <- qvalue::qvalue(cor.kmnsp)$qvalues #recompute q values for data with Cancer status p vals

cor.kmns <- cor.kmns[,-(68)]
cor.kmnsp <- cor.kmnsp[,-(68)]
cor.kmnsq <- cor.kmnsq[,-(68)]

x11();
corrplot(cor.kmns, method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = cor.kmnsq, insig = "label_sig", pch.col = "black", pch.cex = 1.1) #optimise vis
# Split up the genes and the metadata
x11();
corrplot(cor.kmns[,1:5], method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = cor.kmnsq[,1:5], insig = "label_sig", pch.col = "black", pch.cex = 1.1) #optimise vis
x11();
corrplot(cor.kmns[,6:68], method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = cor.kmnsq[,6:68], insig = "label_sig", pch.col = "black", pch.cex = 1.1) #optimise vis

plot(Meso_MD2$PATHOLOGY, cent[,6]) ### The Pathology is set to numeric and so cannot fully encapsulate the difference between two groups


# What is the biggest cluster? 
clusters <- data.frame(rownames(Meso_kmns), Meso_kmns[,51])
names(clusters)[1] <- 'BIOCHEMICAL'
Clusters_info <- merge(clusters, Variables_info_672, on = 'BIOCHEMICAL')
Cluster_1 <- Clusters_info[Clusters_info[,2] == 1,] #7
Cluster_2 <- Clusters_info[Clusters_info[,2] == 2,] #13
Cluster_3 <- Clusters_info[Clusters_info[,2] == 3,] #1
Cluster_4 <- Clusters_info[Clusters_info[,2] == 4,] #81
Cluster_5 <- Clusters_info[Clusters_info[,2] == 5,] #2
Cluster_6 <- Clusters_info[Clusters_info[,2] == 6,] #568
# => There seems to be little to no trend concerning the Superpathways of the clustered variables
#    This suggests the correlations could provide more information as to why these were clustered together

# Correlate Kmeans to WGCNA clusters
KMvsWG <- cor(cent, MME_net1, method = "spearman",use="complete.obs")
x11();
corrplot(KMvsWG, method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full') #optimise vis

# Kmeans visualised against a Y variable:
ClusterB.values.melt <- lapply(ClusterB.values, function(x) melt(data.frame(t(x[,1:24]), G1), id.vars = 'G1')) # melt data

ClusterB.values.melt.sum <- lapply(ClusterB.values.melt, function(x) ddply(x, c("G1", "variable"), summarise, # summarise to get mean and se
                                                                           N = length(`value`),
                                                                           mean = mean(`value`),
                                                                           sd = sd(`value`),
                                                                           se = sd/sqrt(N)))
clusterplot2000 <- function(clu,x){ # input is a list (clu) and list element number (x), these elements are as summary dfs with mean and se
  ggplot(clu[[x]], aes(`G1`, `mean`, group = `variable`)) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), color = alpha(colorme2[x], 0.5), size = 1, width = 0.1) +
    geom_line(size = 1, color = alpha(colorme2[x], 0.5)) +
    geom_point(size = 4, stroke = 1.4, shape = 21, fill = 'white', color = alpha(colorme2[x], 1)) +
    xlab('Timepoint (Hours)') +
    ylab('Normalised intensity') +
    ggtitle(paste('Cell Cluster',x)) +
    scale_x_continuous(breaks = c(G1)) +
    Theme.Box.Blank() +
    theme(axis.text.x = element_text(size = 11, color = 'black', angle = 0, hjust = 0.5), 
          axis.text.y = element_text(size = 11, color = 'black'),
          plot.title = element_text(size = 15, hjust = 0.5)) +
    theme(legend.position = "none")
}
cc2 <- clusterplot2000(ClusterB.values.melt.sum,2)


###################
####   WGCNA   ####
###################
library(WGCNA)
options(stringsAsFactors = FALSE)
dataType = "Cmpds"
subset = "F" # F (full), T, I, N
subsetIndex = NA
fileName = paste("MESO_WGCNA_",dataType,sep = "","_",subset)
network = list()
network$fileName <- fileName

# cluster sample to check outlier
an = CellNorm3_672
ar = Meso_MD3[,-(68), drop = FALSE]

sampleTree = hclust(dist(CellNorm3_672), method = "average")
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 0.1,
     cex.axis = 0.5, cex.main = 1)
height = 0.022 # change accordingly
abline(h = height, col = "red")
clust = cutreeStatic(sampleTree, cutHeight = height, minSize = 1)
table(clust)

keepSamples = (clust==1)
nSamples = nrow(CellNorm3_672)

# Script heavily adapted to ensure it runs
dan = Meso_MD3
rownames(dan) = rownames(CellNorm3_672);

# Sample trait dendrogram
# visualise clinical data with dendrogram
# Re-cluster samples
sampleTree2 = hclust(dist(CellNorm3_672), method = "average")
# Convert traits to a color representation: white means low, red means high, grey means missing entry
traitColors = numbers2colors(as.numeric(Meso_MD3$CANCER_STATUS), signed = T); # signed set to True
# Plot the sample dendrogram and the colors underneath.
plotDendroAndColors(sampleTree2, traitColors,
                    groupLabels = "Sample type",
                    main = "Sample dendrogram and trait heatmap")
# Choose soft threshold power beta
# Choose a set of soft-thresholding powers
powers = c(c(1:10), seq(from = 12, to=26, by=2))
sft = pickSoftThreshold(CellNorm3_672, powerVector = powers, 
                        verbose = 5,networkType = "signed",
                        corOptions = list(method="spearman",use="complete.obs"))

# Plot result
#par(mfrow = c(1,2)); # subplot
x11()
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
sft$powerEstimate # = 18, meaning the soft power threshold should be set to 18


# Step by step 
# 1. Prepare matrix for construction
# Calculate adjacency and similarity
softPower = 18
adj = adjacency(CellNorm3_672, power = softPower,type = "signed",corOptions = "method='spearman',use='complete.obs'")

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
#dev.off()

# 3. Merge modules whose expressions are very similar
# Calculate eigengenes
MEList = moduleEigengenes(CellNorm3_672, colors = dynamicColors)
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
merge = mergeCloseModules(CellNorm3_672, dynamicColors, cutHeight = MEDissThres, verbose = 3)
# The merged module colors
mergedColors = merge$colors;
# Eigengenes of the new merged modules:
mergedMEs = merge$newMEs;

# Visulise the effect of merging
plotDendroAndColors(featTree, cbind(dynamicColors, mergedColors),
                    c("Dynamic Tree Cut", "Merged dynamic"),
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)

# Rename to moduleColors
moduleColors = mergedColors

# get MEs
MEs0 = moduleEigengenes(CellNorm3_672, network$moduleColors,excludeGrey=T)$eigengenes
MEs = orderMEs(MEs0)
grey <- CellNorm3_672[,moduleColors=="grey"]
ME <- cbind(MEs,grey)

network$adj <- adj
network$TOM <- TOM
network$softPower <- softPower
network$minModuleSize <- minModuleSize
network$moduleColors <- moduleColors
network$MEs <- MEs
network$ME <- ME
network$featTree <- featTree
network$ar <- ar

# Name the list file according to the type
save(network,file = paste(network$fileName,".RData"))

# Visualise dendrogram and module assignemnt
plotDendroAndColors(network$featTree, network$moduleColors, "Dynamic Tree Cut",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05,
                    main = "Metabolite dendrogram and module colors")

# Load data
dataType = "Cmpds"
subset = "F" # F (full), T, I, N
fileName = paste("MESO_WGCNA_",dataType,sep = "","_",subset)
load(paste(fileName,".RData"))
network$fileName

# Metabolites in each module
View(network$moduleColors)
var.col <- as.data.frame(network$moduleColors)
var.WGCNA <- as.data.frame(rownames(network$adj))
var.modcol <- cbind(var.WGCNA, var.col)

var.col.unmerged <- as.data.frame(dynamicColors)
var.modcol.unmerged <- cbind(var.WGCNA, var.col.unmerged)

# Group metabolites by module membership (merged modules) 
modcol.lvls <- levels(var.modcol$`network$moduleColors`)
lapply(var.modcol, levels)
Black1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[1],] #67
Brown1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[2],] #164
Darkred1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[3],] #119
Green1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[4],] #56
Grey1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[5],] #57
Grey60_1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[6],] #46
Lightgreen1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[7],] #15
Lightyellow1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[8],] #14
Pink1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[9],] #32
RoyalBlue1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[10],] #11
Salmon1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[11],] #24
Turquoise1 <- var.modcol[var.modcol$`network$moduleColors` == modcol.lvls[12],] #67

names(Black1)[1] <- 'BIOCHEMICAL';names(Brown1)[1] <- 'BIOCHEMICAL';names(Darkred1)[1] <- 'BIOCHEMICAL';
names(Green1)[1] <- 'BIOCHEMICAL';names(Grey1)[1] <- 'BIOCHEMICAL';names(Grey60_1)[1] <- 'BIOCHEMICAL';
names(Lightgreen1)[1] <- 'BIOCHEMICAL';names(Lightyellow1)[1] <- 'BIOCHEMICAL';names(Pink1)[1] <- 'BIOCHEMICAL';
names(RoyalBlue1)[1] <- 'BIOCHEMICAL';names(Salmon1)[1] <- 'BIOCHEMICAL';names(Turquoise1)[1] <- 'BIOCHEMICAL'

#merge dfs with HMDB IDs
#Variable_IDs_HMDB
Black1 <- merge(Black1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');Brown1 <- merge(Brown1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');
Darkred1 <- merge(Darkred1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');Green1 <- merge(Green1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');
Grey1 <- merge(Grey1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');Grey60_1 <- merge(Grey60_1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');
Lightgreen1 <- merge(Lightgreen1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');Lightyellow1 <- merge(Lightyellow1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');
Pink1 <- merge(Pink1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');RoyalBlue1 <- merge(RoyalBlue1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');
Salmon1 <- merge(Salmon1,Variable_IDs_HMDB, on = 'BIOCHEMICAL');Turquoise1 <- merge(Turquoise1,Variable_IDs_HMDB, on = 'BIOCHEMICAL')


Merged_modules <- list()
Merged_modules$Black <- Black1; Merged_modules$Lightgreen <- Lightgreen1;
Merged_modules$Brown <- Brown1; Merged_modules$Lightyellow <- Lightyellow1;
Merged_modules$Darkred <- Darkred1; Merged_modules$RoyalBlue <- RoyalBlue1;
Merged_modules$Green <- Green1; Merged_modules$Salmon <- Salmon1;
Merged_modules$Grey <- Grey1; Merged_modules$Turquoise <- Turquoise1;
Merged_modules$Grey60 <- Grey60_1; Merged_modules$Pink <- Pink1

for(i in names(Merged_modules)){
  write.csv(Merged_modules[[i]], paste0(i,".csv"))
}
# Perform pathway analysis on each list via MetaboAnalyst #
# EXAMPLE:
mSet<-InitDataObjects("conc", "pathora", FALSE)
cmpd.vec<-c("HMDB0007883","HMDB0007974","0","HMDB0000807","HMDB0001494","HMDB0028691","HMDB0000538","HMDB0001176","HMDB0000082","HMDB0001473","HMDB0011738","HMDB0000759","HMDB0028854","HMDB0001273","HMDB0028907","HMDB0028922","HMDB0028927","HMDB0028929","HMDB0011745","HMDB0000230","HMDB0000833","HMDB0029416","HMDB0028995","HMDB0000263","HMDB0011178","HMDB0001220","HMDB0001066","HMDB0001068","HMDB0000560","HMDB0029068","HMDB0029105","HMDB0000285","HMDB0029125","HMDB0029127","HMDB0029131")
mSet<-Setup.MapData(mSet, cmpd.vec);
mSet<-CrossReferencing(mSet, "hmdb");
mSet<-CreateMappingResultTable(mSet)
mSet<-SetKEGG.PathLib(mSet, "hsa", "current")
mSet<-SetMetabolomeFilter(mSet, F);
mSet<-CalculateOraScore(mSet, "rbc", "hyperg")
mSet<-PlotPathSummary(mSet, "path_view_0_", "png", 72, width=NA)
mSet<-SaveTransformedData(mSet)

# Merge to show superpathways in each module
Black1 <- merge(Black1,Variable_IDs_Super, on = 'BIOCHEMICAL');Brown1 <- merge(Brown1,Variable_IDs_Super, on = 'BIOCHEMICAL');
Darkred1 <- merge(Darkred1,Variable_IDs_Super, on = 'BIOCHEMICAL');Green1 <- merge(Green1,Variable_IDs_Super, on = 'BIOCHEMICAL');
Grey1 <- merge(Grey1,Variable_IDs_Super, on = 'BIOCHEMICAL');Grey60_1 <- merge(Grey60_1,Variable_IDs_Super, on = 'BIOCHEMICAL');
Lightgreen1 <- merge(Lightgreen1,Variable_IDs_Super, on = 'BIOCHEMICAL');Lightyellow1 <- merge(Lightyellow1,Variable_IDs_Super, on = 'BIOCHEMICAL');
Pink1 <- merge(Pink1,Variable_IDs_Super, on = 'BIOCHEMICAL');RoyalBlue1 <- merge(RoyalBlue1,Variable_IDs_Super, on = 'BIOCHEMICAL');
Salmon1 <- merge(Salmon1,Variable_IDs_Super, on = 'BIOCHEMICAL');Turquoise1 <- merge(Turquoise1,Variable_IDs_Super, on = 'BIOCHEMICAL')

Merged_modules2 <- list()
Merged_modules2$Black <- Black1; Merged_modules2$Lightgreen <- Lightgreen1;
Merged_modules2$Brown <- Brown1; Merged_modules2$Lightyellow <- Lightyellow1;
Merged_modules2$Darkred <- Darkred1; Merged_modules2$RoyalBlue <- RoyalBlue1;
Merged_modules2$Green <- Green1; Merged_modules2$Salmon <- Salmon1;
Merged_modules2$Grey <- Grey1; Merged_modules2$Turquoise <- Turquoise1;
Merged_modules2$Grey60 <- Grey60_1; Merged_modules2$Pink <- Pink1

## Describe Modules based on variables clustered and pathway analysis:
# Black - Arginine biosynthesis, Plasmologens, Pyrimidine metabolism
# Brown - Biosynthesis of (unsaturated) fatty acids, glycerophospholipid metabolism
# DarkRed - Aminoacyl-tRNA biosynthesis (AAs), Arginine biosynthesis (Arg in this cluster)
# Green - Ala, Asp, Glu Metabolism, Glutathione metabolism
# Grey - Val, Leu, Iso degradation/metabolism
# Grey60 - Pyruvate metabolism, Glycolysis/Gluconeogenesis
# LightGreen - Glycerophospholipid metabolism, glutamate metabolism, creatine metabolism
# LightYellow - Pyrimidine and Purine Metabolism
# Pink - Sphingolipid metabolism
# RoyalBlue - Monoacylglycerols
# Salmon - Lysophospholipids
# Turquoise - Fatty acid metabolism (acyl carnitine)

# ME ranksum test 
NvT <- which(network$ar$CANCER_STATUS==1)
class<-network$ar$CANCER_STATUS[NvT]
modNames = substring(names(network$MEs), 3)

moduleP<-sapply(1:ncol(network$MEs),function(i){
  wilcox.test(as.numeric(network$MEs[NvT,i]),class,na.action=na.omit,exact=FALSE)$p.value
})
modulePadj<-p.adjust(moduleP,method="BH")
moduleQ <- qvalue::qvalue(moduleP, pi0 = 1)$qvalues
modulekwP<-sapply(1:ncol(network$MEs),function(i){
  kruskal.test(as.numeric(network$MEs[,i])~as.factor(network$ar$CANCER_STATUS),na.action=na.omit)$p.value
})
modulekwPadj<-p.adjust(modulekwP,method="BH")

moduleStat <- data.frame(moduleP = moduleP,modulePadj = modulePadj, moduleQ = moduleQ,modulekwP = modulekwP,modulekwPadj = modulekwPadj)
rownames(moduleStat)<- modNames

network$moduleStat <- moduleStat


# Correlate modules to binary disease state
# Unmerged MEs
ME_net1 <- network$MEs 
T_net1 <- network$ar$CANCER_STATUS
corME_net1 <- cor(ME_net1,T_net1,method = "spearman",use="complete.obs")
corMEp_net1 <- apply(data.matrix(T_net1),2,function(y){apply(data.matrix(ME_net1),2,function(x){cor.test(x,y,method="spearman",exact=NULL)$p.value})})
corMEq_net1 <- qvalue::qvalue(corMEp_net1, pi0 = 1)$qvalues
Heatmap(corME_net1, name = "module cor",
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.4f", corMEp_net1[i, j]), x, y, gp = gpar(fontsize = 10))
        })
x11(); corrplot::corrplot(corME_net1, method = 'color', col = brewer.pal(n = 8, name = "RdBu")) #p.mat = corMEq_net1[,1], 
#insig = "p-value", sig.level = -1))
# Merged MEs
MME_net1 <- mergedMEs
T_net1 <- network$ar$CANCER_STATUS
corME_T <- cor(MME_net1,T_net1,method = "spearman",use="complete.obs")
corMEp_T <- apply(data.matrix(T_net1),2,function(y){apply(data.matrix(MME_net1),2,function(x){cor.test(x,y,method="spearman",exact=NULL)$p.value})})
corMEq_T <- qvalue::qvalue(corMEp_T, pi0 = 1)$qvalues
#Heatmap(corME_merged_net1, name = "module cor",
#        cell_fun = function(j, i, x, y, width, height, fill) {
#          grid.text(sprintf("%.4f", corMEp_merged_net1[i, j]), x, y, gp = gpar(fontsize = 10))
#        })
x11(); corrplot::corrplot(corME_T, method = 'color', col = brewer.pal(n = 8, name = "RdBu")) #p.mat = corMEq_net1[,1], 
#insig = "p-value", sig.level = -1))


# Correlate all covariates
MME_net1 <- mergedMEs
MD_net1 <- network$ar
corME_merged_net1 <- cor(MME_net1,MD_net1,method = "spearman",use="complete.obs")
corMEp_merged_net1 <- apply(data.matrix(MD_net1),2,function(y){apply(data.matrix(MME_net1),2,function(x){cor.test(x,y,method="spearman",exact=NULL)$p.value})})
corMEq_merged_net1 <- qvalue::qvalue(corMEp_merged_net1, pi0 = 1)$qvalues
corME_MD <- corME_merged_net1
corME_MD[,1] <- corME_T
rownames(corME_MD) <- c('Black (Plasmalogen lipids)', 'Green (Ala, Asp and Glu metabolism)', 'Salmon (Lysophospholipids)',
                        'Light Yellow (Pyrimidine and Purine Metabolism)', 'Royal Blue (Monoacylglycerols)', 'Brown (Biosynthesis of fatty acids)',
                        'Grey60 (Glycolysis/Gluconeogenesis)', 'Dark Red (Arginine biosynthesis)', 'Light Green (Glycerophospholipid metabolism)', 
                        'Pink (Sphingolipid metabolism)', 'Turquoise (Fatty acid metabolism)', 'Grey (Val, Leu, Ile metabolism)')
corMEq_MD <- corMEq_merged_net1
corMEq_MD[,1] <- corMEq_T
Heatmap(corME_MD, name = "module cor", width = unit(35, "cm"), height = unit(12, "cm"),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.2f", corMEq_MD[i, j]), x, y, gp = gpar(fontsize = 5))
        })
x11(); 
corrplot(t(corME_MD), method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', mar = c(5,2,2,2))
#### Cancer status and TJP3 not correlating with full data..
# Correlate all covariates
MME_net1 <- mergedMEs
MD_net1 <- network$ar
corME_merged_net1 <- cor(MME_net1,MD_net1,method = "spearman",use="complete.obs")
corMEp_merged_net1 <- apply(data.matrix(MD_net1),2,function(y){apply(data.matrix(MME_net1),2,function(x){cor.test(x,y,method="spearman",exact=NULL)$p.value})})
corMEq_merged_net1 <- qvalue::qvalue(corMEp_merged_net1, pi0 = 1)$qvalues
corME_MD <- corME_merged_net1
corME_MD[,1] <- corME_T

# rownames(corME_MD) <- c('Black (Plasmalogen lipids)', 'Green (Ala, Asp and Glu metabolism)', 'Salmon (Lysophospholipids)',
#                        'Light Yellow (Pyrimidine and Purine Metabolism)', 'Royal Blue (Monoacylglycerols)', 'Brown (Biosynthesis of fatty acids)',
#                       'Grey60 (Glycolysis/Gluconeogenesis)', 'Dark Red (Arginine biosynthesis)', 'Light Green (Glycerophospholipid metabolism)',
#                      'Pink (Sphingolipid metabolism)', 'Turquoise (Fatty acid metabolism)', 'Grey (Val, Leu, Ile metabolism)')
rownames(corME_MD) <- c('Plasmalogen lipids (Black)', 'Ala, Asp and Glu metabolism (Green)', 'Lysophospholipids (Salmon)',
                        'Pyrimidine and Purine Metabolism (Light Yellow)', 'Monoacylglycerols (Royal Blue)', 'Biosynthesis of fatty acids (Brown)',
                        'Glycolysis/Gluconeogenesis (Grey60)', 'Arginine biosynthesis (Dark Red)', 'Glycerophospholipid metabolism (Light Green)',
                        'Sphingolipid metabolism (Pink)', 'Fatty acid metabolism (Turquoise)', 'Val, Leu, Ile metabolism (Grey)')
corMEq_MD <- corMEq_merged_net1
corMEq_MD[,1] <- corMEq_T

Heatmap(corME_MD, name = "module cor", width = unit(35, "cm"), height = unit(12, "cm"), #stopped working! FIX!!
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.2f", corMEq_MD[i, j]), x, y, gp = gpar(fontsize = 5))
        })
#### THIS IS THE COR. PLOT:
library(corrplot)
x11(); 
corrplot(corME_MD, method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = corMEq_MD, insig = "label_sig", pch.col = "black", pch.cex = 1.1)
x11(); 
corrplot(corME_MD[,1:5], method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = corMEq_MD[,1:5], insig = "label_sig", pch.col = "black", pch.cex = 1.1)
x11(); 
corrplot(corME_MD[,6:68], method = 'color', addgrid.col = 'black', tl.col='black', 
         tl.cex = 0.9, is.corr = TRUE, type = 'full', p.mat = corMEq_MD[,6:68], insig = "label_sig", pch.col = "black", pch.cex = 1.1)


# FEAT MEASURES
# Module membership
network$CellNorm <- CellNorm3_672
modNames = substring(names(network$MEs2), 3) 
moduleMembership = as.data.frame(cor(network$CellNorm, network$MEs, use = "p", method = "spearman"));
MMPvalue = as.data.frame(corPvalueStudent(as.matrix(moduleMembership), nSamples))

names(moduleMembership) = paste("MM", modNames, sep="");
names(MMPvalue) = paste("p.MM", modNames, sep="");

# write these info into a file
moduleSignificanceQ = as.vector(moduleP[match(network$moduleColors,modNames)])

# Create the starting data frame
networkOut = data.frame(feature = colnames(network$CellNorm),
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
nCellNorm = ncol(network$CellNorm)
modNames = substring(names(network$MEs), 3)
MMneat <-matrix(NA,nCellNorm)
for (ii in 1:nCellNorm){
  if (network$moduleColors[ii]=="grey"){
    MMneat[ii] <- 1
  } else {
    MMneat[ii] <- network$networkOut[ii,match(paste("MM.",network$moduleColors[ii],sep=""), paste("MM.",network$networkOut$moduleColor[ii],sep=""))]
  }
}
grey <- network$CellNorm[,network$moduleColors=="grey"]
network$MEsG <- cbind(network$MEs,grey)
sig <-matrix(NA,ncol(network$CellNorm))
nME <- ncol(network$MEs)
gStat <-  network$moduleStat[(nME+1):nrow(network$moduleStat),2]
g = 1
for (ii in 1:ncol(network$CellNorm)){
  if (network$moduleColors[ii]=="grey"){
    sig[ii] <- gStat[g]
    g = g+1
  } else {
    sig[ii] <- network$moduleStat[match(network$moduleColors[ii],modNames),2]
  }
}
network$networkOut$moduleSig <- sig
network$networkOut = data.frame(network$networkOut,MMneat)

# SAVE NETWORK
save(network,file = paste(network$fileName,".RData"))
# NETWORK METRICS
adj <- network$adj
Size = dim(adj)[1]
Connectivity = apply(adj, 2, sum)
Density = sum(Connectivity)/(Size * (Size - 1))
Centralization = Size * (max(Connectivity) - mean(Connectivity))/((Size - 1) * (Size - 2))
Heterogeneity = sqrt(Size * sum(Connectivity^2)/sum(Connectivity)^2 - 1)

# VISUALISE NETWORKS
library(igraph)

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

#Visualise full dataset
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
x11()
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


#### Plot single network (module) ####
Tum.dat_506_Sa_all <- Tum.dat.cells_overlap_506[,which(colnames(Tum.dat.cells_overlap_506) %in% rownames(Sa.variables))]
TOM.Salmon.T <- TOMsimilarityFromExpr(Tum.dat_506_Sa_all, power = 9)
TOM.Salmon.T[TOM.Salmon.T > 0.1] = 1
TOM.Salmon.T[TOM.Salmon.T != 1] = 0
network.Sa.T <- graph.adjacency(TOM.Salmon.T)
network.Sa.T <- simplify(network.Sa.T)  # removes self-loops
V(network.Sa.T)$color <- 'salmon'
E(network.Sa.T)$color <- "grey90"
par(mar=c(0,0,0,0))
# remove unconnected nodes
network.Sa.T <- delete.vertices(network.Sa.T, degree(network.Sa.T)==0)
plot(network.Sa.T, layout=layout.fruchterman.reingold(network.Sa.T), edge.arrow.size = 0.2)

# complex plotting w/ igraph
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
weight.community=function(network.Sa.T,weight.within,weight.between){
  el <- get.edgelist(network.Sa.T)
  ew <- apply(el,1,function(i){
    if(V(network.Sa.T)$type[match(i[1],V(network.Sa.T)$name)] == V(network.Sa.T)$type[match(i[2],V(network.Sa.T)$name)]) {
      return (weight.within)
    } else { 
      return (weight.between)
    }
  })
  E(network.Sa.T)$ew <- ew
  return(network.Sa.T)
}
lo <- layout.fruchterman.reingold(network.Sa.T,weight = (E(network.Sa.T)$weight))

plot.igraph(network.Sa.T,layout = lo,
            # VERTEX
            vertex.color=V(network.Sa.T)$color,
            vertex.frame.color=NA,
            vertex.size=V(network.Sa.T)$size,
            # EDGE
            edge.width = (E(network.Sa.T)$weight)^2,
            edge.color = E(network.Sa.T)$color,
            # LABEL
            vertex.label = names(Tum.dat_506_Sa_all),
            vertex.label.family="sans", vertex.label.cex=0.8,
            vertex.label.dist=0.7, vertex.label.degree=-pi/3,
            vertex.label.color="black")
# export for cytoscape
network.Sa.T2 <- set.vertex.attribute(network.Sa.T, "name", value = names(Tum.dat_506_Sa_all))
network.Sa.T.Cy <- createNetworkFromIgraph(network.Sa.T2, title = "SalmonCons_tum_igraph")
#### Module Preservation ####
colorsTum <- moduleTum$moduleColor
setLabels = c("Cell", "Tumour");
multiExpr = list(Cell = list(data = CellNorm3_672_tum), Tumour = list(data = Tum.dat.cells_overlap_506)); # Bring in new data as dataframe 'Tumour'
multiColor = list(Cell = colorsCell);

system.time( {
  mp = modulePreservation(multiExpr, multiColor,
                          referenceNetworks = 1,
                          nPermutations = 1000,
                          randomSeed = 1,
                          quickCor = 0,
                          verbose = 3)
} );

save(mp, file = "modulePreservation.RData");

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

#### Consensus Modules ####
# Choose a set of soft-thresholding powers
powers = c(seq(4,10,by=1), seq(12,20, by=2));
# Initialize a list to hold the results of scale-free analysis
powerTables = vector(mode = "list", length = nSets);
# Call the network topology analysis function for each set in turn
for (set in 1:nSets)
  powerTables[[set]] = list(data = pickSoftThreshold(multiExpr2[[set]]$data, powerVector=powers,
                                                     verbose = 2)[[2]]);
collectGarbage();
# Plot the results:
colors = c("black", "red")
# Will plot these columns of the returned scale free analysis tables
plotCols = c(2,5,6,7)
colNames = c("Scale Free Topology Model Fit", "Mean connectivity", "Median connectivity",
             "Max connectivity");
# Get the minima and maxima of the plotted points
ylim = matrix(NA, nrow = 2, ncol = 4);
for (set in 1:nSets)
{
  for (col in 1:length(plotCols))
  {
    ylim[1, col] = min(ylim[1, col], powerTables[[set]]$data[, plotCols[col]], na.rm = TRUE);
    ylim[2, col] = max(ylim[2, col], powerTables[[set]]$data[, plotCols[col]], na.rm = TRUE);
  }
}
# Plot the quantities in the chosen columns vs. the soft thresholding power
sizeGrWindow(8, 6)
par(mfcol = c(2,2));
par(mar = c(4.2, 4.2 , 2.2, 0.5))
cex1 = 0.7;
for (col in 1:length(plotCols)) for (set in 1:nSets)
{
  if (set==1)
  {
    plot(powerTables[[set]]$data[,1], -sign(powerTables[[set]]$data[,3])*powerTables[[set]]$data[,2],
         xlab="Soft Threshold (power)",ylab=colNames[col],type="n", ylim = ylim[, col],
         main = colNames[col]);
    addGrid();
  }
  if (col==1)
  {
    text(powerTables[[set]]$data[,1], -sign(powerTables[[set]]$data[,3])*powerTables[[set]]$data[,2],
         labels=powers,cex=cex1,col=colors[set]);
  } else
    text(powerTables[[set]]$data[,1], powerTables[[set]]$data[,plotCols[col]],
         labels=powers,cex=cex1,col=colors[set]);
  if (col==1)
  {
    legend("bottomright", legend = setLabels, col = colors, pch = 20) ;
  } else
    legend("topright", legend = setLabels, col = colors, pch = 20) ;
}
dev.off();
# choosing soft thresh = 9
net = blockwiseConsensusModules(
  multiExpr2, power = 9, minModuleSize = 9, deepSplit = 2,
  pamRespectsDendro = FALSE,
  mergeCutHeight = 0.25, numericLabels = TRUE,
  minKMEtoStay = 0,
  saveTOMs = TRUE, verbose = 5)

consMEs = net$multiMEs;
moduleLabels = net$colors;
# Convert the numeric labels to color labels
moduleColors2 = labels2colors(moduleLabels)
consTree = net$dendrograms[[1]];

sizeGrWindow(8,6);
#pdf(file = "Plots/ConsensusDendrogram-auto.pdf", wi = 8, he = 6)
plotDendroAndColors(consTree, moduleColors2,
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05,
                    main = "Consensus gene dendrogram and module colors")
dev.off()

save(consMEs, moduleLabels, moduleColors2, consTree, file = "Cell_Tum_Overlap_Consensus-NetworkConstruction-auto.RData")

# Get adjacency
adj2.C <- adjacency(multiExpr2$Cell$data, power = 9)
adj2.T <- adjacency(multiExpr2$Tumour$data, power = 9)
TOM.C <- TOMsimilarity(adj2.C)
TOM.T <- TOMsimilarity(adj2.T)

# consensus hubs
HubComps.Cons.C <- chooseTopHubInEachModule(multiExpr2$Cell$data, moduleColors2)
HubComps.Cons.T <- chooseTopHubInEachModule(multiExpr2$Tumour$data, moduleColors2)

#### Correspondence Plot ####
CellLabels = mergedLabels2C;
CellColors = mergedColors2C;
CellTree = cpdTree;
CellMEs = orderMEs(netC_MEs, greyName = "ME0");

lnames = load("Cell_Tum_Overlap_Consensus-NetworkConstruction-auto.RData")
lnames
# Isolate the module labels in the order they appear in ordered module eigengenes
CellModuleLabels = substring(names(CellMEs), 3)
consModuleLabels = substring(names(consMEs[[1]]$data), 3)
# Convert the numeric module labels to color labels
CellModules = labels2colors(as.numeric(CellModuleLabels))
consModules = labels2colors(as.numeric(consModuleLabels))
# Numbers of Cell and consensus modules
nCellMods = length(CellModules)
nConsMods = length(consModules)
# Initialize tables of p-values and of the corresponding counts
pTable = matrix(0, nrow = nCellMods, ncol = nConsMods);
CountTbl = matrix(0, nrow = nCellMods, ncol = nConsMods);
# Execute all pairwaise comparisons
for (fmod in 1:nCellMods)
  for (cmod in 1:nConsMods)
  {
    CellMembers = (CellColors == CellModules[fmod]);
    consMembers = (moduleColors == consModules[cmod]);
    pTable[fmod, cmod] = -log10(fisher.test(CellMembers, consMembers, alternative = "greater")$p.value);
    CountTbl[fmod, cmod] = sum(CellColors == CellModules[fmod] & moduleColors ==
                                 consModules[cmod])
  }

# Truncate p values smaller than 10^{-50} to 10^{-50}
pTable[is.infinite(pTable)] = 1.3*max(pTable[is.finite(pTable)]);
pTable[pTable>50 ] = 50 ;
# Marginal counts (really module sizes)
CellModTotals = apply(CountTbl, 1, sum)
consModTotals = apply(CountTbl, 2, sum)
# Actual plotting
sizeGrWindow(10,7 );
#pdf(file = "Plots/ConsensusVsCellModules.pdf", wi = 10, he = 7);
par(mfrow=c(1,1));
par(cex = 1.0);
par(mar=c(8, 10.4, 2.7, 1)+0.3);
# Use function labeled Heatmap to produce the color-coded table with all the trimmings
labeledHeatmap(Matrix = pTable,
               xLabels = paste(" ", consModules),
               yLabels = paste(" ", CellModules),
               colorLabels = TRUE,
               xSymbols = paste("Cons ", consModules, ": ", consModTotals, sep=""),
               ySymbols = paste("Cell ", CellModules, ": ", CellModTotals, sep=""),
               textMatrix = CountTbl,
               colors = greenWhiteRed(100)[50:100],
               main = "Correspondence of Cell set-specific and Cell-Tumour consensus modules",
               cex.text = 1.0, cex.lab = 1.0, setStdMargins = FALSE);
dev.off();

####################
####  HEATMAPS  ####
####################
library(ComplexHeatmap)
library(circlize)
# heatmap colouring
col_fun6 <- colorRamp2(c(-7,0,12), c('indianred4','floralwhite', 'deepskyblue4'))
col_fun6(seq(-3,0,3))

# Annotation
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

col_fun5 <- colorRamp2(c(unique(CNV.info2_chranno$`as.numeric(CNV.info2_unique$Chromosome)`)), c(col_vector[1:17]))
col_fun5(seq(-3,0,3))
row_ha <- rowAnnotation(Chr = CNV.info2_chranno$`as.numeric(CNV.info2_unique$Chromosome)`, col = list(Chr = col_fun5), border = TRUE,
                        show_legend = FALSE, 
                        annotation_legend_param = list(Chr = list(color_bar = "discrete", ncol= 3)))
# Multiple annotations
row_ha1 <- rowAnnotation(WGCNA = var.modcol$`network$moduleColors`, 
                         col = list(WGCNA = c('blue' = 'royalblue', 'grey' = 'grey60', 'turquoise' = 'turquoise'),
                                    Kmeans = c('1' = 'firebrick1',
                                               '2' = 'gold1', '3' = 'springgreen4', 
                                               '4' = 'steelblue2', '5' = 'violet'),
                                    Pathway = c('Amino Acid' = colorme[1], 'Carbohydrate' = colorme[2], 'Cofactors and Vitamins' = colorme[3],
                                                'Energy' = colorme[4], 'Lipid' = 'firebrick2', 'Nucleotide' = colorme[6],
                                                'Peptide' = 'springgreen2', 'Xenobiotics' = colorme[10])), border = TRUE,
                         Kmeans = Clusters_info$ADf_kmns...29.,
                         Pathway = Variables_256_info$SUPER_PATHWAY, show_annotation_name = FALSE)

# heatmap
Heatmap(CNV.info2.plotdf1[order(CNV.info2_unique$Chromosome),], name = "CNVs", 
        width = unit(18, "cm"), height = unit(14, "cm"), col = col_fun4, 
        column_names_gp = gpar(fontsize = 8), column_names_rot = 90, show_column_dend = FALSE, 
        row_names_gp = gpar(fontsize = 10), cluster_rows = FALSE, column_km = 7, row_names_side = 'left', 
        column_title = c('60T', '8T', '70T', '23T', '12T', '33T', '55T/3T/7T/H2052/MSTO/Met5A'), 
        heatmap_legend_param = list(title = 'Copy Number',
                                    legend_height = unit(3, 'cm'),
                                    title_position = 'topleft',
                                    color_bar = "discrete"),
        border = TRUE,
        left_annotation = row_ha
)

library(pheatmap)
library(viridis)
pheatmap(RNAseq.filt5, show_rownames = T, annotation_colors = ann_color, annotation_legend = TRUE, scale = "row",
         cluster_rows = F, color= viridis(30),
         cluster_cols = T, legend = TRUE
)

# With GGPLOT2
ggplot(thecor, aes(Var2, Var1))+
  geom_tile(data=thecor, aes(fill=value), color="white")+
  scale_fill_gradient2(low="blue", high="red", mid="white", 
                       midpoint=0, limit=c(-1,1),name="Correlation\n(Pearson)")+
  theme(axis.text.x = element_text(angle=45, vjust=1, size=11, hjust=1))+
  coord_equal()


########################
####  Volcano plot  ####
########################
library(EnhancedVolcano)
# Data prep
binding <- function(df) {
  data.frame(df[,5],df[,6],df[,7])
} 
HCvMMCvol <- binding(HCvMMC)

colnames(HCvMMCvol) <- c('BIOCHEMICAL', 'FDR', 'effsize'); 
rownames(HCvMMCvol) <- HCvMMCvol$BIOCHEMICAL
# simple volcanoes: ####
EnhancedVolcano(HCvMMCvol,
                lab = HCvMMCvol$BIOCHEMICAL,
                x = 'effsize',
                y = 'FDR',
                xlim = c(-0.55, 0.55),
                ylim = c(0, 15.5),
                xlab = 'Effect size',
                ylab = '-Log(FDR)',
                title = 'HC v MMC',
                subtitle = NULL,
                legendPosition = 'top',
                col = c(colorme[2], "forestgreen", 
                             colorme[1], colorme[3]),
                legendLabels = c("NS", expression(Log[2] ~ FC), 
                                      "FDR", expression(FDR ~ and ~  effectsize)),
                labSize = 3.5,
                labhjust = 0.5,
                labvjust = -1,
                colAlpha = 1,
                pCutoff = 0.01, 
                FCcutoff = 0.2)

#########################
####   Sankey Plot   ####
#########################
# Make the Network
p1 <- sankeyNetwork(Links = CellCompcorME.links, Nodes = CellCompcorME.nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    sinksRight=FALSE)
p1

# cell line specific to cons mod:
CountTbl.1 <- CountTbl
rownames(CountTbl.1) <- CellModules
colnames(CountTbl.1) <- ConsModAnno
CountTbl.links <- melt(CountTbl.1)
colnames(CountTbl.links) <- c("source", "target", "value") #links

CountTbl.nodes <- data.frame(
  name=c(as.character(CountTbl.links$source), 
         as.character(CountTbl.links$target)) %>% unique()) #nodes

CountTbl.links$IDsource <- match(CountTbl.links$source, CountTbl.nodes$name)-1 
CountTbl.links$IDtarget <- match(CountTbl.links$target, CountTbl.nodes$name)-1

p2 <- sankeyNetwork(Links = CountTbl.links, Nodes = CountTbl.nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    sinksRight=FALSE)
p2

# 1. scale correspondences similarly
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
CountTbl.links.scale <- CountTbl.links
CountTbl.links.scale$value <- range01(CountTbl.links$value)
CellCompcorME.links.scale <- CellCompcorME.links

# 2. combine
CountTbl.links.1 <- CountTbl.links.scale[order(colnames(CountTbl.links.scale))]
CellCompcorME.links.1 <- CellCompcorME.links[order(colnames(CellCompcorME.links))]
Corre.links <- data.frame(rbind(CellCompcorME.links.1[-c(1:2)],CountTbl.links.1[-c(1:2)]))
Corre.nodes <- data.frame(
  name=c(as.character(Corre.links$source), 
         as.character(Corre.links$target)) %>% unique())

Corre.links$IDsource <- match(Corre.links$source, Corre.nodes$name)-1 
Corre.links$IDtarget <- match(Corre.links$target, Corre.nodes$name)-1

# colours
Corre.nodes$group <- as.factor(c("black", "green", "salmon", "lightyellow", "royalblue","brown", "grey60", "darkred", "lightgreen","pink", "turquoise", "grey", 
                                 "turquoise", "green", "brown", "red","blue", "yellow", "grey",
                                 "salmon", "pink", "greenyellow", "red","purple", "black", "yellow", "green","magenta", "turquoise", "brown", "blue","tan", "grey"))
my_color <- 'd3.scaleOrdinal() .domain([cat(paste(shQuote(unique(Corre.nodes$group), type = "cmd), collapse = ", "))] 
.range([paste(unique(Corre.nodes$group), sep = "")]))'

# 3. plot
p3 <- sankeyNetwork(Links = Corre.links, Nodes = Corre.nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    sinksRight=T, fontSize = 11, fontFamily = 'arial',
                    width = 850, height = 500, margin = NULL, nodeWidth = 20,
                    NodeGroup = "group")
p3
library(htmlwidgets)
saveWidget(p3, file=paste0(getwd(), "/Results/ModuleLinkage_SankeyDiagram.html"))

#####################
####    OLIGO    ####
#####################
x1 <- c(2.8, 4)
y1 <- c(-1, 1)
MAplot(rawData[, 1:3], pairs = TRUE, ylim =  y1, xlim = x1)
#Preproc
ppData <- rma(rawData)
#show(ppData)
exprs(ppData)
ppData.exprs.t <- t(exprs(ppData)) %>% as.data.frame()
ppData.exprs <- exprs(ppData) %>% as.data.frame()
ppData.exprs.1 <- data.frame("AffyID" = rownames(ppData.exprs), ppData.exprs)

# Convert to Gene Symbols:
ppData.exprs.t.IDs <- names(ppData.exprs.t)
write.csv(ppData.exprs.t.IDs, "/Users/michaelolanipekun/Desktop/LUAD_SAFFRON_MO_Desktop/Affy_IDs.csv")
Affy.IDs.conv <- read.csv("/Users/michaelolanipekun/Desktop/LUAD_SAFFRON_MO_Desktop/Affy_IDs_converted.csv", check.names = F)
colnames(Affy.IDs.conv)[1] <- c("AffyID")
ppData.exprs.geneIDs_1 <- merge(Affy.IDs.conv, ppData.exprs.1, by = "AffyID") # has some redundancies, multiple genes per Affy ID
ppData.exprs.geneIDs_2 <- data.frame(ppData.exprs.geneIDs_1[,-c(1:4)]) #%>% `rownames<-`(ppData.exprs.geneIDs_1$To)
# extra: add chromosomal location to gene symbols:
ids <- c(Affy.IDs.conv_2$To[1:3])
ensembl <- useMart("ensembl")
ensembl <- useDataset("hsapiens_gene_ensembl",mart=ensembl)
getBM(attributes = c('chromosome/'),
      filters = c('hgnc_symbol'),
      values = list(ids),
      mart = ensembl)

# Test with real gene symbols:
ids_all <- c(Affy.IDs.conv_2$To)
ids_all_chr <- getBM(attributes = c('hgnc_symbol','chromosome_name'),
                     filters = c('hgnc_symbol'),
                     values = list(ids_all),
                     mart = ensembl)
ids_all_chr2 <- ids_all_chr
ids_all_chr2 <- ids_all_chr2[which(!grepl("CHR", ids_all_chr$chromosome_name, fixed = TRUE)),]

# Remove duplicates:
Affy.IDs.conv_2 <- Affy.IDs.conv %>% distinct(To, .keep_all = TRUE)
ppData.exprs.geneIDs_3 <- merge(Affy.IDs.conv_2, ppData.exprs.1, by = "AffyID") # has some redundancies, multiple genes per Affy ID
ppData.exprs.geneIDs_4 <- data.frame(ppData.exprs.geneIDs_3[,-c(1:4)]) %>% `rownames<-`(ppData.exprs.geneIDs_3$To)

#######################
####   CIBERSORT   ####
#######################
ciber_T <- CIBERSORT(sig_matrix = lm22, 
                     mixture_file = Tum_89_exprDat,
                     perm = 1000,
                     QN = TRUE,
                     absolute = FALSE)
ciber_T2 <- cbind(rownames(ciber_T), ciber_T)
colnames(ciber_T2)[1] <- "ID" 
res_T <- cell_bar_plot(input = ciber_T2, title = 'CIBERSORT fraction')
# manually plot stacked barchart
ciber_T_2 <- ciber_T[,1:22]/rowSums(ciber_T[,1:22])
ciber_T_2.melt <- melt(ciber_T_2)
barcodes <- ciber_T_2.melt$Var1
cell_types <- ciber_T_2.melt$Var2
percent <- ciber_T_2.melt$value

#alt cols
pdf("/Users/michaelolanipekun/Desktop/LUAD_SAFFRON_MO_Desktop/Results/Cibersort_T_bar_alt.pdf", width = 15, height = 15)
ggplot(as.data.frame(ciber_T_2.melt), mapping = aes(x = barcodes, y = percent, fill =  cell_types)) + 
  geom_bar(position= "stack", stat = "identity") + 
  scale_fill_manual(values = c(IOBR::palette3)) +
  theme(plot.title=element_text(size=rel(2),hjust=0.5),
        axis.text.x= element_text(face="plain",angle=90,hjust = 1,color="black"),
        axis.text.y= element_text(face="plain",angle= 30,hjust = 1,color="black")) +
  theme(legend.title = element_blank(),
        legend.position= "right",
        legend.direction= "vertical",
        legend.justification=c(.5,.5),
        legend.box="horizontal",
        legend.box.just="top")
dev.off()

####################
####    LIMMA   ####
####################
Tum_80_exprDat.3 <- Tum_80_exprDat.2
Norm_80_exprDat.3 <- Norm_80_exprDat.2
colnames(Tum_80_exprDat.3) <- paste0(colnames(Tum_80_exprDat.3),"_T") 
colnames(Norm_80_exprDat.3) <- paste0(colnames(Norm_80_exprDat.3),"_N") 
Data.expr <- cbind(Tum_80_exprDat.3, Norm_80_exprDat.3)
des <- matrix(c(rep(0, 80), rep(1, 80))) # 0 = T, 1 = N

fit <- lmFit(Data.expr, des)
fit <- eBayes(fit)
topTable(fit, adjust = "BH")

des2 <- matrix(cbind(
  c(rep(0, 80), rep(1, 80)),
  c(rep(1, 80), rep(0, 80))), ncol = 2)
colnames(des2) = c("T", "N")
rownames(des2) = colnames(Data.expr)
fit2 <- lmFit(Data.expr, des2)
cont.matrix <- makeContrasts(T-N, levels = c("T", "N"))
fit2.2 <- contrasts.fit(fit2, cont.matrix)
fit2.2 <- eBayes(fit2.2)
top25 <- topTable(fit2.2, adjust = "BH", number = 25)
summary(decideTests(fit2.2))
# T - N
# Down    7384
# NotSig  7528
# Up      5483

# Plot:
volcanoplot(fit2.2, highlight = 10, names = rownames(fit2.2$coefficients))
boxplot(as.numeric(Data.expr["GOLM1",]) ~ des, ylab = "GOLM1 Gene Expression", xlab = "")

####################
####    GSEA    ####
####################
pathwaysHall <- msigdbr("Homo sapiens", category="H") # hallmark pathways only
pathways.h <- split(as.character(pathwaysHall$gene_symbol), pathwaysHall$gs_name)
set.seed(2) #random number generator
fgseaRes.h <- fgsea(pathways.h, ranks, minSize = 15, maxSize = 500)
head(fgseaRes.h[order(pval), ])
plotEnrichment(pathways.h[["HALLMARK_E2F_TARGETS"]],
               ranks) + labs(title="E2F TARGETS") # negative enrichment score
plotEnrichment(pathways.h[["HALLMARK_TNFA_SIGNALING_VIA_NFKB"]],
               ranks) + labs(title="HALLMARK_TNFA_SIGNALING_VIA_NFKB") #positive enrichment score
topPathways.h <- c(fgseaRes.h[ES > 0][head(order(pval), n = 10), pathway], 
                   rev(fgseaRes.h[ES < 0][head(order(pval), n = 10), pathway]))
plotGseaTable(pathways.h[topPathways.h], ranks, fgseaRes.h, 
              gseaParam = 0.5)

# for multiple comparisons:
res.AD <- lapply(topTable_all.AD.2, function(x){
  x %>% 
    dplyr::select(`Name`, `T`) %>%
    group_by(Name)
})

ranks.AD <- lapply(res.AD, deframe)

fgseaRes.AD <- lapply(ranks.AD, function(x){
  fgsea(pathways, x, minSize = 15, maxSize = 500)
})

## Split into UP and DOWN:
topPathways.AD_all_Up <- lapply(fgseaRes.AD, function(x){
  x[ES > 0][head(order(pval), n=10), pathway]})
topPathways.AD_all_Down <- lapply(fgseaRes.AD, function(x){
  x[ES < 0][head(order(pval), n=10), pathway]})

topPathways.AD_all <- list()
for(i in 1:15){
  topPathways.AD_all[[i]] <- c(topPathways.AD_all_Up[[i]], rev(topPathways.AD_all_Down[[i]]))
}
names(topPathways.AD_all) <- names(topPathways.AD_all_Up)
plotGseaLIST <- function(x){plotGseaTable(pathways[topPathways.AD_all[[x]]], ranks.AD[[x]], fgseaRes.AD[[x]], gseaParam=0.5)}
#plotGseaLIST(1)
####################
####  MixOmics  ####
####################
#library(mixOmics)
Cell_Tum_list <- list(Cell = CellNorm_tum_overlap_506.2, Tumour = multiExpr2$Tumour$data)
Y1 <- Meso_tum_MD_CNV2.1$NF2
Cell_Tum.diablo <- block.splsda(Cell_Tum_list, Y1)
mixOmics::plotIndiv(Cell_Tum.diablo, group = Y1, legend = TRUE)
plotVar(Cell_Tum.diablo, var.names = c(F, F), legend = TRUE, pch = c(16,16),
        overlap = TRUE, cex = c(4,4), col = c(colorme[3], 'royalblue4'), cutoff = 0.5)
plotDiablo(Cell_Tum.diablo, ncomp = 1)
cimDiablo(Cell_Tum.diablo, legend.position = NULL)
circosPlot(Cell_Tum.diablo, cutoff = 0.9)
plotLoadings(Cell_Tum.diablo, comp = 1, contrib = 'max')

####################
####    MISC    ####
####################
# Apply function to multiple objects
lapply(mget(ls(pattern = '_s_1')), function(x) {
  rownames(x) <- x[,1]
  x <- x[,-1]
})

# Changes plot layout to 2x2
opar <- par()
par(mcfd = c(2,2))

# Reset graphics window
dev.off()

#Editing source code
trace(name_of_function, edit = T)

# add new line
cat("hi\nmy\nname\nis\nmike")
##################
#### Set Down ####
##################
# Save workspace environment
save.image("C:/Users/mo316/Documents/1. Meso PhD/5. Scripts/R/work-directory/R_environment.RData")

