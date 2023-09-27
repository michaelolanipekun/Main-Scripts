library(ropls)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

x <- opls(dat, g, pre = 1, ort = 1, logL = FALSE, perm = 1000, scale = 'none')

plot.opls <- function(x, g){
  if (ncol(x@scoreMN) > 1){
    a <- x@scoreMN[,1] #PC1
    b <- x@scoreMN[,2] #PC2
    ab <- data.frame(a,b,g)
    ggplot(data = ab, mapping = aes (a, b)) +
      geom_point(data = ab, aes(a, b, fill = g), size = 3, color = 'black', shape = 21) +
      labs(x = paste0('t1 ', '(', x@modelDF$R2X[1]*100, '%', ')', # add text annotations of model params
                      "\n", 'R2Y: ', sprintf(x@summaryDF$R2Y, fmt = '%#.3f'), '     ',
                      'Q2Y: ', sprintf(x@summaryDF$Q2, fmt = '%#.3f'), '     ',
                      'pR2Y = ', sprintf(x@summaryDF$pR2Y, fmt = '%#.3f'), '     ',
                      'pQ2 = ', sprintf(x@summaryDF$pQ2, fmt = '%#.3f')),
           y = paste0('t2 ', '(', x@modelDF$R2X[2]*100, '%', ')'),
           fill = '', shape = '') +
      stat_ellipse(type = "norm", color = 'grey') +
      geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
      Theme.PCA.Blank() } else{
        a <- x@scoreMN[,1] #PC1
        b <- x@orthoScoreMN[,1] #Ort1
        ab <- data.frame(a,b,g)
        ggplot(data = ab, mapping = aes (a, b)) +
          geom_point(data = ab, aes(a, b, fill = g), size = 3, color = 'black', shape = 21) +
          labs(x = paste0('t1 ', '(', x@modelDF$R2X[1]*100, '%', ')', # add text annotations of model params
                          "\n", 'R2Y: ', sprintf(x@summaryDF$R2Y, fmt = '%#.3f'), '     ',
                          'Q2Y: ', sprintf(x@summaryDF$Q2, fmt = '%#.3f'), '     ',
                          'pR2Y = ', sprintf(x@summaryDF$pR2Y, fmt = '%#.3f'), '     ',
                          'pQ2 = ', sprintf(x@summaryDF$pQ2, fmt = '%#.3f')),
               y = paste0('to1 ', '(', x@modelDF$R2X[2]*100, '%', ')'),
               fill = '', shape = '') +
          stat_ellipse(type = "norm", color = 'grey') +
          geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
          Theme.PCA.Blank() }
}

plot.opls.weights <- function(x, col){
  if (ncol(x@weightStarMN) > 1){
    m <- x@weightStarMN[,1] #PC1
    n <- x@weightStarMN[,2] #PC2
    mn <- data.frame(rownames(x@weightStarMN),m,n)
    ggplot(data = mn, mapping = aes (m, n, label = mn[,1])) + 
      labs(x = "w*c1", y = "w*c2") +
      geom_point(color = col, size = 2) +
      geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
      Theme.PCA.Blank() +
      stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.1)
  } else {
    m <- x@weightStarMN[,1] #PC1
    n <- x@orthoWeightMN[,1] #PC2
    mn <- data.frame(rownames(x@weightStarMN),m,n)
    ggplot(data = mn, mapping = aes (m, n, label = mn[,1])) + 
      labs(x = "w*c1", y = "w*c2") +
      geom_point(color = col, size = 2) +
      geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
      Theme.PCA.Blank() +
      stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.1)
  }
}

# optional color changing (continuous):
library(paletteer)
library(scico)

plot.opls(x, g) + scale_fill_paletteer_c("scico::bilbao")