#Library:
library(car)
library(heplots)
library(tidyverse)
library(lm.beta)
library(openxlsx)
library(groupedstats)
library(ggplot2)

# Make linear model:
model.glm <- lapply(colnames(Dat), function(x) {
  lm(substitute(i ~ na.omit(YVar), 
                list(i = as.name(x))), data = as.data.frame(Dat))})

# To extract Estimates coefficients:
  # P value
model.glm_pval <- sapply(model.glm, function(f) summary(f)$coefficients[,4])
colnames(model.glm_pval) <- colnames(Dat)
model.glm_pval <- t(model.glm_pval)
colnames(model.glm_pval) <- paste(colnames(model.glm_pval), "pval", sep = "_")
  
  # beta coef 
model.glm_coef <- sapply(model.glm, function(f) summary(f)$coefficients[,1])
colnames(model.glm_coef) <- colnames(Dat)
model.glm_coef <- t(model.glm_coef)
colnames(model.glm_coef) <- paste(colnames(model.glm_coef), "Est.", sep = "_")

  # partial eta sq and conf. intervals (effect size)
model.glm_eta <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f)$eta.sq.partial))
colnames(model.glm_eta) <- c("Mod.YVar_P.Eta Sq.")
rownames(model.glm_eta) <- colnames(Dat)

model.glm_etaLL <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f)$conf.low))
colnames(model.glm_etaLL) <- c("Mod.YVar_CIlower")
rownames(model.glm_etaLL) <- colnames(Dat)

model.glm_etaUL <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f)$conf.high))
colnames(model.glm_etaUL) <- c("Mod.YVar_CIupper")
rownames(model.glm_etaUL) <- colnames(Dat)

  # partial omega sq and conf. intervals (effect size)
model.glm_omega <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f, effsize = 'omega')$omega.sq.partial))
colnames(model.glm_omega) <- c("Mod.YVar_P.omega Sq.")
rownames(model.glm_omega) <- colnames(Dat)

model.glm_omegaLL <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f, effsize = 'omega')$conf.low))
colnames(model.glm_omegaLL) <- c("Mod.YVar_omegaCIlower")
rownames(model.glm_omegaLL) <- colnames(Dat)

model.glm_omegaUL <- as.data.frame(sapply(model.glm, function(f)lm_effsize_ci(f, effsize = 'omega')$conf.high))
colnames(model.glm_omegaUL) <- c("Mod.YVar_omegaCIupper")
rownames(model.glm_omegaUL) <- colnames(Dat)

# Arrange outputs in table
model.glm_effsizeCI <- cbind(model.glm_etaLL, model.glm_etaUL, model.glm_omegaLL, model.glm_omegaUL)
model.glm_effsizeCI <- model.glm_effsizeCI[,order(colnames(model.glm_effsizeCI))]
model.glm_effsizeCI <- cbind(model.glm_eta, model.glm_omega, model.glm_effsizeCI)

model.glm.out <- cbind(model.glm_coef, model.glm_pval)
model.glm.out <- data.frame(model.glm.out, model.glm_effsizeCI)
model.glm.out <- model.glm.out[,order(colnames(model.glm.out))]

# Filter for P value significance
model.glm.out.sig <- model.glm.out[which(model.glm.out$na.omit.RNAseq.filt4.1.YVar._pval <= 0.05),]

# Visualise with waterfall plot
glm.gg1 <- ggplot(data = as.data.frame(model.glm.out.sig), 
                  aes(reorder(rownames(model.glm.out.sig), model.glm.out.sig[,6]), model.glm.out.sig[,6]))
glm.gg1 + 
  geom_bar(stat = 'identity', fill = "#9986A5", color = "black") + 
  ylab('Effect Size (Omega squared)') + 
  xlab('') + 
  Theme.Box.Blank()  +
  theme(axis.text.x = element_text(size = 10, color = 'black', angle = 0, hjust = 0.4), axis.text.y = element_text(size = 8, color = 'black')) +
  theme(legend.position = "none") +
  coord_flip()