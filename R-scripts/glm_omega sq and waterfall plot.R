#### Consecutive Generalised Linear Models for Multivariate data ####
#Library:
library(car)
library(heplots)
library(tidyverse)
library(lm.beta)
library(openxlsx)
library(groupedstats)
library(ggplot2)

# General formula for consectutive glm (example):
variables_names <- names(A)[2:100]
consec_GLM_model <- lapply(variables_names, function(x) {
  lm(substitute(i ~ B + C + D, list(i = as.name(x))), data = A)}) # B = Y variable, C and D are covariates

# Make linear model (actual):
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
model.glm_eta <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f)$estimate))
colnames(model.glm_eta) <- c("Mod.YVar_P.Eta Sq.")
rownames(model.glm_eta) <- colnames(Dat)

model.glm_etaLL <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f)$conf.low))
colnames(model.glm_etaLL) <- c("Mod.YVar_CIlower")
rownames(model.glm_etaLL) <- colnames(Dat)

model.glm_etaUL <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f)$conf.high))
colnames(model.glm_etaUL) <- c("Mod.YVar_CIupper")
rownames(model.glm_etaUL) <- colnames(Dat)

  # partial omega sq and conf. intervals (effect size)
model.glm_omega <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f, effsize = 'omega')$estimate))
colnames(model.glm_omega) <- c("Mod.YVar_P.omega Sq.")
rownames(model.glm_omega) <- colnames(Dat)

model.glm_omegaLL <- as.data.frame(sapply(model.glm, function(f) lm_effsize_ci(f, effsize = 'omega')$conf.low))
colnames(model.glm_omegaLL) <- c("Mod.YVar_omegaCIlower")
rownames(model.glm_omegaLL) <- colnames(Dat)

model.glm_omegaUL <- as.data.frame(sapply(model.glm, function(f)lm_effsize_ci(f, effsize = 'omega')$conf.high))
colnames(model.glm_omegaUL) <- c("Mod.YVar_omegaCIupper")
rownames(model.glm_omegaUL) <- colnames(Dat)

# Arrange outputs in table
model.glm1.out <- data.frame('Est' = model.glm1_coef[,2], 'P_val' = model.glm1_pval[,2],
                                  'eta_sq' = model.glm1_eta[,1], 
                                  'eta_sq_UL' = model.glm1_etaUL[,1], 'eta_sq_LL' = model.glm1_etaLL[,1], 
                                  'omega_sq' = model.glm1_omega[,1], 
                                  'omega_sq_UL' = model.glm1_omegaUL[,1], 'omega_sq_LL' = model.glm1_omegaLL[,1])

# Filter for P value significance
model.glm.out.sig <- model.glm.out[which(model.glm.out[,2] <= 0.05),] # P value should be in column 2

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
