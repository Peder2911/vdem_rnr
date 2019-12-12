
library(ggplot2)
library(dplyr)
library(ggdark)
library(parallel)

dat <- readRDS("data.rds")
scalevalues <- 1:50

source("functions.R")

# ================================================
# Script for testing the effect of different 
# decay specifications (half life of decay function)  
# ================================================

# ================================================
# Constants 

DEPENDENT_VARIABLE <- "c2_onset"

INDEP_ONE <- "lfree_fair_elections"
INDEP_TWO <- "lhorizontal_constraint_narrow"

BASE_CONTROLS <- c("llnpop200", "llnGDPPerCapita200", "decay_c2")
NEW_CONTROLS <- c("ethfrac", "lmtnest", "anyNbConflict")

RANDOM_EFFECTS <- c("(1|gwno)")

# ================================================
# Models

cl <- makeForkCluster(detectCores()-1)
results <- parSapply(cl, scalevalues, function(alpha){
   dat <- dat %>%
      group_by(gwno) %>%
      mutate(
         decay = makeDecay(c_term, halflife(alpha))) %>%
      ungroup()
   mod <- glm(c2_onset ~ lhorizontal_constraint_narrow + llnpop200 + llnGDPPerCapita200  + decay, dat,
      family = binomial("logit"))
   saveRDS(mod,paste0("/tmp/m",alpha,".rds"))
   AIC(mod)
})

# ================================================
# Blit 
data <- data.frame(halflife = scalevalues, AIC = results)

alpha_aic <- ggplot(data,aes(x = halflife, y = AIC)) + 
   geom_path() + 
   dark_mode()
ggsave("/tmp/aic.pdf",alpha_aic, device = "pdf", height = 5, width = 5)

# ================================================
# Alphacurves 
#alphacurves <- lapply(1:100,function(x) {
   #data.frame(base = x,alpha = scalevalues)}) %>% 
   #do.call(rbind, .)
#
#alphacurves$ind <- alphacurves$alpha^(-alphacurves$base/alphacurves$alpha)
#alphacurves <- arrange(alphacurves,alpha,base)
#
#alphacurves_plot <- ggplot(alphacurves, aes(x=base,y=ind,color = as.factor(alpha))) + 
   #geom_path() +
   #dark_mode()
#ggsave("/tmp/curves.pdf",alphacurves_plot, device = "pdf", height = 5, width = 5)
