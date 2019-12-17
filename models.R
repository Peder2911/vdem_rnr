
shh <- suppressPackageStartupMessages
shh(library(dplyr))

shh(library(stargazer))
shh(library(lme4))

shh(library(glue))

shh(library(tictoc))
shh(library(parallel))

shh(library(yaml))

tic("Runtime")
tic("Reading data")
dat <- readRDS("data.rds")
partial <- filter(dat,year >= 1946)
toc()

# ================================================
# This script trains 12 models with varying specs,
# four for each of the following groups:
#
# 1) Partial data (1946-2016), regular models
# 2) Partial data (1946-2016), year-fixed effects models 
# 3) Full data (1900-2016), regular models 
#
# The model objects are written to cache for further 
# processing (eg. writeout with stargazer).
#

# ================================================
# CONSTANTS ======================================
# ================================================

# The constants are defined in a configuration file.
# this makes it possible to re-use this script
# with several different variable configurations.
# 
# Using the same script for the different configurations 
# ensures that nothing else than what one intends
# to vary, varies between the specifications. 

# Configs written so far:

# tab1_overview.yaml: Reproduction of results and addition of
# new control variables. 

# tab1_overview_major.yaml: Same as the previous,
# only without regarding minor conflicts in any
# of the variables.

a <- commandArgs(trailingOnly = TRUE)
if("--config" %in% a){
   fname <- a[which(a == "--config")+1]
} else {
   fname <- "Config/tab1_overview.yaml"
}

config <- yaml.load_file(fname)

DEPENDENT_VARIABLE <- config$dependent_variable

INDEP_ONE <- config$indep_one 
INDEP_TWO <- config$indep_two 

BASE_CONTROLS <- config$base_controls 
NEW_CONTROLS <- config$new_controls 

RANDOM_EFFECTS <-  config$random_effects 

# ================================================
# REPRODUCTION ===================================
# ================================================

makeFormula <- function(lhs, rhs){
   glue("{lhs} ~ {glue_collapse(rhs,' + ')}") %>%
      as.formula()
}

modelOnset <- function(rhs){
   makeFormula(DEPENDENT_VARIABLE,rhs)
}

# ================================================
# These are the four "variants" in each group:
# There are two different variables of interest,
# indep. one and indep. two. In addition, two 
# sets of control variables are tested.
#

formulae <- lapply(list(
   c(INDEP_ONE,BASE_CONTROLS),
   c(INDEP_ONE,BASE_CONTROLS,NEW_CONTROLS),
   c(INDEP_TWO,BASE_CONTROLS),
   c(INDEP_TWO,BASE_CONTROLS,NEW_CONTROLS)
   ), modelOnset)

tic("Fitting models")

# ================================================
# Here different modelling procedures are applied
# to the formulae:
#

models_partial_base <- formulae %>%
   lapply(function(f) {
      glm(f,data = partial, family = binomial("logit"))
   })
saveRDS(models_partial_base,"Cache/models_partial_base.rds")

models_partial_re <- formulae %>%
   lapply(function(f){
      f %>%
         deparse() %>%
         glue_collapse() %>%
         paste0(" + ", RANDOM_EFFECTS) %>%
         as.formula()
   }) %>%
   mclapply(function(f){
      glmer(f, 
         data = partial, family = binomial("logit"),
         control = glmerControl(optimizer = "Nelder_Mead"))
   }, mc.cores = detectCores() - 1)
saveRDS(models_partial_re,"Cache/models_partial_re.rds")

models_full_base <- formulae %>%
   lapply(function(f) {
      glm(f,data = dat, family = binomial("logit"))
   })

saveRDS(models_full_base,"Cache/models_full_base.rds")
toc()

toc()
