
shh <- suppressPackageStartupMessages
shh(library(dplyr))

shh(library(stargazer))
shh(library(lme4))

shh(library(glue))

shh(library(tictoc))
shh(library(parallel))

tic("Runtime")

memoize <- function(name,call){
   if(name %in% list.files("Cache")){
      cat(paste0("\x1b[33mUsing cache for ",name,"\x1b[0m\n"))
      readRDS(paste0("Cache/",name))
   } else {
      cat(paste0("\x1b[32mRunning and caching ",name,"\x1b[0m\n"))
      res <- eval(call)
      saveRDS(res,paste0("Cache/",name))
      res
   }
}

tic("Reading data")
dat <- readRDS("data.rds")
partial <- filter(dat,year >= 1946)
toc()

# ================================================
# CONSTANTS ======================================
# ================================================

DEPENDENT_VARIABLE <- "c2_onset"

INDEP_ONE <- "lfree_fair_elections"
INDEP_TWO <- "lhorizontal_constraint_narrow"

BASE_CONTROLS <- c("llnpop200", "llnGDPPerCapita200", "decay_c_term_long")
NEW_CONTROLS <- c("ethfrac", "lmtnest", "anyNbConflict")

RANDOM_EFFECTS <- c("(1|gwno)")

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

formulae <- lapply(list(
   c(INDEP_ONE,BASE_CONTROLS),
   c(INDEP_ONE,BASE_CONTROLS,NEW_CONTROLS),
   c(INDEP_TWO,BASE_CONTROLS),
   c(INDEP_TWO,BASE_CONTROLS,NEW_CONTROLS)
   ), modelOnset)

tic("Fitting models")

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

tic("Stargazer")
stargazer(models_partial_base, models_partial_re, models_full_base, type = "html") %>%
   writeLines("/tmp/tab.html")

toc()

toc()
