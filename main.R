library(stargazer)
library(dplyr)
library(lme4)
library(parallel)
library(glue)

# ================================================

if(!"prepped_data.rds" %in% list.files("Cache")){
   system("Rscript replication.R")
}

data <- readRDS("Cache/prepped_data.rds")

COUNTRY_RANDOM_EFFECTS<-c("(1|gwno)")
YEAR_RANDOM_EFFECTS<-c("(1|year)")

CONTROLS <- c(
   "llnpop200",
   "llnGDPPerCapita200",
   "decay_c_term_short",
   "ethfrac",
   "lmtnest",
   "anyNbConflict"
)

formulae <- lapply(c("lfree_fair_elections","lhorizontal_constraint_narrow"),
   function(predictor){
      reformulate(c(predictor,CONTROLS),"c2_onset")
   })

# ================================================
# Models
memoize <- function(call,file){
   if(!file.exists(file)){
      res <- eval(call)
      saveRDS(res,file)
      res

   } else {
      readRDS(file)

   }
}


logitModel <- function(data){
   lapply(formulae, 
      function(f){
         glm(f,
             family = binomial("logit"),
             data)
      }
   )
}

reModel <- function(rterm){
   mclapply(formulae, 
      function(f){
         f <- update.formula(f,as.formula(glue("~.+{rterm}"))) 
         glmer(f,
               family = binomial("logit"),
               control = glmerControl(optimizer ="Nelder_Mead"),
               filter(data,year >=1946))
      }, mc.cores = detectCores() - 1

   )
}

# ================================================
# Definition

partial_regular <- call("logitModel",filter(data,year >= 1946)) %>%
   memoize("Cache/partial_regular.rds")

full_regular <- call("logitModel",data) %>%
   memoize("Cache/full_regular.rds")

country_re <- call("reModel",COUNTRY_RANDOM_EFFECTS) %>%
   memoize("Cache/country_random_effects.rds")

year_re <- call("reModel",YEAR_RANDOM_EFFECTS) %>%
   memoize("Cache/year_random_effects.rds")

either_re <- call("reModel",
   paste(COUNTRY_RANDOM_EFFECTS,YEAR_RANDOM_EFFECTS,sep ="+")) %>%
   memoize("Cache/either_random_effects.rds")

# ================================================
# Printout

plt <- capture.output(stargazer(list(partial_regular,full_regular,country_re,year_re,either_re),
   type = "html",

   dep.var.labels ="Conflict onset",
   covariate.labels=c(
   "Vertical constraints (lagged)",
   "Horizontal constraints (lagged)",
   "ln Population (lagged)",
   "ln GDP per cap. (lagged)",
   "Conflict decay (5 year halflife)",
   "Ethnic fractionalization",
   "Mountainous terrain",
   "Neighbouring conflict"),  

   column.labels=c("Logistic",
                   "1900-2016 Logistic",
                   "Country random effects",
                   "Year random effects",
                   "Combined random effects"),
   column.separate=c(2,2,2,2,2),
   model.names = FALSE,

   keep.stat = c("AIC","ll","n")

   ))

writeLines(plt,"/tmp/tab.html")

