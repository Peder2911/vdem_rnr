
shh <- suppressPackageStartupMessages
shh(library(stargazer))
shh(library(dplyr))
shh(library(lme4))
shh(library(parallel))
shh(library(glue))
shh(library(splines))
shh(library(texreg))
shh(library(lmtest))
shh(library(sandwich))
shh(library(yaml))

# ================================================
# This script makes the "main" table for the paper,
# as well as two axuillary tables that show model
# robustness.

# The tables are made using a casing function
# (model). This is done to reduce code repetition, and also
# makes it possible to memoize (cache) the models for
# repeated runs, making it a lot easier to do
# iterative devlopment. 

# Cached output is stored in Cache/

# The input data (and "supplemental" data) is (of course) 
# not included in this repository, and must be downloaded
# separately. To ensure that you have the correct files,
# hash them with md5 and compare with the "manifest" files.

source("functions.R")

# ================================================
# Run "replication.R" to produce data.

if(!"prepped_data.rds" %in% list.files("Cache")){
   cat("\x1b[33mRunning prep. script\x1b[0m\n")
   source("scripts/preparation.R")
} else {
   cat("\x1b[35mUsing cached prepared data\x1b[0m\n")
}

data <- readRDS("Cache/prepped_data.rds")
partial_data <- filter(data,year >= 1946)

# ================================================

# This list also orders the variables in the tables.
VARIABLE_NAMES <- yaml.load_file("vnames.yaml")

# ================================================
# Functions 

# This function "memoizes" (caches) the result of a call to a file.
# This saves time on repeated calls.
memoize <- function(call,file){
   if(!file.exists(file)){
      cat(glue("\x1b[33mEvaluating model 4 {file}\x1b[0m\n\n"))
      res <- eval(call)
      saveRDS(res,file)
      res

   } else {
      cat(glue("\x1b[35mUsing cache 4 {file}\x1b[0m\n\n"))
      readRDS(file)

   }
}

model <- function(data,
   tolerance = 2, # c{n}_onset as dependent variable
   timectrl="decay", # Choice of time control (spline /polyn. /decay)
   re = "none", # Random effects?
   major = FALSE, # Major conflicts only?
   predictor = "vdem" # vdem or polity as explanatory variable 
   ){

   # This function outputs a list of two models, one for each predictor.
   # The model can be customized with various parameters, like different time controls.

   # Created to make it easier to handle the large number of different permutations of models.

   maj <<- ifelse(major,"major_","")

   # this part creates the formula to model


   predictors <- switch(predictor,
      vdem = c("lfree_fair_elections","lhorizontal_constraint_narrow"),
      polity = "polity"
   )
   #predictors <- c("lfree_fair_elections","lhorizontal_constraint_narrow")

   dependentVariable <- glue("{maj}c{tolerance}_onset")

   controls <- c(
      "llnpop200",
      "llnGDPPerCapita200",
      "ethfrac",
      "lmtnest",
      "{maj}nbConflict"
   ) %>% sapply(glue)

   time <- switch(timectrl,
      decay = "{maj}decay_c_term_short",
      polynomials = c("{maj}timesince","{maj}timesince_sq","{maj}timesince_cb"),
      poly_norm = c("{maj}timesince_norm","{maj}timesince_sq_norm","{maj}timesince_cb_norm"),
      splines = "bs({maj}timesince,knots = c(1,4,7))",
      ceiling = c("timesince_tr","timesince_tr_sq","timesince_tr_cb")
      ) %>% sapply(glue)

   effects <- c("(1|gwno)","(1|year)")
   whichEffects <- switch(re,
      none = c(FALSE,FALSE),
      country = c(TRUE,FALSE),
      time = c(FALSE,TRUE),
      both = c(TRUE,TRUE)
   )
   effects <- effects[whichEffects]

   independentVariables <- c(controls,time,effects)

   formulae <- sapply(predictors,function(predictor){
      reformulate(c(predictor, independentVariables), dependentVariable)
   })

   lapply(formulae, function(formula){
      args <- list(
         formula = formula,
         family = binomial("logit"),
         data = data
      )

      if(re != "none"){
         args$control <- glmerControl(
            optimizer = "Nelder_Mead",
            optCtrl=list(maxfun=100000)
            )
      }
      
      do.call(ifelse(re == "none",glm, glmer), args)
   })
}

# ================================================
# TABLE 1

partial_regular <- call("model",partial_data,timectrl="polynomials") %>%
   memoize("Cache/regular.rds")

full_regular <- call("model",data,timectrl="polynomials") %>%
   memoize("Cache/full_regular.rds")

country_re <- call("model",partial_data,re = "country",timectrl="poly_norm") %>%
   memoize("Cache/country_random_effects.rds")

#year_re <- call("model",partial_data,re = "time") %>%
#   memoize("Cache/year_random_effects.rds")

#either_re <- call("model", partial_data, re = "both") %>%
#   memoize("Cache/either_random_effects.rds")

table_1 <- clusteredTexreg(c(partial_regular,country_re,full_regular),
   custom.model.names = c(
      "Log. A",
      "Log. B",
      "Ctry. RE A",
      "Ctry. RE B",
      "Full A",
      "Full B"
   ),
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   stars = c(0.01,0.05,0.1),
   digits = 3)

writeLines(stripenv(table_1), "Out/table_1.tex")

# ================================================
# Show table w. normalized n-since 

partial_regular_norm <- call("model",partial_data,timectrl="poly_norm") %>%
   memoize("Cache/regular_norm.rds")

full_regular_norm <- call("model",data,timectrl="poly_norm") %>%
   memoize("Cache/full_regular_norm.rds")

norm_test <- clusteredTexreg(c(partial_regular_norm,full_regular_norm),
   custom.model.names = c(
      "Norm A",
      "Norm B",
      "Full norm A",
      "Full norm B"
   ),
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   stars = c(0.01,0.05,0.1),
   digits = 3)

writeLines(stripenv(norm_test), "Out/normalized.tex")

# ================================================
# TABLE 2 (Time controls)

decay <- call("model",partial_data) %>%
   memoize("Cache/decay.rds")

splines <- call("model",partial_data,timectrl = "splines") %>%
   memoize("Cache/splines.rds")

ceiling <- call("model",partial_data,timectrl = "ceiling") %>%
   memoize("Cache/ceiling.rds")

timetable <- clusteredTexreg(c(decay,splines,ceiling),
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   stars = c(0.01,0.05,0.1),
   custom.model.names = c(
      "Decay A",
      "Decay B",
      "Spline time A",
      "Spline time B",
      "Ceil. time A",
      "Ceil. time B"
   ),
   digits = 3)
writeLines(stripenv(timetable), "Out/timetable.tex")

# ================================================
# Major conflict models

major_partial_regular <- call("model",partial_data,major=TRUE,timectrl="polynomials") %>%
   memoize("Cache/major_regular.rds")

major_full_regular <- call("model",data,major=TRUE,timectrl="polynomials") %>%
   memoize("Cache/major_full_regular.rds")

major_country_re <- call("model",partial_data,re = "country",major=TRUE,timectrl="poly_norm") %>%
   memoize("Cache/major_country_random_effects.rds")

#major_year_re <- call("model",partial_data,re = "time",major=TRUE) %>%
   #memoize("Cache/major_year_random_effects.rds")

#major_either_re <- call("model", partial_data, re = "both",major=TRUE) %>%
   #memoize("Cache/major_either_random_effects.rds")

majortable <- clusteredTexreg(c(major_partial_regular,major_country_re,major_full_regular),
   custom.model.names = c(
      "Log. A",
      "Log. B",
      "Ctry. RE A",
      "Ctry. RE B",
      "Full A",
      "Full B"
   ),
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   stars = c(0.01,0.05,0.1),
   digits = 3)
writeLines(stripenv(majortable), "Out/majortable.tex")

# ================================================
# Polity (for ROC plot)

polity <- call("model", partial_data, predictor = "polity",timectrl = "polynomials") %>%
   memoize("Cache/polity.rds")

# ================================================
# Peace-year-skip robustness 

noskip <- call("model",partial_data,tolerance=0,timectrl="polynomials") %>%
   memoize("Cache/noskip.rds")
fourskip <- call("model",partial_data,tolerance=4,timectrl="polynomials") %>%
   memoize("Cache/fourskip.rds")
sevenskip <- call("model",partial_data,tolerance=7,timectrl="polynomials") %>%
   memoize("Cache/sevenskip.rds")
tenskip <- call("model",partial_data,tolerance=10,timectrl="polynomials") %>%
   memoize("Cache/tenskip.rds")

skiptable <- clusteredTexreg(c(noskip,fourskip,sevenskip,tenskip),
   custom.model.names = c(
      "Skip 0 A",
      "Skip 0 B",
      "Skip 4 A",
      "Skip 4 B",
      "Skip 7 A",
      "Skip 7 B",
      "Skip 10 A",
      "Skip 10 B"
   ),
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   stars = c(0.01,0.05,0.1),
   digits = 3)
writeLines(stripenv(skiptable),"Out/skiptable.tex")

