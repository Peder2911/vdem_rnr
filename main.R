
shh <- suppressPackageStartupMessages
shh(library(stargazer))
shh(library(dplyr))
shh(library(lme4))
shh(library(parallel))
shh(library(glue))
shh(library(splines))
shh(library(texreg))

# ================================================

# This script runs scripts that make tables and
# ROC plots for a paper.
#
# TODO: write documentation
#

# ================================================
# Run "replication.R" to produce data.

if(!"prepped_data.rds" %in% list.files("Cache")){
   source("./preparation.R")
}

data <- readRDS("Cache/prepped_data.rds")
partial_data <- filter(data,year >= 1946)

# ================================================

# This list also orders the variables in the tables.
VARIABLE_NAMES <- list(
   lfree_fair_elections = "Vertical Constraints",
   lhorizontal_constraint_narrow = "Horizontal Constraints",

   llnpop200 = "ln Population (lagged)",
   llnGDPPerCapita200 = "ln GDP Per Capita (lagged)",

   ethfrac = "Ethnic fractionalization",
   lmtnest = "Mountainous terrain",

   nbConflict = "Neighbouring conflict", 
   major_nbConflict = "Neighbouring conflict (Major)", 

   decay_c_term_short = "Conflict decay",
   major_decay_c_term_short = "Conflict decay (Major)",

   timesince = "Time since conflict",
   `I(timesince^2)` = "Time since conflict squared",
   `I(timesince^3)`  = "Time since conflict cubed",

   `(Intercept)` = "(Intercept)"
   )

# ================================================
# Functions 

# This function "memoizes" (caches) the result of a call to a file.
# This saves time on repeated calls.
memoize <- function(call,file){
   if(!file.exists(file)){
      res <- eval(call)
      saveRDS(res,file)
      res

   } else {
      readRDS(file)

   }
}

model <- function(data,tolerance = 2, depvar = "onset",timectrl="decay",re = "none",major = FALSE){
   # This function outputs a list of two models, one for each predictor.
   # The model can be customized with various parameters, like different time controls.

   # Created to make it easier to handle the large number of different permutations of models.

   maj <<- ifelse(major,"major_","")

   # this part creates the formula to model


   predictors <- c("lfree_fair_elections","lhorizontal_constraint_narrow")

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
      polynomials = c("{maj}timesince","I({maj}timesince^2)","I({maj}timesince^3)"),
      splines = "bs({maj}timesince,knots = c(1,4,7))"
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
         args$control <- glmerControl(optimizer = "Nelder_Mead")
      }
      
      do.call(ifelse(re == "none",glm, glmer), args)
   })
}

# ================================================
# TABLE 1

partial_regular <- call("model",partial_data) %>%
   memoize("Cache/regular.rds")

full_regular <- call("model",data) %>%
   memoize("Cache/full_regular.rds")

country_re <- call("model",partial_data,re = "country") %>%
   memoize("Cache/country_random_effects.rds")

year_re <- call("model",partial_data,re = "time") %>%
   memoize("Cache/year_random_effects.rds")

either_re <- call("model", partial_data, re = "both") %>%
   memoize("Cache/either_random_effects.rds")

table_1 <- htmlreg(c(partial_regular,country_re,year_re,either_re,full_regular),
   custom.model.names = c(
      "Logistic A",
      "Logistic B",
      "Country RE A",
      "Country RE B",
      "Year RE A",
      "Year RE B",
      "Combined RE A",
      "Combined RE B",
      "Full Logistic A",
      "Full Logistic B"
   ),
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   digits = 3)
writeLines(table_1, "/tmp/tab.html")

# ================================================
# TABLE 2 (Time controls)

polynomials <- call("model",partial_data,timectrl = "polynomials") %>%
   memoize("Cache/regular_polynomials.rds")

splines <- call("model",partial_data,timectrl = "splines") %>%
   memoize("Cache/splines.rds")

table_1 <- htmlreg(c(polynomials,splines),
   #custom.coef.map = VARIABLE_NAMES,
   caption = "",
   digits = 3)
writeLines(table_1, "/tmp/tab2.html")

# ================================================
# Major conflict models

major_partial_regular <- call("model",partial_data,major=TRUE) %>%
   memoize("Cache/major_regular.rds")

major_full_regular <- call("model",data,major=TRUE) %>%
   memoize("Cache/major_full_regular.rds")

major_country_re <- call("model",partial_data,re = "country",major=TRUE) %>%
   memoize("Cache/major_country_random_effects.rds")

major_year_re <- call("model",partial_data,re = "time",major=TRUE) %>%
   memoize("Cache/major_year_random_effects.rds")

major_either_re <- call("model", partial_data, re = "both",major=TRUE) %>%
   memoize("Cache/major_either_random_effects.rds")

table_1 <- htmlreg(c(major_partial_regular,major_country_re,major_year_re,major_either_re,major_full_regular),
   custom.model.names = c(
      "Logistic A",
      "Logistic B",
      "Country RE A",
      "Country RE B",
      "Year RE A",
      "Year RE B",
      "Combined RE A",
      "Combined RE B",
      "Full Logistic A",
      "Full Logistic B"
   ),
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   digits = 3)
writeLines(table_1, "/tmp/tab3.html")
