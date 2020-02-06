
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

data <- readRDS("Cache/prepped_data.rds") %>%
   filter(year >= 1946)
# This list also orders the variables in the tables.
VARIABLE_NAMES <- yaml.load_file("vnames.yaml")

source("functions.R")

controls <- c(
   "llnpop200",
   "llnGDPPerCapita200",
   "timesince",
   "timesince_sq",
   "timesince_cb",
   "ethfrac",
   "lmtnest",
   "nbConflict",
   "x_polity",
   "x_polity_sq"
)

model <- function(variables,data){
   formula <- reformulate(c(variables, controls), "c2_onset")
   glm(formula,family=binomial("logit"),data=data)
}

models <- lapply(
   list(
      c(),
      c("lfree_fair_elections"),
      c("lhorizontal_constraint_narrow"),
      c("lfree_fair_elections","lfree_fair_elections_sq"),
      c("lhorizontal_constraint_narrow","lhorizontal_constraint_narrow_sq"),
      c("lfree_fair_elections*lhorizontal_constraint_narrow")
   ),
   data = data,
   model
)

tbl <- clusteredTexreg(models,
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   stars = c(0.01,0.05,0.1),
   custom.model.names = c(
      "X",
      "X",
      "X",
      "X",
      "X",
      "X"
   ),
   digits = 3)

writeLines(stripenv(tbl),"Out/table_3.tex")
