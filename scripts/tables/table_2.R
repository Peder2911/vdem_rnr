
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
   "nbConflict"
   #"x_polity",
   #"x_polity_sq"
)

special_controls <- c(
   "lgrGDPPerCapita200",
   "llnGDPcap_oilrent",
   "ltimeindep"
   )

model <- function(variables,data){
   formula <- reformulate(c(variables, controls), "c2_onset")
   glm(formula,family=binomial("logit"),data=data)
}

yeardummies <- paste("dec",c("00",as.character(seq(40,90,10))),sep="_")
regiondummies <- paste("reg",c("southam","ssafrica","seasia","mena"),sep="_")

models <- lapply(
   list(
      c("lfree_fair_elections",special_controls),
      c("lhorizontal_constraint_narrow",special_controls),
      c("lfree_fair_elections",yeardummies,regiondummies),
      c("lhorizontal_constraint_narrow",yeardummies,regiondummies),
      c("lfree_fair_elections","ff_dem","ff_aut"),
      c("lhorizontal_constraint_narrow","hc_dem","hc_aut"),
      c("lfree_fair_elections","lhorizontal_constraint_narrow")
   ),
   data = data,
   model
)

tbl <- clusteredTexreg(models,
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   stars = c(0.01,0.05,0.1),
   custom.model.names = c(
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7"
   ),
   digits = 3)

writeLines(stripenv(tbl),"Out/table_2.tex")
