
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
)

model <- function(variables,data){
   formula <- reformulate(c(variables, controls), "c2_onset")
   glm(formula,family=binomial("logit"),data=data)
}

polity <- c("x_polity","x_polity_sq")

# ========================================================
# Article tab. 3
models <- lapply(
   list(
      c(polity),
      c("lfree_fair_elections",polity),
      c("lhorizontal_constraint_narrow",polity),
      c("lfree_fair_elections","lfree_fair_elections_sq"),
      c("lhorizontal_constraint_narrow","lhorizontal_constraint_narrow_sq"),
      c("lfree_fair_elections*lhorizontal_constraint_narrow")
      #c("lhorizontal_constraint_narrow_sq",
      #  "lfree_fair_elections_sq",
      #  "lfree_fair_elections*lhorizontal_constraint_narrow")),
      ),
   data = data,
   model
)
for(modelnumber in 1:length(models)){
   fname <- paste0("Cache/t3_model_",modelnumber,".rds")
   saveRDS(models[[modelnumber]],fname)
}

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
      "6"
      #"7"
   ),
   digits = 3)

writeLines(stripenv(tbl),"Out/table_3.tex")

# ========================================================
# Table 3 col. 7 for appendix A-2

app <- model(c("lhorizontal_constraint_narrow_sq", "lfree_fair_elections_sq",
        "lfree_fair_elections*lhorizontal_constraint_narrow"), data = data)
saveRDS(app,"Cache/t3_model_7.rds")

tbl <- clusteredTexreg(list(app),
   custom.coef.map = VARIABLE_NAMES,
   caption = "",
   stars = c(0.01,0.05,0.1),
   custom.model.names = c(
      "1"),digits = 3)

writeLines(stripenv(tbl),"Out/table_A_2.tex")

# ========================================================
# Table 3 with polity controls (appendix) 

models <- lapply(
   list(
      c(polity),
      c("lfree_fair_elections",polity),
      c("lhorizontal_constraint_narrow",polity),
      c("lfree_fair_elections","lfree_fair_elections_sq",polity),
      c("lhorizontal_constraint_narrow","lhorizontal_constraint_narrow_sq",polity),
      c("lfree_fair_elections*lhorizontal_constraint_narrow",polity),
      c(#"lhorizontal_constraint_narrow",
        "lhorizontal_constraint_narrow_sq",
        #"lfree_fair_elections",
        "lfree_fair_elections_sq",
        "lfree_fair_elections*lhorizontal_constraint_narrow",
        polity)
   ),
   data = data,
   model
)
for(modelnumber in 1:length(models)){
   fname <- paste0("Cache/t3pol_model_",modelnumber,".rds")
   saveRDS(models[[modelnumber]],fname)
}

saveRDS(models[[6]],"Cache/inter.rds")
saveRDS(models[[length(models)]],"Cache/polyn.rds")

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

writeLines(stripenv(tbl),"Out/table_3_appendix.tex")
