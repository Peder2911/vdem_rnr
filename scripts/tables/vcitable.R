
shh <- suppressPackageStartupMessages
shh(library(timelib))
shh(library(texreg))
shh(library(parallel))
shh(library(yaml))

source("functions.R")

data <- readRDS("Cache/prepped_data.rds")
VARNAMES <- yaml.load_file("vnames.yaml")

indexVariables<-c(
   "v2x_suffr",
   "FHKN_frefair",
   "v2x_elecoff",
   "v2x_frassoc_thick",
   "v2x_freexp_thick"
   )



models <- mclapply(c(indexVariables,""), function(ivar){
   variables <- indexVariables[which(!indexVariables == ivar)]
   data$index <- apply(data[variables],1,function(r){
      purrr::reduce(r,`*`)
   })
   data$index <- offset(data$index)

   mdl <- glm(c2_onset ~ index + llnpop200 + llnGDPPerCapita200 + timesince + timesince_sq + timesince_cb + ethfrac + lmtnest + nbConflict,
      family = binomial("logit"),
      data = data[data$year >= 1946,])
   mdl
}, mc.cores = 7)


texreg(models,
   custom.coef.map = VARNAMES,
   custom.model.names = c(paste0(
      "Sans ", indexVariables
   ), "All")) %>%
   stripenv() %>%
   writeLines("Out/index_testing.tex")
