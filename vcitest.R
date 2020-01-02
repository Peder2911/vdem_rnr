library(timelib)

data <- readRDS("Cache/prepped_data.rds")

indexVariables<-c(
   "v2x_suffr",
   "FHKN_frefair",
   "v2x_elecoff",
   "v2x_frassoc_thick",
   "v2x_freexp_thick"
   )

models <- lapply(c(indexVariables,""), function(ivar){
   variables <- indexVariables[which(!indexVariables == ivar)]
   data$index <- apply(data[variables],1,function(r){
      purrr::reduce(r,`*`)
   })
   print(variables)
   data$index <- offset(data$index)

   mdl <- glm(c2_onset ~ index + llnpop200 + llnGDPPerCapita200 + decay_c_term_short + ethfrac + lmtnest + anyNbConflict,
      family = binomial("logit"),
      data = data[data$year >= 1946,])
   list(leftout = ivar, model = mdl)
})

saveRDS(models, "Cache/leftouts.rds")
