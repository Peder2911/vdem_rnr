library(glue)
library(MASS)
library(sandwich)
library(ggplot2)
library(parallel)
library(gridExtra)
library(imlib)
suppressPackageStartupMessages(library(dplyr))

# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
RES <- 100 
NSIM <- 100000

# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%

logitToProb <- function(x) exp(x) / (1+exp(x))

countryCl <- function(...){
   sandwich::vcovCL(..., cluster = ~gwno)
}

margPlot <- function(data){
   data %>%
      ggplot(aes(x=xvar, y=sim_mean))+
         geom_line() +
         geom_ribbon(aes(ymin = sim_lower, ymax = sim_upper), alpha = 0.3,size = 0) +
         scale_fill_discrete(guide = "none") +
         theme(legend.position = "bottom") 
}

margPlotFn <- function(simdata, origdata, name){
   margPlot(simdata)
}

saveSet <- function(...,name){
   ggsave(glue("Out/{name}"),margPlotFn(...,name=name),device = "pdf",
      height = 4, width = 8)
   }


# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%

   dat <- readRDS("Cache/prepped_data.rds")
   partial <- dat[dat$year >= 1946,]

   models <- list(
      list(file="t3_model_4.rds",var="lfree_fair_elections"),
      list(file="t3_model_5.rds",var="lhorizontal_constraint_narrow")
   )

   for (model in models) {
      print(model)
      m <- readRDS(file.path("Cache",model$file))
      variables <- list()
      variables[[model$var]] <- seq(0,1,length.out=RES+1)

      simset <- makeTestSet(partial,variables,mean)

      simset[[paste0(model$var,"_sq")]] <- simset[[model$var]] ^ 2
      simset$timesince_sq <- simset$timesince ^ 2
      simset$timesince_cb <- simset$timesince ^ 3
      simset$x_polity_sq <- simset$x_polity ^ 2
      print(length(coef(m)))
      print(dim(countryCl(m)))

      print("yeehaw")

      simset <- cbind(simset,sim(simset,m,vcov=countryCl))
      simset$sim_mean <- logitToProb(simset$sim_mean)
      simset$sim_upper <- logitToProb(simset$sim_upper)
      simset$sim_lower <- logitToProb(simset$sim_lower)
      simset$xvar <- simset[[model$var]]

      #write.csv(simset,simfilePath,row.names = FALSE)
      modelname <- gsub("\\..*$","",model$file)
      writeLines(glue("Saving {modelname}"))
      saveSet(simset,partial,name=paste0(modelname,".pdf"))
   }

   writeLines("Done!")
