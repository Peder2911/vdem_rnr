library(glue)
library(MASS)
library(sandwich)
library(ggplot2)
library(imlib)
library(parallel)
library(gridExtra)
suppressPackageStartupMessages(library(dplyr))

# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
RES <- 100 
NSIM <- 10000

# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%

logitToProb <- function(x) exp(x) / (1+exp(x))

countryCl <- function(...){
   sandwich::vcovCL(..., cluster = ~gwno)
}

margPlot <- function(data,x,y,margins,labs){
   x <- enquo(x)
   y <- enquo(y)

   data %>%
      filter(!!y %in% margins) %>%
      ggplot(aes(x=!!x, y=sim_mean, 
         color = factor(!!y),fill = factor(!!y)))+
         geom_line() +
         geom_ribbon(aes(ymin = sim_lower, ymax = sim_upper), alpha = 0.3,size = 0) +
         scale_fill_discrete(guide = "none") +
         theme(legend.position = "bottom") +
         labs
}

margPlotSet <- function(simdata, origdata, name){
   lfree_marg <- quantile(origdata$lfree_fair_elections,c(0.25,0.75),na.rm = T) %>%
      round(1)
   lhorizontal_marg <- quantile(origdata$lhorizontal_constraint_narrow,c(0.25,0.75),na.rm = T) %>%
      round(1)

   title <- name 
   list(
      margPlot(simdata, lfree_fair_elections, lhorizontal_constraint_narrow,
         lfree_marg, labs(
            #title = title, 
            x = "Vertical Constraints",
            color = "Horizontal Constraints",
            y = "P (Conflict onset)"
         )),
      margPlot(simdata, lhorizontal_constraint_narrow, lfree_fair_elections,
         lhorizontal_marg, labs(
            #title = title, 
            x = "Horizontal Constraints",
            color = "Vertical Constraints",
            y = "P (Conflict onset)"
         ))
   )
}

saveSet <- function(...,name){
   set <- margPlotSet(...,name = name)
   set$nrow <- 1
      ggsave(glue("Out/{name}"),do.call(grid.arrange,set),device = "pdf",
         height = 4, width = 8)
   }


# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%

   dat <- readRDS("Cache/prepped_data.rds")
   partial <- dat[dat$year >= 1946,]

   models <- c(
      "t3pol_model_6.rds",
      "t3_model_7.rds",
      "t3pol_model_7.rds",
      "t3_model_6.rds"
   )

   for (model in models) {
      simfile <- paste0(model,".simulations.csv")
      simfilePath <- file.path("Cache",simfile)
      if(simfile %in% list.files("Cache")){
         writeLines(glue("Using cache for {model} simulations"))
         simset <- read.csv(simfilePath, stringsAsFactors = FALSE)
      } else {
         writeLines(glue("Caching {model} simulations"))
         m <- readRDS(file.path("Cache",model))

         xvar <- "lhorizontal_constraint_narrow"
         yvar <- "lfree_fair_elections"

         variables <- list()
         variables[[xvar]] <- seq(0,1,length.out=RES+1)
         variables[[yvar]] <- seq(0,1,length.out=RES+1) 


         simset <- makeTestSet(partial,variables,mean)

         simset[[paste(xvar,yvar,sep=":")]] <- simset[[xvar]] * simset[[yvar]] 
         simset[[paste(yvar,xvar,sep=":")]] <- simset[[xvar]] * simset[[yvar]] 
         simset[[paste0(xvar,"_sq")]] <- simset[[xvar]] ^ 2
         simset[[paste0(yvar,"_sq")]] <- simset[[yvar]] ^ 2

         simset$timesince_sq <- simset$timesince ^ 2
         simset$timesince_cb <- simset$timesince ^ 3
         simset$x_polity_sq <- simset$x_polity ^ 2

         simset <- cbind(simset,sim(simset,m,vcov = countryCl))
         simset$sim_mean <- logitToProb(simset$sim_mean)
         simset$sim_upper <- logitToProb(simset$sim_upper)
         simset$sim_lower <- logitToProb(simset$sim_lower)

         write.csv(simset,simfilePath,row.names = FALSE)
      }
      modelname <- gsub("\\..*$","",model)
      writeLines(glue("Saving {modelname}"))
      saveSet(simset,partial,name=paste0(modelname,".pdf"))
   }

   writeLines("Done!")
