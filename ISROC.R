library(dplyr)
library(evallib)
library(ggplot2)

rocPlots <- function(models,depvar){
   lapply(models,function(m){
      dat <- m@frame
      dat[["outcome"]] <- dat[[depvar]]

      dat$pred <- predict(m,dat,type="response")

      roc <- roc(dat$pred,dat$outcome)
      auc <- auc(roc$fallout,roc$recall) 

      plt <- roc %>% 
      ggplot(aes(x=fallout,y=recall)) + 
         geom_line()+
         geom_abline(intercept=0,slope=1,color="red")+
         labs(subtitle=paste0("AUC: ",round(auc,4))) 
   })
}

anyConflict <- rocPlots(readRDS("Cache/models_partial_re.rds"),"c2_onset") %>%
   saveRDS("Cache/rocs.rds")
majorConflict <- rocPlots(readRDS("Cache/major_models_partial_re.rds"),"major_c2_onset") %>%
   saveRDS("Cache/major_rocs.rds")
