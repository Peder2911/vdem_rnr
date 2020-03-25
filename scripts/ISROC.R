shh <- suppressPackageStartupMessages
shh(library(dplyr))
shh(library(evallib))
shh(library(ggplot2))
shh(library(parallel))

regular <- readRDS("Cache/regular.rds")
polity <- readRDS("Cache/polity.rds")

preds <- lapply(c(regular,polity), function(mdl){
   dat <- mdl$data
   dat$pred <- predict(mdl,dat,type = "response")
   select(dat,gwno,year,pred,outcome = c2_onset)
})

curves <- mclapply(preds, function(dat){
   dat <- dat[complete.cases(dat),]
   roc(dat$pred, dat$outcome)
}, mc.cores = 7)

names <- c(
   "Vertical constraints", 
   "Horizontal constraints", 
   "Polity Index")
orderednames <- factor(
      names,
      levels = names
   )

for(i in 1:3){
   curves[[i]]$type <- orderednames[i]
}

aucs <- sapply(curves,function(curve){
   auc(curve$fallout,curve$recall)
   }) 

aucs <- tibble(val = aucs, type = orderednames) %>%
   mutate(
      repr = paste0(type, ": ", round(val, digits = 3)),
      order = row_number()
      )

write.csv(aucs,"/tmp/view.csv",row.names = FALSE)

curves <- do.call(rbind, curves) 
rocCurve <- ggplot(curves, aes(x=fallout,y=recall,color=type)) +
   geom_path() +
   geom_abline(intercept = 0, slope = 1) +
   geom_text(data = aucs, 
      aes(x = 1, y = 0.4 - 0.1 * order, label = repr),
      size = 6,
      hjust = 1) +
   labs(x = "Fallout", y = "Recall", color = "Predictor")+
   theme_classic() +
   scale_color_manual(values = c(
   `Vertical constraints`="#e41a1c", 
   `Horizontal constraints`="#377eb8", 
   `Polity Index`="#4daf4a"
   )) +
   theme(
      text = element_text(size = 18),
      legend.position = "bottom") +
   guides(color=guide_legend(override.aes=list(size=5)))
   

ggsave("Out/roc_comparison.pdf", rocCurve, 
   device = "pdf", height = 9, width = 9) 
