shh <- suppressPackageStartupMessages 

shh(library(RSQLite))
shh(library(dplyr))
shh(library(ggplot2))
shh(library(ggdark))
shh(library(gridExtra))
shh(library(stargazer))
shh(library(glue))
shh(library(tidyr))


if(!"ged.rds" %in% list.files("Cache")){
   c <- dbConnect(SQLite(),"SuppData/ged191.sqlite")
   ged <- dbGetQuery(c,"SELECT * FROM ged")

# ================================================

   zeropad <- function(data){
      blank <- lapply(unique(data$country_id), function(ccode){
         data.frame(country_id = ccode, year = min(data$year):max(data$year), deaths = 0)}) %>% 
         do.call(rbind, .) %>%
         as.data.frame()
      print(head(blank)) 

      m <- merge(data,blank,c("year","country_id"),all.y = TRUE)
      m$deaths <- ifelse(is.na(m$deaths.x), m$deaths.y, m$deaths.x)

      arrange(select(m,year,country_id,deaths),country_id,year)
   }

# ================================================

   ged_sum <- ged %>% 
                group_by(country_id, year) %>% 
                summarize(deaths = sum(best)) %>% 
                ungroup() %>%
                zeropad()

   dbDisconnect(c)
   saveRDS(ged_sum, "Cache/ged.rds")
} else {
   ged_sum <- readRDS("Cache/ged.rds")
}

repr <- readRDS("data.rds")
anacountries <- unique(repr$gwno)
missing <- anacountries[!anacountries %in% ged_sum$country_id]
casualty_padding <- lapply(missing, function(ccode){
   data.frame(year = min(repr$year):max(repr$year), country_id = ccode, deaths = 0) 
   }) %>%
   do.call(rbind, .)

write.csv(casualty_padding,"/tmp/view.csv",row.names = FALSE)

ged_sum <- rbind(casualty_padding,ged_sum) %>%
   filter(country_id %in% anacountries) %>%
   rename(gwno = country_id)

anadata <- repr %>%
   filter(year %in% min(ged_sum$year):max(ged_sum$year)) %>%
   merge(ged_sum,c("gwno","year"))

# ================================================
# *** 
ctry <- sample(anadata$gwno, size = 1)
anadata %>%
   select(year,gwno,deaths,lhorizontal_constraint_narrow,lfree_fair_elections,anyConflict,decay_c_term_long,c_term) %>%
   filter(gwno == ctry) %>%
   write.csv("/tmp/view.csv",row.names = FALSE)

# *** 
# ================================================

DEPENDENT_VARIABLE <- "deaths"

INDEP_ONE <- "lfree_fair_elections"
INDEP_TWO <- "lhorizontal_constraint_narrow"

BASE_CONTROLS <- c("llnpop200", "llnGDPPerCapita200","decay_c_term_short")
NEW_CONTROLS <- c("ethfrac", "lmtnest", "anyNbConflict")

TIME_FIXED_EFFECTS <- c("as.factor(year)")
CTRY_FIXED_EFFECTS <- c("as.factor(gwno)")

makeFormula <- function(lhs, rhs){
   glue("{lhs} ~ {glue_collapse(rhs,' + ')}") %>%
      as.formula()
}

modelthis <- function(lhs){
   function(rhs){makeFormula(lhs,rhs)}
}

modelwith <- function(rhs){
   function(lhs){makeFormula(lhs,rhs)}
}


formulae <- lapply(list(
   c(INDEP_ONE,BASE_CONTROLS),
   c(INDEP_ONE,BASE_CONTROLS,NEW_CONTROLS),
   #c(INDEP_ONE,BASE_CONTROLS,NEW_CONTROLS,CTRY_FIXED_EFFECTS),
   c(INDEP_ONE,BASE_CONTROLS,NEW_CONTROLS,TIME_FIXED_EFFECTS),
   c(INDEP_TWO,BASE_CONTROLS),
   c(INDEP_TWO,BASE_CONTROLS,NEW_CONTROLS),
   #c(INDEP_TWO,BASE_CONTROLS,NEW_CONTROLS,CTRY_FIXED_EFFECTS),
   c(INDEP_TWO,BASE_CONTROLS,NEW_CONTROLS,TIME_FIXED_EFFECTS)
   ), modelwith)

models <- lapply(formulae,function(f){
   #lm(f,data = anadata)
   glm(f("deaths"),family = poisson(link="log"), data = anadata)
})
models <- c(models,lapply(formulae,function(f){
   #lm(f,data = anadata)
   lm(f("deaths"), data = anadata)
}))
models <- c(models,lapply(formulae,function(f){
   #lm(f,data = anadata)
   lm(f("log(deaths+1)"), data = anadata)
}))

saveRDS(models,"Cache/model_casualties.rds")

writeLines(stargazer(models,type = "html",omit="year"),"/tmp/tab.html")

statplots <- lapply(models, function(m){
   data.frame(ftd = m$fitted.values,rsd = m$residuals) %>%
      ggplot(aes(x=ftd,y=rsd)) + geom_point()
})
do.call(grid.arrange, c(statplots,list(ncol = 2))) %>%
   ggsave("/tmp/plt.pdf",.,device = "pdf", height = 20, width=10)

decplots <- anadata %>%
   filter(gwno == 101) %>%
   select(year,decay_c_term,decay_c_term_short,anyConflict) %>%
   gather(var,val,-year) %>%
   ggplot(aes(x=year,y=val,color=var)) + geom_line() + dark_mode()
 
ggsave("/tmp/plt2.pdf",decplots,device = "pdf", height = 10, width=20)
