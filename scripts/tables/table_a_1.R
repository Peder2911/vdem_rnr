library(dplyr)
library(yaml)
library(tibble)
library(knitr)

data <- readRDS("Cache/prepped_data.rds")
data <- data[data$year > 1944 & data$year < 2016,]
vnames <- yaml.load_file("vnames.yaml")
print(nrow(data))

source("functions.R")

nnotna <- function(x,na.rm){sum(!is.na(x))}
summaryFunctions <- list(
         "Obs"=nnotna,
         "Mean"=mean,
         "SD"=sd,
         "Min"=min,
         "Max"=max)
variables <- c(
         "c2_onset",
         "lfree_fair_elections",
         "lhorizontal_constraint_narrow",
         "llnpop200",
         "llnGDPPerCapita200",
         "llnGDPcap_oilrent",
         "lgrGDPPerCapita200",
         "ltimeindep",
         "timesince",
         "x_polity"
         )

summaries <- lapply(variables,function(vname){
                sapply(summaryFunctions,function(fn){
                   fn(data[[vname]],na.rm = TRUE)
                })
             })

df <- bind_rows(summaries)
df <- tibble(cbind(sapply(variables,function(v){vnames[[v]]}),df))
names(df)[1] <- "Variable"
writeLines(knitr::kable(df,format="latex"),"Out/table_a_1.tex")

