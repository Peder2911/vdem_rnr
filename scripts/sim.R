library(glue)
library(MASS)
library(sandwich)
library(ggplot2)
library(imlib)
library(parallel)
library(gridExtra)
suppressPackageStartupMessages(library(dplyr))

# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
NHOR <- 100 
logitToProb <- function(x) exp(x) / (1+exp(x))

# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%

dat <- readRDS("Cache/prepped_data.rds")
partial <- dat[dat$year >= 1946,]

# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
countryCl <- function(...){
   sandwich::vcovCL(..., cluster = ~gwno)
}

# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
# Figure 3 %=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
# =%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
# This figure shows the marginal effect of a variable, given
# a range of values on another variable.

#' @title figure_3_marg
#' @description Show marginal change in x for different values of y
figure_3_marg <- function(data,model,x,y){
   mchange <- function(v){
      v[2]-v[1]
   }

   xname <- as.character(substitute(x))
   yname <- as.character(substitute(y))

   variables <- list()
   variables[[xname]] <- seq(0,1,length.out = 100) 
   variables[[yname]] <- quantile(data[[yname]],c(0.25,0.75),na.rm =T) %>% 
      round(digits = 2)

   testset <- makeTestSet(data,variables,mean)

   # adding interaction terms
   testset[[paste0(xname,"_sq")]] <- testset[[xname]]^2 
   testset[[paste0(xname,"_cb")]] <- testset[[xname]]^3
   testset[[paste0(xname,":",yname)]] <- testset[[xname]] * testset[[yname]]
   testset[[paste0(yname,":",xname)]] <- testset[[paste0(xname,":",yname)]]

   testset$timesince_sq <- testset$timesince ^ 2
   testset$timesince_cb <- testset$timesince ^ 3
   testset$x_polity_sq <- testset$x_polity ^ 2

   cbind(testset,sim(testset,model,10000,vcov = countryCl))
}

figure_3_marg_plt<- function(dat,x,y,labs){
   x <- enquo(x)
   y <- enquo(y)
   ggplot(dat,aes(!!x,logitToProb(sim_mean),color=factor(!!y))) +
      geom_ribbon(aes(
         ymin=logitToProb(sim_lower),ymax=logitToProb(sim_upper),
         fill = factor(!!y)),alpha = 0.2,size = 0) + 
      geom_line() +
      #scale_y_continuous(limit = c(0,.5)) +
      scale_fill_discrete(guide = "none") + 
      theme(legend.position="bottom") +
      labs
}

plotFromModel <- function(data,modelfile,title){
   m <- readRDS(modelfile)

   list( 
      figure_3_marg(data,m,lhorizontal_constraint_narrow,lfree_fair_elections) %>%
         figure_3_marg_plt(lhorizontal_constraint_narrow,lfree_fair_elections,
            labs = labs(y="P(Conflict onset)",x="Horizontal constraints",color="Vertical constraints",subtitle=title)),
      figure_3_marg(data,m,lfree_fair_elections,lhorizontal_constraint_narrow) %>%
         figure_3_marg_plt(lfree_fair_elections,lhorizontal_constraint_narrow,
            labs = labs(y="P(Conflict onset)",x="Vertical constraints",color="Horizontal constraints",subtitle=title))
   )
}

plots <- c(
   plotFromModel(partial,"Cache/t3pol_model_6.rds","Model 6 w/. polity"),
   plotFromModel(partial,"Cache/t3pol_model_7.rds","Model 7 w/. polity"),
   plotFromModel(partial,"Cache/t3_model_6.rds","Model 6"),
   plotFromModel(partial,"Cache/t3_model_7.rds","Model 7")
)

blitout <- function(what,where){
   ggsave(where,do.call(grid.arrange,what),device = "pdf",
      height = 6, width = 6)
}

plots[c(5,6,1,2)] %>%
   blitout("Out/model6_sim.pdf")
plots[c(7,8,3,4)] %>%
   blitout("Out/model7_sim.pdf")

