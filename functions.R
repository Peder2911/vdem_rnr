
suppressPackageStartupMessages(library(timelib))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(stringr))

#' makeChangeDecay
#'

makeChangeDecay <- function(variable,threshold = 0.1){
   compfun <- ifelse(threshold > 0,`>`,`<`)
   change <- variable - offset(variable)
   thresh <- as.numeric(compfun(change,threshold))
   decay <- offset(halflife(2)(nsince(thresh)))
   ifelse(is.na(decay),0,decay)
}


#' clusteredTexreg
#'
#' Convenience function that replaces standard standard errors with
#' clustered standard errors for regular glm models (not glmer models).
clusteredTexreg <- function(...){
   models <- ..1

   clusteredVcov <- function(m){
      #vcovHC(m, type = "HC0")#, cluster = "gwno")
      vcovCL(m,cluster=~gwno)
   }

   tests <- lapply(models, function(model){
      cluster <- !"glmerMod" %in% class(model)

      if(cluster){
         coeftest(model,clusteredVcov)
      } else {
         summary(model)$coefficients
      }
   })

   args <- c(list(...),list(
      override.se = lapply(tests, function(t) t[,2]),
      override.pvalues = lapply(tests, function(t) t[,4])
   ))
   do.call(texreg,args)
}

#' makeOnset 
#' 
#' Creates a 1-lagged decay variable
makeOnset <- function(event,tolerance = NULL){
   if(!is.null(tolerance)){
      event <- crop(sustain(event,casefun = function(x){!is.na(x) & x == 1},tolerance = tolerance),tolerance)
   }

   res <- change(event,"onset") %>%
      ifelse(. == 0 & event == 1, NA, .) 
   res[1] <- ifelse(event[1] == 0, 0, res[1])
   res
}

#' makeDecay 
#' 
#' Creates a 1-lagged decay variable
makeDecay <- function(event,fn = function(x){x},missingval = 9999999){
   nsince(event) %>%
      ifelse(is.na(.), missingval, .) %>%
      offset(1) %>%
      fn()
}

#' curved
#' 
#' Returns a function that will yield a curved decay variable 
#' sustain controls the acuteness of the curve.
curvedDecay <- function(sustain = 2){
   function(v){
      sustain ^ (-v/2)
   }
}

#' halflife 
#' 
#' Returns a function that will yield an exponential decay that halves at t.
halflife <- function(t, init = 1){
   function(x){
      init * (0.5 ^ (x * (1/t)))
   }
}

#' normalize 
#' 
#' Normalizes a vector, making it vary between 0 and 1 
normalize <- function(x) (x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T))

#' nonmissing 
#' 
nonmissing <- function(x) !is.na(x)

#' fixElvar
#'  
#' "Fixes" elvars (v2elrgstry v2elvotbuy v2elirreg v2elfrfair) by applying several functions
fixElvar <- function(var,censor){
   censor <- censor == 0

   var %>%
      sustain(nonmissing,999) %>%
      ifelse(censor, 0, .)
}
 
#' rollingFlip 
#' 
#' Rolls over a vector and "flips" the previous
#' value. Used to find onsets and terminations.
rollingFlip <- function(x,mode){
   flip <- function(x) as.numeric(!as.logical(x))
   mode_num <- switch(mode, onset = 1, term = 0)
   sapply(1:length(x), function(i){
      if(i > 1){
         as.numeric(x[i-1] == flip(mode_num) &
                    x[i] == mode_num)
      } else {
         NA 
   }})
}

#' rollingFlip 
#' 
#' "Moves" values along a time-axis. 
rollingOffset <- function(x,n = 1){
   sapply(1:length(x), function(i){
      if(i > n){
         x[i-n]
      } else {
         NA
      }
   })
}

#' onsetAndTerm
#'
#' Applies rollingFlip to a possibly grouped dataset
#'
onsetAndTerm <- function(data,...,n=1){
   if(!is.null(groups(data)[[1]])) {
         grplen <- length(unique(data[[groups(data)[[1]]]]))
   } else {
      grplen <- 0
   }
   if(grplen > 1){
      grpvar <- groups(data)[[1]] # Only supports one grp. variable 

      print(glue::glue("Rolling over {length(unique(data[[grpvar]]))} separate groups "))

      prev <<- 0
      
      res <- lapply(unique(data[[grpvar]]), function(grpval){
         nmbr <- as.character(grpval)
         
         
         prev <<- nchar(nmbr)

         sub <- arrange(data[data[[grpvar]] == grpval,],year)
         onsetAndTerm(sub, ..., n = n)}) %>%
         bind_rows()
      
      res
   } else {
      variables <- sapply(substitute(list(...)),as.character)[-1]
      for(v in variables){
         data[[paste0("onset_",v,"_",n)]] <- rollingFlip(data[[v]],"onset",n)
         data[[paste0("term_",v,"_",n)]] <- rollingFlip(data[[v]],"term",n)
      }
      data
   }
}

#' stripenv
#' 
#' "Prepares" the table for Overleaf by commenting out some tags. 

stripenv <- function(tex){
   pattern <- "^\\\\(caption|label|(begin|end)\\{(center|table))"
   lines <- str_split(tex,"\n",simplify = TRUE)
   lines <- sapply(lines, function(l){
      if(str_detect(l, pattern)){
         paste0("%",l)
      } else {
         l
      }
   })
   glue_collapse(lines,"\n")
}
