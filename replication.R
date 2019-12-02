#
# This file replicates steps from a STATA do-file called 
# _master_VDemCivPeace_V7 
# Its checksum is 
# daafbab1f26a0dfd08a194ffd0a047dc  
#

shh <- suppressPackageStartupMessages

shh(library(haven))
shh(library(knitr))
shh(library(glue))
shh(library(crayon))
shh(library(dplyr))
shh(library(tictoc))
shh(library(xtable))

source("constants.R")
source("functions.R")


# ================================================

# Make cache if cache is not present
currentCache <- list.files("Cache")
tic("Runtime")

if(!CACHEFILE %in% currentCache){
   cat("\x1b[33mNo cache!\x1b[0m\n")
   dat <- read_dta("Data/FVP_masterdata_copy.dta")

   # Shrinking the data
   dat <- dat %>%
      filter(
         gwno <= 1000,
         year <= 2016,
         year >= 1900,
      ) %>%
      select(
         lnpop = lnpop200, conflict,
         gwno, year, ltsc0, ltsc1, ltsc2, 
         polity2, v2x_polyarchy, #lnGDPcap? lnGDPPerCap?
         v2x_partip, v2x_liberal, v2x_libdem, v2x_frassoc_thick,
         v2x_suffr, v2xel_frefair, v2x_elecoff, v2x_cspart,
         v2xlg_legcon, v2x_jucon, v2xcl_rol, v2xel_locelec ,
         v2xel_regelec, v2elffelr, v2clrspct, v2cltrnslw,
         v2clacjstm, v2clacjstw, v2clprptym, v2clprptyw,
         v2clrelig , v2xcl_dmove, v2pepwrsoc, v2x_freexp_thick,
         v2clacjstm, v2clacjstw, v2elembaut, v2elembcap,
         v2elrgstry, v2elvotbuy, v2elirreg, v2elfrfair,
         v2x_elecreg , YMHEPSSP2, lnpop200, lnGDPPerCapita200, 
         xconst, xropen, xrcomp
      ) 

   cat(glue("nrow: {nrow(dat)} ncol: {ncol(dat)}\n"))
   saveRDS(dat,glue("Cache/{CACHEFILE}"))
} else {
   cat("\x1b[35mUsing cache\x1b[0m\n")
   dat <- readRDS(glue("Cache/{CACHEFILE}"))
   cat(glue("nrow: {nrow(dat)} ncol: {ncol(dat)}") + "\n\n")
}

# ================================================
# Variable gen. 

dat <- dat %>%
   group_by(gwno) %>%
   mutate(
      cowyear = as.numeric(year <= 1945),
      anyConflict = as.numeric(conflict != 0)) %>%
   onsetAndTerm(anyConflict) %>%
   onsetAndTerm(onset_anyConflict_1,n=2) %>%
   ungroup()

saveRDS(dat,"tee2.rds")

# ================================================
# Indexes (indices?)

normalize <- function(x) (x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T))

dat <- dat %>%
   mutate(
      tsc = exp(max(ltsc1,ltsc2, na.rm = T)),
      tsc = ifelse(ltsc0 > 0, 0, tsc),
      decay_2 = 2^(-tsc/2),

      FHKN_frefair_temp = (v2elembaut + v2elembcap + v2elrgstry + v2elvotbuy + v2elirreg + v2elfrfair)/6,
      FHKN_frefair = (FHKN_frefair_temp + 2.533413)/ 6.125325,

      free_fair_narrow = (v2x_suffr * FHKN_frefair * v2x_elecoff),
      free_fair_elections = (v2x_suffr * FHKN_frefair * v2x_elecoff * v2x_frassoc_thick * v2x_freexp_thick),

      combined_vertical = (2*free_fair_elections + v2x_cspart)/3,

      FHKN_RoL = (v2clrspct + v2cltrnslw +(v2clprptym + v2clprptyw)/2 )/3,
      FHKN_RoL = normalize(FHKN_RoL),
      #(FHKN_RoL - min(FHKN_RoL)) / (max(FHKN_RoL) - min(FHKN_RoL)),

      horizontal_constraint =  (v2xlg_legcon  + v2x_jucon + FHKN_RoL)/3,
      horizontal_constraint_narrow =  (v2xlg_legcon + v2x_jucon)/2,

      minority_rep = (v2clrelig  + (v2xcl_dmove) + v2pepwrsoc)/3,
      civlib = (v2x_freexp_thick + v2x_frassoc_thick)/ 2,

      # Her er elf_min / elf_max relatert til v2elffelr-variabelen   
      # !!!
      SDI =(0.5*v2xel_locelec + 0.5*v2xel_regelec)*(normalize(v2elffelr)),

      # Denne virker feil, v2xcl_dmove er flyttall
      #replace movement_restricted_group = 4 if v2xcl_dmove == 0 | v2xcl_dmove == 1 | v2xcl_dmove == 4
      #replace movement_restricted_group = 2 if v2xcl_dmove == 2
      #replace movement_restricted_group = 0 if v2xcl_dmove == 3

      xconst = ifelse(xconst == -10, NA, xconst),
      xropen = ifelse(xropen == -10, NA, xropen),
      xrcomp = ifelse(xrcomp == -10, NA, xrcomp),
      x_polity = (xconst + xropen + xrcomp) - 7,
      x_polity_sq = x_polity^2,

      ffe_sq = free_fair_elections^2,
      hc_sq = horizontal_constraint^2,
      mr_sq = minority_rep^2,

      vert_hor_int = free_fair_elections*horizontal_constraint,
      rol_hor_int = FHKN_RoL*horizontal_constraint,
      minor_hor_int = minority_rep*horizontal_constraint,

      vert_horsq_int = free_fair_elections*hc_sq,

      vertsq_hor_int = ffe_sq*horizontal_constraint,

      vertsq_horsq_int = ffe_sq*hc_sq,

      min_horsq_int = minority_rep*hc_sq,
      minsq_hor_int = mr_sq*horizontal_constraint,
      minsq_horsq_int = mr_sq*hc_sq,

      categorical = case_when(

         free_fair_elections > 0.25 & horizontal_constraint > 0.3 ~ 2,

         free_fair_elections > 0.25 & horizontal_constraint <= 0.3 |
         free_fair_elections <= 0.25 & horizontal_constraint > 0.3 ~ 1,

         !is.na(free_fair_elections) & !is.na(horizontal_constraint) ~ 0,

         TRUE ~ as.numeric(NA),
      ))

tee2 <- saveRDS(dat,"tee.rds")
cat(paste0("\x1b[33m",ncol(dat),"\x1b[0m\n"))

# ================================================

#model <- glm(as.factor(c_onset) ~ 
   #horizontal_constraint +
   #lnpop +
   #lnGDPPerCapita200 +
   #decay_2, data = dat,
   #family = binomial("logit"))
#
#print(summary(model))
#kable(broom::tidy(model), "rst")
#
## ================================================
#
cat(glue("\n\n{strrep('*',64)}\n\n\n"))
cat(green("\x1b[32mAll done!\x1b[0m\n"))
toc()
