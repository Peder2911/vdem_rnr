shh <- suppressPackageStartupMessages

shh(library(haven))
shh(library(knitr))
shh(library(glue))
shh(library(crayon))
shh(library(dplyr))
shh(library(tictoc))
shh(library(xtable))

shh(library(tidyr))
shh(library(ggplot2))
shh(library(ggdark))

shh(library(stargazer))

shh(library(timelib))

source("functions.R") 

# ================================================
#  This script reproduces the data-manipulation 
#  steps necessary to train models for the RNR.
# 
#  These include creating indices, onset and decay
#  variables and merging with combat-death
#  data.
# 
#  The script caches the initial subset of the data
#  to save time. To re-run the first steps, simply
#  delete the file "Cache/cached.rds"
# 

# ================================================
# CONSTANTS ======================================

CACHEFILE <- "prepped_data.rds"
VERBOSE <- TRUE

# ================================================
# Make cache if cache is not present

currentCache <- list.files("Cache")
tic("Runtime")

if(!CACHEFILE %in% currentCache){
   cat("\x1b[33mNo cache!\x1b[0m\n")
   dat <- read_dta("InputData/FVP_masterdata_copy.dta")

   # Shrinking the data
   dat <- dat %>%
      filter(
         gwno <= 1000,
         year <= 2016,
         year >= 1900,
      ) %>%
      select(
         #===================================================
         # Time series ID variables
         gwno, year, 

         # Conflict 
         conflict,

         # Control variables
         lnpop200, lnGDPPerCapita200, 

      
         # Special controls
         grGDPPercapita200, lnGDPcap_oilrent, ltimeindep,


         #===================================================
         v2x_elecreg,

         # Vertical constraints stuff
         v2elembaut,v2elembcap,v2elrgstry,v2elvotbuy,v2elirreg,v2elfrfair,

         # Narrow
         v2x_suffr, v2x_elecoff,
         # Thicc
         v2x_frassoc_thick, v2x_freexp_thick,
         # Combined
         v2x_cspart,

         #===================================================
         # Horizontal constraints stuff

         # FHKN_RoL
         v2clrspct, v2cltrnslw, v2clprptym, v2clprptyw,

         # Narrow
         v2xlg_legcon, v2x_jucon,

         #===================================================
         # Polity scores    
         xconst, xropen, xrcomp,

         #===================================================
         # Neighbouring conflict
         nb_conflict,

         #===================================================
         # Polity score (alternative hyp.)
         polity = polity2,

         #===================================================
         # Region name (for dummies) 
         region_name

      ) 

   fearon_laitin <- read_dta("SuppData/repdata.dta") %>%
      select(gwno = ccode,year,ethfrac,lmtnest) %>% 
      group_by(gwno) %>%
      filter(year == max(year)) %>%
      ungroup() %>%
      select(gwno,ethfrac,lmtnest)
   dat <- merge(dat, fearon_laitin, c("gwno"), all.x = TRUE) %>% arrange(gwno,year)

   cat(glue("nrow: {nrow(dat)} ncol: {ncol(dat)}\n"))
   saveRDS(dat,glue("Cache/{CACHEFILE}"))

} else {
   cat("\x1b[35mUsing cache\x1b[0m\n")
   dat <- readRDS(glue("Cache/{CACHEFILE}"))
   cat(glue("nrow: {nrow(dat)} ncol: {ncol(dat)}") + "\n\n")
}

# ===================================================
# ===================================================
# ===================================================
# Variable gen. 

# Conflict variables

dat <- dat %>%
   mutate(

      anyConflict = as.numeric(conflict != 0),
      majorConflict = as.numeric(conflict == 2),
      minorConflict = as.numeric(conflict == 1),

      nbConflict = as.numeric(nb_conflict != 0),
      major_nbConflict = as.numeric(nb_conflict == 2)
   )

# Time-series specific variables, needing grouping

dat <- dat %>%
   group_by(gwno) %>%
   mutate(

      c0_onset = makeOnset(anyConflict),
      c2_onset = makeOnset(anyConflict,tolerance = 1),
      c4_onset = makeOnset(anyConflict,tolerance = 3),
      c7_onset = makeOnset(anyConflict,tolerance = 6),
      c10_onset = makeOnset(anyConflict,tolerance = 10),

      c_term = change(anyConflict,"term"),
      c_term = offset(ifelse(c_term == 0 & anyConflict == 1,NA,c_term),-1),

      decay_c_term = makeDecay(c_term,halflife(10)),
      decay_c_term_long = makeDecay(c_term,halflife(35)),
      decay_c_term_short = makeDecay(c_term,halflife(5)),

      timesince_tr = nsince(c_term) %>% offset(1),
      timesince_tr = ifelse(is.na(timesince_tr),999, timesince_tr),
      timesince_tr_sq = timesince_tr ^ 2, 
      timesince_tr_cb = timesince_tr ^ 3, 

      timesince = nsince(as.numeric(c_term == 1|row_number() == 1)) %>% offset(1),
      timesince_sq = timesince ^ 2,
      timesince_cb = timesince ^ 3,

      # Rescaling time since conflict to fix a convergence issue.
      # This is shown in the appendix
      timesince_norm = timesince * 0.0001, #/ max(timesince,na.rm = T),
      timesince_sq_norm = timesince_sq * 0.0001, #/ max(timesince_sq,na.rm = T),
      timesince_cb_norm = timesince_cb * 0.0001, #/ max(timesince_cb,na.rm = T),

      major_c_onset = makeOnset(majorConflict),
      major_c2_onset = makeOnset(majorConflict,tolerance = 1),

      major_c_term = change(majorConflict,"term"),
      major_c_term = offset(ifelse(major_c_term == 0 & majorConflict == 1,NA,major_c_term),-1),
      major_decay_c_term = makeDecay(major_c_term,halflife(10)),
      major_decay_c_term_long = makeDecay(major_c_term,halflife(35)),
      major_decay_c_term_short = makeDecay(major_c_term,halflife(4)),

      major_timesince = nsince(as.numeric(major_c_term == 1|row_number() == 1)) %>% offset(1),
      major_timesince_sq = major_timesince ^ 2,
      major_timesince_cb = major_timesince ^ 3,

      # Rescale time since conflict to fix convergence issue
      major_timesince_norm = major_timesince * 0.0001, #/ max(major_timesince,na.rm = T),
      major_timesince_sq_norm = major_timesince_sq * 0.0001,  #...
      major_timesince_cb_norm = major_timesince_cb * 0.0001,  #...

      # ============================================== 
      # Vertical constraints 
      # ============================================== 
      # Fixing every-other-year variables by sustaining
      # if NA. v2x_elecreg determines whether or not to
      # replace missing values.

      v2elrgstry = fixElvar(v2elrgstry,v2x_elecreg),
      v2elvotbuy = fixElvar(v2elvotbuy,v2x_elecreg),
      v2elirreg = fixElvar(v2elirreg,v2x_elecreg),
      v2elfrfair = fixElvar(v2elfrfair,v2x_elecreg),

      # Do some censoring based on "last active thresh" with
      # lastseen.

      ) %>%
   ungroup()

# ================================================
# Indexes (indices?)
# Ungrouped mutate

dat <- dat %>%
   mutate(

      # ============================================== 
      # Vertical constraints 
      # ============================================== 

      FHKN_frefair_temp = (v2elembaut + v2elembcap + v2elrgstry + v2elvotbuy + v2elirreg + v2elfrfair)/6,
      FHKN_frefair = (FHKN_frefair_temp + 2.533413)/ 6.125325,

      free_fair_narrow = (v2x_suffr * FHKN_frefair * v2x_elecoff),
      free_fair_elections = (v2x_suffr * FHKN_frefair * v2x_elecoff * v2x_frassoc_thick * v2x_freexp_thick),

      combined_vertical = (2*free_fair_elections + v2x_cspart)/3,

      # ============================================== 
      # Horizontal constraints
      # ============================================== 
      FHKN_RoL = (v2clrspct + v2cltrnslw +(v2clprptym + v2clprptyw)/2 )/3,
      FHKN_RoL = normalize(FHKN_RoL),

      horizontal_constraint =  (v2xlg_legcon  + v2x_jucon + FHKN_RoL)/3,
      horizontal_constraint_narrow =  (v2xlg_legcon + v2x_jucon)/2,

      # ============================================== 
      # Polity scores
      # ============================================== 

      xconst = ifelse(xconst == -10, NA, xconst),
      xropen = ifelse(xropen == -10, NA, xropen),
      xrcomp = ifelse(xrcomp == -10, NA, xrcomp),
      x_polity = (xconst + xropen + xrcomp) - 7,
      x_polity_sq = x_polity^2,

      # ============================================== 
      # Categorical model 
      # ============================================== 
      categorical = case_when(

         free_fair_elections > 0.25 & horizontal_constraint > 0.3 ~ 2,

         free_fair_elections > 0.25 & horizontal_constraint <= 0.3 |
         free_fair_elections <= 0.25 & horizontal_constraint > 0.3 ~ 1,

         !is.na(free_fair_elections) & !is.na(horizontal_constraint) ~ 0,

         TRUE ~ as.numeric(NA),
      ),
      
      # ============================================== 
      # Other 
      # ============================================== 
      cowyear = as.numeric(year <= 1945), 

      # ============================================== 
      # Decades and regions 
      # ============================================== 
      # Fixing every-other-year variables by sustaining
      # if NA. v2x_elecreg determines whether or not to
      # replace missing values.

      dec_40 = as.numeric(year >= 1940 & year < 1950),
      dec_50 = as.numeric(year >= 1950 & year < 1960),
      dec_60 = as.numeric(year >= 1960 & year < 1970),
      dec_70 = as.numeric(year >= 1970 & year < 1980),
      dec_80 = as.numeric(year >= 1980 & year < 1990),
      dec_90 = as.numeric(year >= 1990 & year < 2000),
      dec_00 = as.numeric(year >= 2000 & year < 2010),

      # Avoid using all dummies? 
      #dec_10 = as.numeric(year >= 2010 & year < 2020),

      reg_southam = as.numeric(region_name %in% paste(c("Central","South"),"America")),
      reg_ssafrica = as.numeric(region_name %in% paste(c("West","East","Southern"),"Africa")),
      reg_seasia = as.numeric(region_name %in% paste(c("Central","East","South"),"Asia")),
      reg_mena = as.numeric(region_name == "MENA")
   )

# ================================================
# Lagged 
# Grouped 

dat <- dat %>%
   group_by(gwno) %>%
   mutate(
      llnpop200 = offset(lnpop200,1),
      llnGDPPerCapita200 = offset(lnGDPPerCapita200 ,1),
      lfree_fair_elections = offset(free_fair_elections,1),
      lfree_fair_elections_sq = lfree_fair_elections ^ 2,
      lhorizontal_constraint_narrow = offset(horizontal_constraint_narrow, 1),
      lhorizontal_constraint_narrow_sq = lhorizontal_constraint_narrow ^ 2,

      lgrGDPPerCapita200 = offset(grGDPPercapita200,1), 
      llnGDPcap_oilrent = offset(lnGDPcap_oilrent,1),

      # ============================================== 
      # Time since democratic / authoritarian change 
      # ============================================== 
      # Fixing every-other-year variables by sustaining
      # if NA. v2x_elecreg determines whether or not to
      # replace missing values.

      ff_dem = makeChangeDecay(lfree_fair_elections,0.1),
      ff_aut = makeChangeDecay(lfree_fair_elections,-0.1),

      hc_dem = makeChangeDecay(lhorizontal_constraint_narrow,0.1),
      hc_aut = makeChangeDecay(lhorizontal_constraint_narrow,-0.1),

      )

saveRDS(dat,"Cache/prepped_data.rds")
