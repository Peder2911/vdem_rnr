## GAM analysis, V-Dem civil peace article
## HH 07-12-2017
## Peder Landsver 2020 revision

# Libraries
library(mgcv) 
library(ggplot2)
library(ggrepel)
library(readstata13)

results_loc <- "Out"

cydata <- read.csv("InputData/FVP_V-Demcivpeace.csv", header=TRUE, sep=",")
FVPdata <- read.dta13("InputData/GAMData.dta")

civpeacedata <- subset(cydata, year <= 2016)
FVPdata <- subset(FVPdata, year <= 2016)

writeLines("ModelMT1") 
ModelMT1 <- glm(c_onset ~ horizontal_constraint + free_fair_elections + decay_2 + 
                   lnpop200 + lnGDPPerCapita200 + minority_rep + cowyear, 
                 family="binomial", data=civpeacedata)

writeLines("ModelInt1") 
ModelInt1 <- glm(c_onset ~ horizontal_constraint + free_fair_elections + vert_hor_int + decay_2 + 
                   lnpop200 + lnGDPPerCapita200 + minority_rep + cowyear, 
                 family="binomial", data=civpeacedata)

writeLines("ModelGAM1") 
ModelGAM1 <- gam(c_onset ~ s(horizontal_constraint) + s(free_fair_elections) + decay_2 + 
                   lnpop200 + lnGDPPerCapita200 + minority_rep + cowyear, 
                 family="binomial", data=civpeacedata)

#writeLines("ModelGAM2") 
#ModelGAM2 <- gam(c_onset ~ s(horizontal_constraint, free_fair_elections) + decay_2 + 
#                     lnpop200 + lnGDPPerCapita200 + minority_rep + cowyear, 
#                   family="binomial", data=civpeacedata)

#writeLines("ModelGAM2") 
#ModelGAM2 <- gam(c2_onset ~ s(lfree_fair_elections,lhorizontal_constraint_narrow) +
#                   lfree_fair_elections*lhorizontal_constraint_narrow + 
#                   decay_c2 + llnpop200 + llnGDPPerCapita200, 
#                 family="binomial", data=FVPdata)

writeLines("ModelGAM2") 
ModelGAM2 <- gam(c2_onset ~ s(lhorizontal_constraint_narrow,lfree_fair_elections) +
                   decay_c2 + llnpop200 + llnGDPPerCapita200, 
                 family="binomial", data=FVPdata)

pdf(paste0(results_loc,"/GAM_verthor_perspective.pdf"), width=10, height=8)
plot(ModelGAM2, scheme=1,rug=TRUE,
     main="",
     xlab="Horizontal constraints",
     ylab="Vertical constraints")
dev.off()

pdf(paste0(results_loc,"/GAM_verthor.pdf"), width=10, height=8)
plot(ModelGAM2, scheme=2,rug=TRUE,
     hcolors=heat.colors(50),
     main="",
     xlab="Horizontal constraints",
     ylab="Vertical constraints")
dev.off()

#plot(ModelGAM2, scheme=2,hcolors=rainbow(50, start=.4))

#vis.gam(ModelGAM2, view=c("horizontal_constraint","free_fair_elections"), plot.type="contour", color="heat")

y2015data <- subset(civpeacedata, year==2015)

y2015data$casecnt <- ifelse(y2015data$gwno == 690 | y2015data$gwno == 731 | y2015data$gwno == 380 | 
                              y2015data$gwno == 130 | y2015data$gwno == 2 | y2015data$gwno == 200 | 
                              y2015data$gwno == 220 | y2015data$gwno == 140 | y2015data$gwno == 750 | 
                              y2015data$gwno == 365 | y2015data$gwno == 475 | y2015data$gwno == 501 | 
                              y2015data$gwno == 100 | y2015data$gwno == 640 | y2015data$gwno == 630 | 
                              y2015data$gwno == 600 | y2015data$gwno == 42 | y2015data$gwno == 339 | 
                              y2015data$gwno == 101 | y2015data$gwno == 344 | y2015data$gwno == 850 | 
                              y2015data$gwno == 840 | y2015data$gwno == 530 | y2015data$gwno == 770 |
                              y2015data$gwno == 355 | y2015data$gwno == 616 | y2015data$gwno == 500 | 
                              y2015data$gwno == 830 | y2015data$gwno == 93 | y2015data$gwno == 110  | 
                              y2015data$gwno == 260 | y2015data$gwno == 160 | y2015data$gwno == 771  | 
                              y2015data$gwno == 540 | y2015data$gwno == 775 | y2015data$gwno == 615  | 
                              y2015data$gwno == 91 | y2015data$gwno == 620 | y2015data$gwno == 710  | 
                              y2015data$gwno == 531 | y2015data$gwno == 652 | y2015data$gwno == 370 | 
                              y2015data$gwno == 366 | y2015data$gwno == 390 | y2015data$gwno == 663 
                               , 1, 0)


vert_hor_desc_plot <- ggplot(y2015data, aes(horizontal_constraint, free_fair_elections, label=gwno)) +
  geom_point(na.rm=TRUE) +
  guides(size=FALSE) +
  geom_text_repel(aes(label=ifelse(casecnt> 0,as.character(gwno),'')), size=3) + theme_bw()

ggsave(file.path(results_loc,"vert_hor_desc_plot.png"), vert_hor_desc_plot, width=16, height=9, units = "cm")
