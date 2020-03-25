
library(ggplot2)
library(ggrepel)
library(dplyr)

dat <- readRDS("Cache/prepped_data.rds") %>%
   filter(year == 2015)

include <- c(690,731,380, 130,2,200, 220,140,750, 365,475,501, 100,640,630, 
             600,42,339, 101,344,850, 840,530,770, 355,616,500, 830,93,110,
             260,160,771, 540,775,615, 91,620,710, 531,652,370, 366,390,663)
dat$include <- sapply(dat$gwno,function(no) no %in% include)

plt <- ggplot(dat,aes(x=horizontal_constraint,y=free_fair_elections))+
   geom_point() +
   geom_text_repel(aes(label=ifelse(include,as.character(gwno),""))) +
   labs(x="HCI",y="VCI") +
   theme_bw()

ggsave("Out/indices_2015.pdf",plt,device="pdf",height=5,width=9)

