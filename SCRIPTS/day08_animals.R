 # day8 animals

library(pacman)
pacman::p_load(tidyverse, plotly, cowplot, magick, showtext, extrafont, ggtext, lubridate, ragg, gghalves,openxlsx)

orni <- read.csv("DATA/dinos/ornitichia.csv", skip = 15)
orni$order <- "ornitischia"
sauri <- read.csv("DATA/dinos/saurischia.csv", skip = 15)
sauri$order <- "saurischia"
dinos <- rbind(orni,sauri)

dinos$era <- "Cretaceous"
dinos <- dinos %>%
  mutate(age_jitter = max_ma + (max_ma-min_ma)/2) %>%
  mutate(era = ifelse(age_jitter > 145.5 , "Jurassic", era)) %>%
  mutate(era = ifelse(age_jitter > 199.6, "Trias", era)) %>%
  filter(age_jitter < 252)

dinos$age_jitter <- dinos$age_jitter + runif(nrow(dinos),0,1)

mylab <- data.frame(x=230,y=1500,order=c("ornitischia", "saurischia"))
myvlines <- data.frame(x=c(201,145))
myera <- data.frame(x=c(235, 175,115), era= c("Trias", "Jurassic","Cretaceous"))

ggplot(dinos, aes(age_jitter)) +
  geom_histogram(aes(fill=era),binwidth = 2)+
  scale_x_continuous(trans = "reverse",breaks = seq(0,300,by=10),name="",limits = c(252,66)) +
  scale_y_continuous(name="",limits=c(0,1800))+
  stat_ecdf(geom = "step") +
  facet_grid(order~.) +
  scale_fill_manual(values=c("#88BD39","#38AFCA","#832383"))+
  theme(
    legend.position = "none",
    panel.background = element_rect(fill="white"),
    plot.background = element_rect(fill="white"),
    panel.grid.major.y  = element_line(color="lightgrey"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  geom_text(data=mylab, aes(x, y, label=order)) +
  labs(title = "Explosion of dinosaurs diversity during mesosoic",
  subtitle = " Dinosaurs are divided in two orders : ornitischian and saurischian, they evolved during mesosoic era (between 252 and 66 Myrs). 
  The graph represents the number of fossiles described by palenonthologist across the geological times for the two orders.
  Species needs time to rich a large diversity and conqueer different ecological niches.") +
  geom_vline(data=myvlines, aes(xintercept=x)) 

+
  geom_label(data=myera, aes(x,y=1000, label=era))



ggsave("CHARTS/dinohist.pdf",width=10,height=7)
