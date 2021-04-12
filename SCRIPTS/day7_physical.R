#day 7 - distributions - physical

library(pacman)
pacman::p_load(tidyverse, plotly, cowplot, magick, showtext, extrafont, ggtext, ggridges, pdftools, here, ragg)

font_add_google("Lobster", "Lobster")


font_add("treasure", regular = "Treamd.ttf")

showtext_auto()

waves <- read.csv("DATA/era_interim_swell/swell.csv") 

waves$X <- as.factor(waves$X)
waves$Y <- as.factor(waves$Y)


lines = data.frame(lat = as.factor(c(-60,-30,0,30,60)), names=c("arctic circle","capricorn","equator","cancer","antarctic circle"))
meslab = data.frame(lat=as.factor(-45),lab="The roaring \n Forties")


ggplot(waves, aes(GRID_CODE, Y)) + 
  geom_density_ridges2(color="#94C4F7", fill="#499DF5", bandwidth=0.125) +
  scale_x_continuous(name = "significant wave height [m]",limits=c(0,5)) +
  scale_y_discrete(name="latitude") +
  theme(
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(),
    axis.ticks.y = element_blank(),
    text = element_text(family = "treasure",size=18),
    plot.title = element_text(color="#990000", hjust=0.5),
    plot.caption = element_text(family = "sans",size=10, hjust=0.5,color="grey60")
  ) +
  geom_segment(data=lines, aes(x=0, xend=5, y=lat, yend=lat), color= "grey30", linetype="dashed") +
  geom_text(data=lines, aes(x=0,y=lat, label=names), family="treasure",size=6,hjust=0, vjust=0) +
  geom_text(data=meslab, aes(x=4.2,y=lat,label=lab), family="treasure", size=6, color="#990000") +
  labs(title = "Wave height across latitude") +
  labs(caption = "viz Mélody Prémaillon | 30DayChartChallenge day7:physical | data : ERAinterim")

file <- here::here("CHARTS", "07_physical.pdf")

#ggsave(file,width = 6,height=12, device=cairo_pdf)
ggsave(file, width = 6, height = 12)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 200
)
