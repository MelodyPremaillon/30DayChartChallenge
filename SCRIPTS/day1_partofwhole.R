#Day 1

library(pacman)
pacman::p_load(tidyverse, plotly, cowplot, magick, showtext, extrafont, ggtext)

font_add("african",
         regular = "african.ttf")

showtext_auto()
font_add_google("Josefin Sans","Josefin Sans")

imdb <- read.csv("DATA/IMDB/IMDb movies.csv")

dinos <- imdb  %>% 
  filter(grepl('Dinosaurs|dinosaurs|dinosaur|Dinosaur|dino', description) | grepl('Jurassic Park', title)) 

dinos$original_title <- as.character(dinos$original_title)
dinos$year <- as.numeric(levels(dinos$year))[dinos$year]

jurassicParklicence <- dinos  %>% 
  filter(grepl('Jurassic Park', title)) %>%
  pull(imdb_title_id)
jurassicWorldlicence <- dinos  %>% 
  filter(grepl('Jurassic World', title)) %>%
  pull(imdb_title_id)


dinos$licence <- "other"
dinos <- dinos %>% 
  mutate(licence = ifelse(imdb_title_id %in% jurassicParklicence, "Jurassic Park", licence)) %>%
  mutate(licence = ifelse(imdb_title_id %in% jurassicWorldlicence, "Jurassic World", licence))


sumtotvotes = sum(dinos$votes)  

prctdinos <- dinos %>%
  mutate(original_title = ifelse(licence == "other", "other dinosaur's movies", original_title)) %>%
  group_by(original_title) %>%
  summarise(prctvotes = sum(votes)/sumtotvotes*100)  %>%
  left_join(dinos, by="original_title") %>%
  arrange(year) %>%
  mutate(ymax = cumsum(prctvotes), 
         ymin= c(0, head(ymax, n=-1)), 
         labelpos=(ymax + ymin) / 2) 



  

myplot <- ggplot(prctdinos, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=original_title)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  scale_x_continuous(limits = c(2,4)) +
  scale_y_continuous(labels=c("100%","25%","50%","75%","0%"))+
  scale_fill_manual(values=c("#FF0000", #jp
                             "#802626", #jpII
                             "#024959", #jw
                             "#012840", #jwII
                             "black", #other
                             "#CC0000")) + #jpII+
  geom_text( x=3.5, aes(y=labelpos, label=original_title), size=4.5, color="white",  family = "Josefin Sans") +
  #theme
  theme(
    plot.background = element_rect(fill="black", color="black"),
    panel.background = element_rect(fill="black", color="black"),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    text = element_text(size=17, color="white", hjust=0.5, vjust=1, family="Josefin Sans"),
    plot.title = element_markdown()
  ) +
  labs(title = 'Ah, now eventually you do plan to have <br> <b style="font-size:17pt;font-family:african;">Jurassic</b>   <b style="font-size:17pt;font-family:african;">Park</b> <br> on your dinosaurs movies?') 

ggdraw() +
  draw_text("Proportion of users votes for dinosaurs' movies on IMDB dataset",
            size = 12, x = 0.5, y = 1, hjust = 0.5,
            color = "white", family="Josefin Sans") -> subtitle

ggdraw() +
  draw_text("Visualization: Melody Premaillon | #30dayChartChallenge| 01-Part-to-Whole | Data: IMDB",
            size = 10, x = 0.98, y = 0.5, hjust = 1,
            color = "white") -> caption


finalplot <- plot_grid(myplot,
                       subtitle,
                       caption,
                       ncol=1,
                       rel_heights = c(4,0.2,0.2)) 


finalplot + theme(plot.background = element_rect(fill= "black", color="black"))
  

#image_graph(500,700,res=300, bg = "black")
#finalplot

ggsave("CHARTS/jurassicplot.pdf", width=7, height = 9, device = cairo_pdf)
dev.off()
