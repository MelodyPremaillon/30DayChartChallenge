#Day 16 _ relationship _ trees

library(pacman)
pacman::p_load(tidyverse, plotly, cowplot, magick, showtext, extrafont, ggtext, cairo, janitor)

font_add_google("Caveat", "Caveat")
font_add_google("Montserrat", "Montserrat")
showtext_auto()

#Load Data
trees <- read.csv("DATA/trees/us_trees.csv")

trees <- trees %>%
  clean_names() %>%
  mutate(diameter = dbh_cm/100,
         height = tree_ht_m) %>%
  mutate(volume = pi * (diameter/2)^2 * height)

ggplot(trees) + 
  geom_point(aes(volume, volume*350), color= "#808000") +
  geom_point(aes(volume, volume*800), color= "#808000") +
  geom_line(aes(volume, volume*80), color="#6B8E23") +
  geom_line(aes(volume, volume*1400), color="#6B8E23") +
  scale_y_log10(name = "Estimated mass [thousands kg, log]", limits=c(1,1E7)) +
  scale_x_continuous(name= "Estimated volume [m3]") +
  geom_hline(yintercept = c(50E3,150E3,6E6), color=c("#1E90FF","#1E90FF","#556B2F")) +
  theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, family = "Caveat", size =25),
      plot.subtitle = element_text(hjust=0.5, color = "grey40"),
      text = element_text(family = "Montserrat"),
      axis.text = element_text(family = "Montserrat", size=12),
      axis.title = element_text(family = "Montserrat", size=14)
    ) +
  annotate("text",x= -10 , y= 50E3,label="Small blue whales", vjust = 1 , hjust=0, family = "Caveat", size = 5) + 
  annotate("text",x= -10 , y= 150E3,label="Heaviest blue whales 150 T", vjust = 0 , hjust=0, family = "Caveat", size = 5) + 
  annotate("text",x= -10 , y= 6E6,label="Pando, 1600 T : a tree and the heaviest organism on earth (Utah s Fishlake National Forest)  \nIt looks like 47,000 individual trunks but is an interconnected clonal colony, with a massive underground root system connecting its", 
           vjust = 0.5 , hjust=0, family = "Caveat", size=5) +
  labs(title = "What's the heaviest thing on earth: a tree or a whale?",
       subtitle = "Estimated mass vs volume for US trees
Mass is computed for several densities min (Balsa tree) and max (Guaiacum officinale) are green lines
But generaly density ranges between 350 and 800 kg/m3, green dots.",
       caption = "viz Melody Prémaillon | 30DayChartChallenge, day 16 : trees | data : US forest service")

file <- here::here("CHARTS", "16_trees.pdf")

ggsave(file,width=11,height = 8, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 200
)

