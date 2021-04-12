# Day9 - strips

library(pacman)
pacman::p_load(tidyverse, plotly, cowplot, magick, showtext, extrafont, ggtext, lubridate, ragg, httr, jsonlite, ggimage)

font_add_google("Lobster","Lobster")
showtext_auto()

iris <- data.frame(read.csv("DATA/fleurs/iris.csv"), specie="iris")
crocus <- data.frame(read.csv("DATA/fleurs/crocus.csv"), specie="crocus")
galanthus <- data.frame(read.csv("DATA/fleurs/galanthus.csv"), specie="galanthus")
cyclamen <- data.frame(read.csv("DATA/fleurs/cyclamen.csv"), specie="cyclamen")
hyacinthoides <- data.frame(read.csv("DATA/fleurs/hyacinthoides.csv"), specie="hyacinthoides")
narcissus <- data.frame(read.csv("DATA/fleurs/narcissus.csv"), specie="narcissus")

flowers <- rbind(iris, crocus, cyclamen, galanthus, hyacinthoides, narcissus)
rm(iris, crocus, galanthus, cyclamen, hyacinthoides, narcissus)


flowers <- flowers %>%
  mutate(date = as.Date(observed_on)) %>%
  mutate(image = "DATA/fleurs/iris.png",
         x = 8) %>% #iris
  mutate(image = ifelse(specie == "crocus", "DATA/fleurs/crocus.png", image),
         x = ifelse(specie == "crocus", 9, x)) %>%
  mutate(image = ifelse(specie == "galanthus", "DATA/fleurs/galanthus.png", image),
         x = ifelse(specie == "galanthus", 4, x)) %>%
  mutate(image = ifelse(specie == "cyclamen", "DATA/fleurs/cyclamen.png", image),
         x = ifelse(specie == "cyclamen", 7, x)) %>%
  mutate(image = ifelse(specie == "hyacinthoides", "DATA/fleurs/hyacinthoid.png", image),
         x = ifelse(specie == "hyacinthoides", 5, x)) %>%
  mutate(image = ifelse(specie == "narcissus", "DATA/fleurs/narcissus.png", image),
         x = ifelse(specie == "narcissus", 6, x)) %>%
  mutate(date2 = as.Date(paste(2020, strftime(date, format = "%m-%d"), sep="-"))) %>%
  mutate(week = floor_date(date2, "week")) 

mylabels = data.frame(x=c(8,9,4,7,5,6),
                      week = as.Date(c("2020-08-08","2020-08-09","2020-12-15","2020-01-15","2020-07-25","2020-06-06")),
                      label=c("iris", "crocus", "galanthus", "cyclamen", "hyacinthoides", "narcissus"))


plotbkgd <- "#FFF5EE"

ggplot(flowers, aes(week,x)) + geom_image(aes(image=image)) + 
  coord_polar()+
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(breaks = seq(0,9,by=1),limits = c(2,9))+#, labels = c("iris","crocus","galanthus","hyacinthoid","narcissus")) +
  theme(
    plot.background = element_rect(fill=plotbkgd),
    panel.background = element_rect(fill=plotbkgd),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(family = "Lobster",size=12, color="#806450"),
    plot.title = element_text(size=20, hjust = 0.5),
    plot.subtitle = element_text(size=12,color = "#807A77",hjust=0.5),
    plot.caption = element_text(hjust=0.5,size=9, color = "#807A77")
  ) +
  labs(title = "Waltz of the Flowers",
       subtitle = "iNaturalist is an online community where people can record and share observations.
       Here are ploted community observations for different flower species in France during one year",
       caption = "viz Mélody Prémaillon | 30DayChartChallenge day11&12 : circular/strips | data : iNaturalist")+
  geom_text(data=mylabels, aes(week,x, label=label), color="black", size=4, family="Lobster")


file <- here::here("CHARTS", "12_strip.pdf")

ggsave(file,width=7,height = 8.5)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 200
)


