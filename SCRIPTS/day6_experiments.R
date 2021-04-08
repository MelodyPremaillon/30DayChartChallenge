#Day 1

library(pacman)
pacman::p_load(tidyverse, plotly, cowplot, magick, showtext, extrafont, ggtext, rtweet, tm, wordcloud)

tweets <- search_tweets("#30daychartchallenge", n = 4000, fromDate = "202104040001", include_rts = F)

magic_tweets <- tweets  %>% 
  filter(grepl('magic|MAgic|Day 4|day4| day 4', text)) 

HP_tweets <- magic_tweets  %>% 
  mutate(text = tolower(text)) %>%
  filter(grepl('harry|potter|harrypotter', text)) 

other_tweets <- magic_tweets  %>% 
  mutate(text = tolower(text)) %>%
  filter(!grepl('harry|potter|harrypotter', text)) 


sum(HP_tweets$retweet_count)
sum(other_tweets$retweet_count)

18/88
344/(344+73)



