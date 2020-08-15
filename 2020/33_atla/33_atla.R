# This plot is a remake of Jack Davison's ATLA #TidyTuesday viz - https://twitter.com/JDavison_/status/1292896713003409410?s=20

# Code heavily influenced by:
# Jack Davison's ATLA #TidyTuesday viz - https://github.com/jack-davison/TidyTuesday/blob/master/R/2020_08_11_Avatar.R
# Cedric Scherer - The Office Ratings by IMDb & data.world - https://github.com/Z3tt/TidyTuesday/blob/master/R/2020_12_TheOffice.Rmd

library(tidyverse)
library(ib)
extrafont::loadfonts(device = "win", quiet = TRUE) ## to load the fonts

# Load data ----
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

# Prepare the data -----
avatar_episodes <- avatar %>% 
  distinct(book, book_num, chapter, chapter_num, imdb_rating) %>% 
  mutate(episode_num = row_number(),
         imdb_rating = if_else(episode_num == 20, 9.7, imdb_rating)) %>%  # fill in missing value - rating taken from IMDB on 8/14/2020
  group_by(book) %>% 
  mutate(book_avg_rating = mean(imdb_rating))

# for the average IMDB rating per book line - geom_line
book_avg <- avatar_episodes %>% 
  summarize(start_x = min(episode_num) - 0.1,
         end_x = max(episode_num) + 0.1,
         y = unique(book_avg_rating)) %>% 
  pivot_longer(cols = c(start_x, end_x),
               names_to = "type",
               values_to = "x")

# for the book labels - geom_text
book_labels <- avatar_episodes %>% 
  summarise(x = min(episode_num),
            y = unique(book_avg_rating))

# Plots -----

# Avatar logo
logo <- png::readPNG('2020/33_atla/atla_logo.png')
logo <-  grid::rasterGrob(logo, interpolate=TRUE)

# Theme
theme_set(theme_ib(title_family = "Herculanum", text_family = "Herculanum", bg_color = "#ECE5D3")) +
  theme_update(axis.title.x = element_blank(),
               axis.title.y = element_text(hjust = 0.5),
               axis.line = element_blank(),
               axis.text.x = element_blank(),
               plot.caption = element_text(family = "IBM Plex Sans Light"),
               legend.position = "none",
               panel.grid.minor = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(color = "#d4cebd", size = 0.3)
  )

update_geom_fonts_ib(family = "Herculanum")

avatar_episodes %>% 
  ggplot(aes(episode_num, imdb_rating, color = book)) +
  labs(y = "IMDB Rating", caption = "Data: {appa} by Avery Robbins | @_isabellamb") +
  geom_text(data = book_labels,
            aes(x = x, y = y, label = book),
            size = 22, alpha = 0.4, vjust = 0, hjust = 0) +
  geom_segment(aes(xend = episode_num, yend = book_avg_rating, color = book), 
               size = 1, alpha = 0.2) +
  geom_line(data = book_avg,
            aes(x, y), size = 1.5) +
  geom_point(size = 2) +
  annotation_custom(grob = logo, xmin = 18, xmax = 60, ymin = 6.7, ymax = 8.1) +
  annotate("text", x = 38.2, y = 7.875, label = "All episodes of", size = 8, hjust = 0.5) +
  scale_color_manual(values = c("Water" = "#075897", "Earth" = "#096B42", "Fire" = "#B03E1E")) +
  scale_y_continuous(limits = c(7,10)) +
  ggsave(here::here("2020", "33_atla", "avatar_episodes.png"), device = "png", type = "cairo", width = 12, height = 8, dpi = 300)
  
  
  
