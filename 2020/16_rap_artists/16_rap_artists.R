#date: April 18, 2020

library(tidyverse)
#library(RColorBrewer)
library(extrafont)
library(hexbin)
fonttable <- fonttable()
#font_import(paths = c("c:/Users/isabe/FontBase"))
loadfonts(device = "win", quiet = TRUE) ## to load the font

# Set theme

theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Encode Sans Thin", size = 13),
                      title = element_text(family = "Fira Mono", size = 20, color = "gray20"),
                      plot.title = element_text(family = "CHANEY Ultra Extended", size = 20, color = "gray20"),
                      plot.subtitle = element_text(family = "Gruppo", size = 25, color = "gray20"),
                      legend.text = element_text("Gruppo", size = 16),
                      axis.text = element_text(size = 14),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80"))

# Get the Data

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Scatter plot of all of the songs and their total points
rankings %>% 
  ggplot(aes(x = year, y = points)) +
  geom_point(aes(color = gender),size = 5, alpha = 0.3) +
  labs(title = "Top Hip Hop Songs",
       subtitle = "According to 107 critics worldwide",
       x = "",
       y = "Points",
       color = "") +
  scale_color_manual(values = c("#f35588","#05dfd7","#ffac41"),
                     labels = c("female rapper", "male rapper","mixed")) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(here::here("2020", "16_rap_artists", "All songs.png"), device = "png", width = 8, height = 6, dpi = 300)

# Plot of critic origins
polls %>% 
  distinct(critic_name,critic_country) %>% 
  count(critic_country) %>% 
  ggplot(aes(x = reorder(critic_country,n), y = n)) +
  geom_bar(stat = "identity", fill = "#4cd3c2") +
  labs(title = "But how biased is this opinon?",
       subtitle = "Most of the critics are from the US",
       x = "",
       y = "",
       color = "") + 
  scale_y_continuous(expand = expansion(c(0,.05))) +
  theme(axis.text.x = element_text(family = "Fira Mono",size = 12)) + coord_flip()

ggsave(here::here("2020", "16_rap_artists", "Critics - countries.png"), device = "png", width = 11, height = 4, dpi = 300)