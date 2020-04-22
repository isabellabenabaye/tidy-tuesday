#date: April 18, 2020 / April 21, 2020

library(tidyverse)
library(ggrepel)
library(extrafont)
library(Cairo)

fonttable <- fonttable()
#font_import(paths = c("c:/Users/isabe/FontBase"))
loadfonts(device = "win", quiet = TRUE) ## to load the font

#----------------------------------------------------------------------------------------------
# Set theme

theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Encode Sans Thin", size = 13),
                      title = element_text(family = "Fira Mono", size = 20, color = "gray20"),
                      plot.title = element_text(family = "CHANEY Ultra Extended", size = 20, color = "gray20"),
                      plot.subtitle = element_text(family = "Gruppo", size = 25, color = "gray20"),
                      plot.caption = element_text(family = "Encode Sans Thin", size = 12, color = "gray20"),
                      legend.text = element_text("Gruppo", size = 16),
                      axis.text = element_text(size = 14),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80"))

#----------------------------------------------------------------------------------------------
# Get the Data

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

#----------------------------------------------------------------------------------------------
# Scatter plot of all of the songs and their total points - with artists

# get the labels
top <- rankings %>% 
  head(6)

notable <- rankings %>% 
  filter(year >= 2010 | year == min(year)) %>% 
  head(3)

labels <- rbind(top,notable) %>% 
  mutate(label = paste0(title," by ",artist," (",year,")"))

rankings %>% 
  ggplot(aes(x = year, y = points)) +
  geom_point(aes(color = gender),size = 4, alpha = 0.3) +
  labs(title = "Top Hip Hop Songs",
       subtitle = "According to 107 critics worldwide",
       caption = "Critic rating: Each critic voted for five songs, ranking them from one (favourite) to five (5th favourite).\nBBC Music awarded 10 points for first ranked track, eight points for second ranked track, \nand so on down to two points for fifth place.The song with the most points won.\nSource: BBC Music (2019)",
       x = "",
       y = "Critic Rating",
       color = ""
       ) +
  scale_color_manual(values = c("#f35588","#05dfd7","#ffac41"),
                     labels = c("female rapper", "male rapper","mixed")) +
  theme(
    plot.title = element_text(size = 30,),
    plot.subtitle = element_text(size = 35),
    legend.position = c(.85, .9),
    legend.justification = c("right", "top"),
    legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_label_repel(data = labels, 
                   aes(label = label),
                   family = "Encode Sans Thin",
                   size = 4,
                   nudge_y = 5,
                   #nudge_x = 5,
                   box.padding = 0.25,
                   segment.alpha = 0,
                   label.size = NA) # remove border

ggsave(here::here("2020", "16_rap_songs", "All songs - artists.png"), device = "png", type = "cairo", width = 12, height = 10, dpi = 300)

#----------------------------------------------------------------------------------------------
# Scatter plot of all of the songs and their total points - no artists

# get the labels
top <- rankings %>% 
  head(6)

notable <- rankings %>% 
  filter(year >= 2010 | year == min(year)) %>% 
  head(3)

labels <- rbind(top,notable)

rankings %>% 
  ggplot(aes(x = year, y = points)) +
  geom_point(aes(color = gender),size = 4.5, alpha = 0.3) +
  labs(title = "Top Hip Hop Songs",
       subtitle = "According to 107 critics worldwide",
       x = "",
       y = "Critic Rating",
       color = "",
       caption = "Critic rating: Each critic voted for five songs, ranking them from one (favourite) to five (5th favourite).\nBBC Music awarded 10 points for first ranked track, eight points for second ranked track, \nand so on down to two points for fifth place.The song with the most points won.\nSource: BBC Music (2019)") +
  scale_color_manual(values = c("#f35588","#05dfd7","#ffac41"),
                     labels = c("female rapper", "male rapper","mixed")) +
  theme(
    plot.title = element_text(size = 30,),
    plot.subtitle = element_text(size = 35),
    legend.position = c(.9, .9),
    legend.justification = c("right", "top"),
    legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_text_repel(data = labels, 
                   aes(label = title),
                   family = "Encode Sans Thin",
                   size = 4.5,
                   nudge_y = 5,
                   nudge_x = ,
                   box.padding = 0.25,
                   segment.alpha = 0)

ggsave(here::here("2020", "16_rap_songs", "All songs.png"), device = "png", type = "cairo", width = 10, height = 9, dpi = 300)

#----------------------------------------------------------------------------------------------
# Plot of critic origins
polls %>% 
  distinct(critic_name,critic_country) %>% 
  count(critic_country) %>% 
  ggplot(aes(x = reorder(critic_country,n), y = n)) +
  geom_bar(stat = "identity", fill = "#baf1a1") +
  labs(title = "But how biased is this opinon?",
       subtitle = "Most of the critics are from the US",
       x = "",
       y = "",
       color = "") + 
  scale_y_continuous(expand = expansion(c(0,.05))) +
  theme(axis.text.x = element_text(family = "Fira Mono",size = 12)) + coord_flip()

ggsave(here::here("2020", "16_rap_songs", "Critics - countries.png"), device = "png", type = "cairo", width = 11, height = 4, dpi = 300)
