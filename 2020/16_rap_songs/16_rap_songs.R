#date: April 18, 2020 / April 21, 2020

library(tidyverse)
library(ggrepel)
library(extrafont)
library(Cairo)
library(gganimate)

fonttable <- fonttable()
font_import(paths = c("C:/Users/isabe/FontBase/2020-04-08/CHANEY-ULTRA-EXTENDED/CHANEY-ULTRA-EXTENDED/Webfont/CHANEY-UltraExtended-webfont"))
loadfonts(device = "win", quiet = TRUE) ## to load the font

# Set theme-----

theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Source Sans Pro Light", size = 13),
                      title = element_text(family = "Fira Mono", size = 20, color = "gray20"),
                      plot.title = element_text(family = "CHANEY Ultra Extended", size = 20, color = "gray20"),
                      plot.subtitle = element_text(family = "Gruppo", size = 25, color = "gray20"),
                      plot.caption = element_text(family = "Source Sans Pro Light", size = 13, color = "gray20"),
                      legend.text = element_text("Gruppo", size = 16),
                      axis.text = element_text(size = 14),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80"))

# Get the Data-----

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Scatter plot of all of the songs and their total points - with artists-----

# get the labels
top <- rankings %>% 
  head(6)

top_females <- rankings %>%
  filter(gender  == "female") %>% 
  head(2)

notable <- rankings %>% 
  filter(year >= 2010 | year == min(year)) %>% 
  head(3)

labels <- rbind(top,notable) %>% 
  mutate(label = paste0(title," by ",artist," (",year,")"))

rankings %>% 
  ggplot(aes(x = year, y = points)) +
  geom_point(aes(color = gender),size = 4, alpha = 0.3) +
  geom_point(data = top_females, aes(x = year, y = points), color = "#f35588",size = 4, alpha = 0.3) +
  labs(title = "Top Hip Hop Songs",
       subtitle = "According to 107 critics worldwide",
       caption = "Critic rating: Each critic voted for five songs, ranking them from one (favourite) to five (5th favourite). BBC Music awarded 10 points for first ranked track,\neight points for second ranked track, and so on down to two points for fifth place.The song with the most points won.\nSource: BBC Music (2019)",
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
                   family = "Source Sans Pro Light",
                   size = 4.5,
                   nudge_y = 5.5,
                   color = "#3FDAC4",
                   nudge_x = ,
                   box.padding = 0.25,
                   segment.alpha = 0,
                   label.size = .25)

ggsave(here::here("2020", "16_rap_songs", "All songs - artists.png"), device = "png", type = "cairo", width = 12, height = 10, dpi = 300)

# Scatter plot of all of the songs and their total points - no artists-----

# get the labels

rankings %>% 
  ggplot(aes(x = year, y = points)) +
  geom_point(aes(color = gender),size = 4.5, alpha = 0.3) +
  geom_point(data = top_females, aes(x = year, y = points), color = "#f35588",size = 4, alpha = 0.3) +
  labs(title = "Top Hip Hop Songs",
       subtitle = "According to 107 critics worldwide",
       x = "",
       y = "Critic Rating",
       color = "",
       caption = "Critic rating: Each critic voted for five songs, ranking them from one (favourite) to five (5th favourite). BBC Music awarded 10 points for first ranked track,\neight points for second ranked track, and so on down to two points for fifth place.The song with the most points won.\nSource: BBC Music (2019)") +
  scale_color_manual(values = c("#f35588","#05dfd7","#ffac41"),
                     labels = c("female rapper", "male rapper","mixed")) +
  theme(
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 35),
    plot.caption = element_text(size = 11),
    legend.position = c(.9, .9),
    legend.justification = c("right", "top"),
    legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_label_repel(data = labels, 
                   aes(label = title),
                   family = "Source Sans Pro Light",
                   size = 4.5,
                   nudge_y = 5.5,
                   color = "#3FDAC4",
                   nudge_x = ,
                   box.padding = 0.25,
                   segment.alpha = 0,
                   label.size = .25)  +
  geom_label_repel(data = top_females, 
                   aes(label = title),
                   family = "Source Sans Pro Light",
                   #fontface = "bold",
                   size = 4.5,
                   nudge_y = 5.5,
                   color = "#f35588",
                   box.padding = 0.2,
                   segment.alpha = 0,
                   label.size = .25) # remove border

ggsave(here::here("2020", "16_rap_songs", "All songs.png"), device = "png", type = "cairo", width = 10, height = 9, dpi = 300)

# Plot of critic origins-----
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

# Animated plot (April 23, 2020)-----

songs_animated <- rankings %>% 
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
    plot.title = element_text(size = 28,),
    plot.subtitle = element_text(size = 32),
    plot.caption = element_text(size = 16),
    legend.position = c(.85, .92),
    legend.justification = c("right", "top"),
    legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text("Gruppo", size = 22)
  ) +
  transition_states(gender,
                    transition_length = 1,
                    state_length = 2) +
  enter_fade() + 
  exit_shrink()
animate(songs_animated, width = 800, height = 600)
anim_save("All songs - animated.gif")
