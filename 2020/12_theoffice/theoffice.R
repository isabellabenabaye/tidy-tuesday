# date: March 27-28, 2020
library(schrute)
library(tidyverse)
library(splitstackshape) ## for cSplit()
library(treemapify) ## for geom_treemap()
library(extrafont)

loadfonts(device = "win", quiet = TRUE) ## to load the font

# Import the transcripts
office_transcript <- schrute::theoffice  ## 55130 lines
office_transcript <- office_transcript %>%
  mutate(season = as.numeric(season),
         episode = as.numeric(if_else(episode_name == "Finale", "23", episode)), ## match finale episode number to the one in office_ratings
         )

# Import the IMDb ratings from data.world
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
## 188 episodes

# Join tables to get episode summary
office_episodes <- office_ratings %>%
  left_join(office_transcript, 
            by = c("season" = "season", "episode" = "episode"),
            suffix = c("", "2")) %>% 
  select(-c(character, text, text_w_direction, index)) %>% 
  unique()

# Set theme
theme_set(theme_gray())
theme <- theme_update(text = element_text(family = "American Typewriter", size = 13),
                      title = element_text("American Typewriter", size = 20, color = "gray20"),
                      plot.title = element_text("American Typewriter", size = 30, color = "gray20"),
                      axis.text = element_text(size = 16))


# ‘The Office’ IMDb Ratings
## WIP: need to make annotations more efficient
office_episodes %>% 
  mutate(season = as.factor(season)) %>% 
  ggplot(aes(x = season, y = imdb_rating, fill = season)) +
  geom_violin(show.legend = FALSE,
              draw_quantiles = TRUE,
              color = "gray90") +
  labs(title = "‘The Office’ IMDb Ratings", x = "Season", y = "IMDb Rating") + 
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(6,10))  +
  theme(axis.title.y = element_text(family = "Impact"),
        axis.title.x = element_text(size = 25)) +
  annotate(geom = "label", x = 1, y = 7.42, size = 5, ## label
           label = "Pilot:\nDiversity Day", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#9E0142",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 2, y = 9.4, size = 5, ## label
           label = "Casino Night", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#F46D43",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 3, y = 9.4, size = 5, ## label
           label = "The Job", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#FDAE61",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 4, y = 9.4, size = 5, ## label
           label = "Dinner Party", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#FDAE61",
           #label.size = NA,
           fill = "white")  +
  annotate(geom = "label", x = 5, y = 9.7, size = 5, ## label
           label = "Stress Relief", 
           family = "American Typewriter",
           #fontface = "bold",
           hjust = "center",
           lineheight = 0.9,
           color = "goldenrod2",
           #label.size = NA,
           fill = "white")  +
  annotate(geom = "label", x = 6, y = 9.4, size = 5, ## label
           label = "Niagara", 
           family = "American Typewriter",
           #fontface = "bold",
           hjust = "center",
           lineheight = 0.9,
           color = "darkolivegreen3",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 7, y = 9.8, size = 5, ## label
           label = "Goodbye, Michael", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#66C2A5",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 8, y = 6.6, size = 5, ## label
           label = "Get the Girl", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#66C2A5",
           #label.size = NA,
           fill = "white")  +
  annotate(geom = "label", x = 9, y = 9.8, size = 5, ## label
           label = "Finale", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#3288BD",
           #label.size = NA,
           fill = "white")

ggsave(here::here("2020", "12_theoffice", "The Office IMDb Ratings.png"), device = "png", width = 12, height = 8, dpi = 300)


# ‘The Office’ Writers
## list of episodes with a column for each writer: writer_1, writer_2, writer_3
## WIP: make the binding of the list of writers & their ratings more efficient
writers <- office_episodes %>%
  select(season, episode, episode_name, writer, imdb_rating) %>% 
  cSplit("writer", sep = ";") 

w_1 <- writers %>% 
  filter(!is.na(writer_1)) %>% ## take out obs where the writer is NA
  select(season, episode, writer_1, imdb_rating) %>% 
  rename(writer = writer_1)

w_2 <- writers %>% 
  filter(!is.na(writer_2)) %>% 
  select(season, episode, writer_2, imdb_rating) %>% 
  rename(writer = writer_2)

w_3 <- writers %>% 
  filter(!is.na(writer_3)) %>% 
  select(season, episode, writer_3, imdb_rating) %>% 
  rename(writer = writer_3)

writer_ratings_all <- rbind(w_1, w_2, w_3) ## bind to make the list of all writers and their ratings per episode

writer_ratings_counts <- writer_ratings_all %>%  ## summary of each writer's average rating and # of episodes they wrote
  group_by(writer) %>% 
  summarise(avg_value = mean(imdb_rating), count = n()) %>% 
  arrange(desc(count))

## treemap of the writers
writer_ratings_counts %>% 
  ggplot(aes(area = count, fill = count, label = writer, family = "Source Sans Pro Light")) +
  geom_treemap(start = "topleft") +
  geom_treemap_text(fontface = "italic",
                    size = 20,
                    colour = "white", 
                    place = "topleft",
                    reflow = TRUE,
                    start = "topleft") +
  ggtitle("‘The Office’ Writers") +
  scale_fill_gradient(name = "Number of\nepisodes", low = "#2980b9", high = "brown3", na.value = NA)

ggsave(here::here("2020", "12_theoffice", "The Office Writers.png"), device = "png", width = 12, height = 8, dpi = 300)
