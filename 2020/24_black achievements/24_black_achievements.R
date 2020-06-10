library(tidyverse)
library(ggbeeswarm)
library(extrafont)
fonttable <- fonttable()
loadfonts(device = "win", quiet = TRUE) ## to load the font


# Get the Data ------
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

# Clean up data ------
## make gender and category factors
firsts <- firsts %>% 
  mutate_at(c("gender", "category"), as.factor) %>% 
  mutate(decade = year - year %% 10)

summary(firsts)

# Plot ------
# Theme
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Fira Code", size = 13, color = "#F5F5F5"),
                      title = element_text("IBM Plex Mono SemiBold", size = 24, color = "#F5F5F5"),
                      plot.title = element_text("IBM Plex Mono SemiBold", face = "bold", size = 32, color = "#F5F5F5"),
                      plot.title.position = "plot",
                      plot.subtitle = element_text("IBM Plex Mono SemiBold", face = "bold", size = 24, color = "#F5F5F5"),
                      plot.caption = element_text(size = 14),
                      axis.text = element_text(size = 14, color = "#F5F5F5"),
                      #axis.text.x = element_blank(),
                      axis.line.x = element_line(color = "#F5F5F5"),
                      axis.line.y = element_line(color = "#F5F5F5"),
                      #axis.line.x = element_blank(),
                      #axis.line.y = element_line(color = "gray80"),
                      #panel.grid = element_blank(),
                      legend.text = element_text(size = 21, family = "IBM Plex Mono Medium"),
                      legend.title = element_blank(),
                      legend.background = element_rect(fill = "#525252", color = "#525252"),
                      plot.margin = margin(20, 20, 20, 20),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      panel.grid.major.y = element_line(color = "gray70"),
                      panel.grid.minor.y = element_blank(),
                      plot.background = element_rect(fill = "#525252", color = "#525252"),
                      legend.position = c(.2, .785))

# Count of firsts
firsts %>% 
  ggplot(aes(decade, fill = category)) +
  geom_rect(mapping = aes(xmin = 1945, xmax = 1965, ymin = 0, ymax = 64.3), fill = NA, color = "#C2C2C2", linetype = 3) + ## dotted outline of 50s-60s
  geom_bar() +
  labs(title = "First Achievements by Black Americans Throughout History", 
       x = "", y = "Number of Achievements",
       caption = "Data: Wikipedia | Plot: @_isabellamb") +
  
  scale_y_continuous(expand = expansion(0,0)) +
  scale_x_continuous(breaks = seq(from = 1730, to =2010, by = 10), labels = paste0(seq(1730, 2010, by = 10), "s"), expand = expansion(0,0)) +
  
  geom_curve(aes(x = 1739, y = 13, xend = 1730, yend = 1.5), size = 0.5,  ## 1738 arrow
             arrow = arrow(length = unit(0.2, "inch")), 
             curvature = 0.4, color = "#F5F5F5") +
  geom_curve(aes(x = 1915, y = 53, xend = 1942, yend = 58.5), size = 0.5, ## civil rights movement arrow
             arrow = arrow(length = unit(0.2, "inch")), 
             curvature = -0.3, color = "#F5F5F5") +
  geom_rect(mapping = aes(xmin = 1969, xmax = 2001, ymin = 55, ymax = 62), fill = "#525252", color = "#525252") +  ## background of the post civil rights annotation
  annotate("text", x = 1740, y = 13, label = "1738:\nFirst free African-American community,\nGracia Real de Santa Teresa de Mose in Florida", 
           hjust = 0, family = "IBM Plex Mono Medium", fontface = "italic", size = 7, color = "#F5F5F5") +
  annotate("text", x = 1855, y = 30, label = "1865:\nRatification of\nthe 13th Amendment,\nAbolishment of slavery", 
           hjust = 0, family = "IBM Plex Mono Medium", fontface = "italic", size = 7, color = "#F5F5F5") +
  annotate("text", x = 1895, y = 50, label = "1950s-1960s:\nCivil Rights Movement", 
           hjust = 0, family = "IBM Plex Mono Medium", fontface = "italic", size = 7, color = "#F5F5F5") +
  annotate("text", x = 1970, y = 60, label = "Post Civil Rights Era:\nGreater political\nrepresentation", 
           hjust = 0, family = "IBM Plex Mono Medium", fontface = "italic", size = 6, color = "#F5F5F5") +
  
  paletteer::scale_fill_paletteer_d("dutchmasters::pearl_earring") +
  ggsave("black-achievements.png", device = "png", type = "cairo", width = 24, height = 12, dpi = 300)
