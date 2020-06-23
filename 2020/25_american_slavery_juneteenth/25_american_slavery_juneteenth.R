library(tidyverse)
library(ggtext)
library(extrafont)
loadfonts(device = "win", quiet = TRUE) ## to load the font


# Get the Data-----
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')


# Queries to inform plot captions-----
# Percent of Black population that was enslaved
##  min % across all years = 86.3%
census %>% 
  filter(region == "USA Total") %>%            ## look at just the overall totals
  select(year, black_free, black_slaves) %>%   ## only interested in the composition of the Black population per year
  ## values for 1830 was misrecorded (double checked with source PDF)
  ## real values: black_free = 319599, black_slaves = 2009043
  mutate(black_free = if_else(year == 1830, black_free - 300000, black_free),
         black_slaves = if_else(year == 1830, black_slaves + 300000, black_slaves),
         perc_slaves = (black_slaves / (black_slaves + black_free))*100)  ## get the percentage of the Black population that were slaves

# Percent of slaves in each region per year-
## min % in South across all years = 94.2% (1790)
census %>% 
  filter(region %in% c("Northeast", "Midwest", "South", "West","USA Total"),
         is.na(division)) %>%   ## remove counts per division
  select(year, region, black_slaves) %>%   ## only get the counts of the slaves
  pivot_wider(names_from = region, values_from = black_slaves, values_fill = 0) %>%   ## pivot to have a row for each year and column for each region
  mutate(total = Northeast + South + Midwest + West,  ## manually calculate total since the total for 1830 was misrecorded (real value = 2009043)
         ## get percent of slaves from each region 
         ne_perc = Northeast/total *100,
         s_perc = South/total *100,
         mw_perc = Midwest/total *100,
         w_perc = West/total *100)


# Black population - data used for plot-----
black_pop <- census %>% 
  filter(region == "USA Total") %>% 
  select(year, black_free, black_slaves) %>% 
  mutate(black_free = if_else(year == 1830, black_free - 300000, black_free),
         black_slaves = if_else(year == 1830, black_slaves + 300000, black_slaves)) %>% 
  pivot_longer(-year,names_to = "group",values_to = "population")
  

# Theme-----
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "IBM Plex Mono", size = 13),
                      title = element_text(size = 24, margin = margin(t = 0, r = 100, b = 0, l = 0)),
                      plot.title = element_markdown(face = "bold", size = 26, lineheight = 1.2),
                      plot.caption = element_text(size = 12, color = "gray50"),
                      axis.text = element_text(size = 20),
                      axis.title.x = element_blank(),
                      axis.text.y = element_text(size = 18),
                      axis.line.x = element_line(color = "gray60"),
                      axis.line.y = element_blank(),
                      panel.grid = element_blank(),
                      panel.grid.major.y = element_line(color = "gray90"),
                      legend.text = element_text(size = 20, face = "bold"),
                      plot.margin = margin(70, 80, 30, 35),
                      plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
                      legend.position = c(.15, .51),
                      legend.direction = "horizontal")


# Plot-----
plot <- black_pop %>% 
  ggplot(aes(year,population)) +
  labs(title = "How many people were enslaved in the U.S. before the last of<br>them were proclaimed <span style = 'color:#E09D00;'>free</span> in Texas on <span style = 'color:#E09D00;'>June 19, 1865</span>?",
       y = "Black Population", fill = "",
       caption = "Data: U.S. Census Bureau | Plot: @_isabellamb") +
  geom_col(aes(fill = group), width = 7) +
  annotate("text", x = 1785, y = 5050000, label = "The number of enslaved people in the U.S. grew from 700,000 in 1790 to 4 million by the abolishment of slavery.\nBased on every census before 1870, at least 85% of the Black population were enslaved.\nUntil they were all freed, more than 94% of the slaves in the U.S. were in the South.", 
           hjust = 0, vjust = 1, family = "Rubik",size = 6.4) +
  annotate("text", x = 1786, y = 3500000, label = "The first Juneteenth was celebrated in 1866.", 
           hjust = 0, family = "IBM Plex Mono", fontface = "bold", size = 7) +
  scale_x_continuous(breaks = seq(1790,1870,10), expand = expansion(0,0)) +
  scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6,accuracy = 1), ## millions labels
                     breaks = seq(1000000,4000000,1000000),
                     expand = expansion(0,0),
                     limits = c(0,5100000)) +
  scale_fill_manual(values = c("#FFD166", "gray20"), labels = c("Free","Enslaved")) 

plot + ggsave("juneteenth.png", device = "png", type = "cairo", width = 15.5, height = 14, dpi = 300)
