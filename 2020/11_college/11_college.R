#date: April 4, 2020
library(tidyverse)
library(rvest)
library(ggrepel)
library(usmap)
library(RColorBrewer)
library(extrafont)
fonttable <- fonttable()

loadfonts(device = "win", quiet = TRUE) ## to load the font

# Import the data

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

# Prepare the data

tuition_income_summary <- tuition_income %>%  ## summarizing the tuition_income table
  group_by(year, income_lvl) %>% 
  summarise(avg_net = mean(net_cost),
            avg_cost = mean(total_price),
            min_net = min(net_cost),
            max_net = max(net_cost),
            min_tot = min(total_price),
            max_tot = max(total_price)) 

tuition_diversity <- left_join(tuition_cost, diversity_school, by = c("name", "state")) ## joining the cost & diversity data

tuition_diversity <- tuition_diversity %>%  ## fixing the table
  filter(!is.na(name)) %>% 
  group_by(name) %>% 
  spread(category, enrollment) %>% 
  mutate(poc = `Total Minority`/total_enrollment,
         women_percent = Women/total_enrollment,
         nonresident = `Non-Resident Foreign`/total_enrollment)

# Set theme

theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Source Sans Pro Light", size = 13),
                      title = element_text("Franklin Gothic", size = 20, color = "gray20"),
                      plot.title = element_text("Franklin Gothic", size = 30, color = "gray20"),
                      plot.subtitle = element_text("Source Sans Pro Light", size = 20, color = "gray20"),
                      axis.text = element_text(size = 16),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80"))



# Graphs-----------------------------------------------------------------------

# How the price of tuition has changed per income level between 2010 - 2018
tuition_income_summary %>% 
  ungroup() %>% 
  group_by(income_lvl) %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot(aes(x = year, y = avg_net, group = income_lvl, color = income_lvl)) +
  labs(title = "Tuition per income level has started to decrease",
       x = "",
       y = "Average net cost",
       color = "Income level"
       ) +
  geom_point(size = 1.8) +
  geom_line(size = 2) +
  scale_color_brewer(palette = "YlGnBu") +
  scale_x_discrete(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(labels = scales::comma)

ggsave(here::here("2020", "11_college", "Tuition thru time per income level.png"), device = "png", width = 12, height = 8, dpi = 300)


# The increasing cost of college, selected years between 1985 - 2016
  ## for the labels
labels_inst <- historical_tuition %>% 
  filter(type != "All Institutions") %>%
  mutate(year = as.numeric(str_sub(year,1,4))) %>%  ## reformatting the year
  group_by(type) %>% 
  summarize(tuition_cost = max(tuition_cost),
            year = max(year))
  ## plot
historical_tuition %>%
  mutate(year = as.numeric(str_sub(year,1,4))) %>% ## reformatting the year
  filter(tuition_type == "All Current",
         type != "All Institutions") %>% 
  group_by(type) %>% 
  ggplot(aes(x = year, y = tuition_cost, group = type, color = type)) +
  labs(title = "The cost of a college education in the US continues to rise",
       subtitle = "All costs have been adjusted to the value of the dollar in 2016",
       x = "",
       y = "Average total tuition, fees, room and board rates\n",
       color = "") +
  geom_line(size = 2) +
  scale_x_continuous(expand = expansion(mult = c(0,0)), breaks = seq(1985,2020, by = 5)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values = c("#f7797d", "#f5af19")) +
  geom_label(data = labels_inst, 
                   aes(label = c("Private institution", "Public institution")),
                   family = "Source Sans Pro Light",
                   size = 7,
                   label.size = NA,
                   nudge_x = -4) + theme(legend.position = "none")

ggsave(here::here("2020", "11_college", "Tuition thru time.png"), device = "png", width = 12, height = 8, dpi = 300)


# In-state tuition and diversity
tuition_diversity %>% 
  filter(degree_length == "4 Year") %>%  ## only looking at 4-year colleges
  ggplot(aes(x = in_state_tuition, y = poc)) +
  geom_point(alpha = 1/3, color = "#8360c3", size = 4) +
  labs(title = "More expensive colleges have less diversity",
       x = "In-state tuition",
       y = "% of minority students",
       color = "") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent)

ggsave(here::here("2020", "11_college", "Tuition and diversity.png"), device = "png", width = 12, height = 8, dpi = 300)

