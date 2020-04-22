rankings %>% 
  filter(gender  == "female")

rankings %>% 
  filter(gender  == "female") %>% 
  count(artist, sort = TRUE)

rankings %>% 
  count(artist, sort = TRUE)

polls %>% 
  distinct(critic_name) %>% 
  count()

polls %>% 
  distinct(critic_name,critic_country) %>% 
  count(critic_country) %>% 
  summarize(sum(n))