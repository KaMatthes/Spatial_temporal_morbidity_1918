pop1910 <- read.xlsx("data/Population_Age1910.xlsx") %>%
  select(MapName = Name, Agegroups, male=m_pop, female=f_pop) %>%
  group_by(MapName) %>%
  mutate(pop_male = sum(male),
         pop_female = sum(female))%>% 
  ungroup() %>%
  distinct(MapName, .keep_all = TRUE) %>%
  select(MapName, pop_male, pop_female) %>%
  mutate(total = pop_female + pop_male,
         prop_male = pop_male/total) %>%
  select(MapName, prop_male)

dt <- read.xlsx("data/Population.xlsx") %>%
  filter(year ==1918) %>%
  rename(pop_total = pop) %>%
  left_join(pop1910) %>%
  mutate(pop_male=round(pop_total*prop_male)) %>%
  select(Bezirk, MapName, prop_male) 


write.xlsx(dt,file=paste0("data/pop_sex.xlsx"),row.names=FALSE, overwrite = TRUE)
