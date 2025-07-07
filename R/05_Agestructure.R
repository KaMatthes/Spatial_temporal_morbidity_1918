pop1910 <- read.xlsx("data/Population_Age1910.xlsx") %>%
  select(MapName = Name, Agegroups, pop=Total) %>%
  group_by(MapName) %>%
  mutate(pop_total = sum(pop),
         prop = pop/pop_total) %>% 
  ungroup() %>%
  select(-pop_total, -pop)

dt <- read.xlsx("data/Population.xlsx") %>%
  rename(pop_total = pop) %>%
  left_join(pop1910) %>%
  mutate(pop_age=round(pop_total*prop))


write.xlsx(dt,file=paste0("data/pop_age.xlsx"),row.names=FALSE, overwrite = TRUE)

dt_s <- dt %>%
  filter(Agegroups %in% c("5-9", "10-14")) %>%
  group_by(MapName, year) %>%
  mutate(pop_s = sum(pop_age)) %>%
  ungroup() %>%
  distinct(Bezirk, year, .keep_all = TRUE) %>%
  select(-Agegroups, -prop) %>%
  mutate(prop=pop_s/pop_total) %>%
  select(Bezirk, MapName, year, prop)

write.xlsx(dt_s,file=paste0("data/prop_schoolkids.xlsx"),row.names=FALSE, overwrite = TRUE)

dt_o <- dt %>%
  filter(Agegroups %in% c("60-64","65-69","70-74", "75-79", "80-84","85-89","90-95",">=95")) %>%
  group_by(MapName, year) %>%
  mutate(pop_s = sum(pop_age)) %>%
  ungroup() %>%
  distinct(Bezirk, year, .keep_all = TRUE) %>%
  select(-Agegroups, -prop) %>%
  mutate(prop=pop_s/pop_total)%>%
  select(Bezirk, MapName, year, prop)

write.xlsx(dt_o,file=paste0("data/prop_age60.xlsx"),row.names=FALSE, overwrite = TRUE)


dt_y <- dt %>%
  filter(Agegroups %in% c("20-24","25-29","30-34","35-39")) %>%
  group_by(MapName, year) %>%
  mutate(pop_s = sum(pop_age)) %>%
  ungroup() %>%
  distinct(Bezirk, year, .keep_all = TRUE) %>%
  select(-Agegroups, -prop) %>%
  mutate(prop=pop_s/pop_total)%>%
  select(Bezirk, MapName, year, prop)

write.xlsx(dt_y,file=paste0("data/prop_age20.xlsx"),row.names=FALSE, overwrite = TRUE)