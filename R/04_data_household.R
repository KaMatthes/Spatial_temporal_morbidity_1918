pop <- read.xlsx("data/Population.xlsx") %>%
  filter(year== 1920)
area <- read.xlsx("data/Haushalte1920.xlsx")


dt <- pop %>%
  left_join(area) %>%
  mutate(person.pro.Haushalt = pop/Haushalte) %>%
  select(-year)

write.xlsx(dt ,file=paste0("data/Household.xlsx"),row.names=FALSE, overwrite = TRUE)
