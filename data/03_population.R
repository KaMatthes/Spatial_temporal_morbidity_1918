dt <- read.xlsx("data/Population_district_1910_1920.xlsx") %>%
  select(-Bemerkungen) %>%
  gather(., year, pop,  `1910`:`1920`) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(Bezirk,MapName) %>%
  complete(year = seq(1910,1920, by=1)) %>%
  arrange(Bezirk, year) %>%
  mutate(pop = round(zoo::na.approx(pop, na.rm=FALSE),0)) %>%
  filter(year %in% 1918:1920)



write.xlsx(dt ,"data/Population.xlsx", row.names=FALSE, overwrite = TRUE)
