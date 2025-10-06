pop <- read.xlsx("data/Population.xlsx") 
area <- read.xlsx("data/Area.xlsx")


dt <- pop %>%
  left_join(area) %>%
  mutate(area= `area.in.ha`/100,
         densPop=pop/area) %>%
  filter(year %in% 1918) %>%
  select(Bezirk, MapName,pop,area, densPop)

write.xlsx(dt ,file=paste0("data/DensPop.xlsx"),row.names=FALSE, overwrite = TRUE)
