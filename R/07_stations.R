Stations <- read_sf("data_raw/nicht_online/Stationen/Map_stationen.shp") %>%
  select(Bezirk=BEZIRKSNUM, MapName=NAME) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  mutate(station = 1,
         Bezirk= as.factor(Bezirk)) %>%
  group_by(Bezirk,MapName) %>%
  summarise(n_stat = sum(station))


Maps <- read_sf("data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") 
plot_excess <- 
  tm_shape(Stations)  +
  tm_dots() +
  tm_shape(Maps)+
  tm_borders(alpha = 1, lwd=0.8,col="grey20")

  tm_shape( bezirk_geo  ) + 
  tm_fill("excess_jenk", title = "Excess Mortality",
          palette = "YlOrBr", 
          style = "fixed",
          legend.format=)


write.xlsx(Stations,file=paste0("data/Stations.xlsx"),row.names=FALSE, overwrite = TRUE)
save(Stations,file=paste0("data/Stations.RData"))

