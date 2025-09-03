dt <- read.xlsx("data/Faelle_Bezirke_Conscrips_wave.xlsx", detectDates = TRUE) %>%
  mutate(
    Bezirk = as.numeric(Bezirk)
  )

dt_g <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  select(Bezirk, geometry) %>%
  slice(rep(1:n(), 3)) %>%
  mutate(
    wave = rep(1:3, each=183)
  ) %>%
  full_join(dt) %>%
  mutate(
    Cases = ifelse(is.na(Cases), 0, Cases),
    Cases_col =ifelse(Cases >0, "1", "0")

  )

dt1 <- dt_g %>%
  filter(wave==3)

tm_shape(dt1)  +
  tm_fill("Cases_col")+
  tm_borders(lwd=0.8,col="grey20")
