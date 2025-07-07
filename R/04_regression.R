library(sp)
library(spdep)
library(spatialreg)

data(meuse)
coordinates(meuse) <- ~x+y  # Convert to spatial object



# Get coordinates
coords <- coordinates(meuse)

# Create neighbor list
knn <- knearneigh(coords, k = k)
nb <- knn2nb(knn)

# Create spatial weights list (row-standardized)
lw <- nb2listw(nb, style = "W")


lag_model <- lagsarlm(zinc ~ lead + copper, data = meuse, listw = lw)

# Summary of model
summary(lag_model)


dt <- read.xlsx("data/Inzidence.xlsx")
gpt <- read.xlsx("data/GDP.xlsx") 
denspop <- read.xlsx("data/DensPop.xlsx")
dlw <- read.xlsx("data/Landwirtschaft.xlsx")
hou <- read.xlsx("data/Household.xlsx") %>%
  select(-pop)
sex <-  read.xlsx("data/pop_sex.xlsx")
hosp <-  read.xlsx("data/Hospitals.xlsx")
tb <-  read.xlsx("data/TB.xlsx")%>%
  select(-pop)
age20 <-  read.xlsx("data/prop_age20.xlsx") %>%
  filter(year %in% 1919)

age60 <-  read.xlsx("data/prop_age20.xlsx") %>%
  filter(year %in% 1919)

dt_g <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  select(Bezirk, geometry) %>%
  full_join(dt) %>%
  left_join(gpt) %>%
  left_join(denspop) %>%
  full_join(dlw) %>%
  full_join(hou) %>%
  full_join(sex)  %>%
  full_join(hosp) %>%
  full_join(tb)%>%
  full_join(age20)

k <- 5
knn  <- st_knn(dt_g$geometry, k = k)
lw <- nb2listw(knn)

lag_model <- lagsarlm(Inzidenz_W2 ~ prop, listw = lw,data =dt_g)

lm_model <- lm(Inzidenz_W2 ~ prop,data =dt_g)
summary(lag_model)
summary(lm_model)



tm_shape(bezirk_geo)  +
  tm_fill() +
  tm_shape( bezirk_geo  ) + dt_g

  tm_fill("Inzidenz_W2", title = "Excess Mortality",
          palette = "YlOrBr", 
          style = "fixed",
          legend.format=)+
  
  tm_borders(alpha = 1, lwd=0.8, lty="dashed",col="grey20")