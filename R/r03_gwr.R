dt <- read.xlsx("data/Faelle_Bezirke_total_pop_wave.xlsx") %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  filter(!Kanton %in% c("AI")) %>%
  filter(wave==2)

# dt <- read.xlsx("data/Faelle_Bezirke_total_20_40_wave.xlsx") %>%
#   mutate(
#     Bezirk = as.numeric(Bezirk),
#   ) %>%
#   filter(!Kanton %in% c("AI"))


gpd <- read.xlsx("data/GDP.xlsx") 

denspop <- read.xlsx("data/DensPop.xlsx") %>%
  select(-pop, -area)

dlw <- read.xlsx("data/Landwirtschaft.xlsx") %>%
  rename(LW_prop = Anteil) %>%
  select(-Erwerbende, -LW) 

hou <- read.xlsx("data/Household.xlsx") %>%
  select(-pop, -Hauser,-Haushalte)


hosp <-  read.xlsx("data/Hospitals.xlsx")

tb <-  read.xlsx("data/TB.xlsx")%>%
  select(-pop, -Tb.Deaths.1911_1920) %>%
  rename(TB_death = pro_jahr)

ur <-  read.xlsx("data/Urbanity.xlsx") %>%
  mutate(
    city=as.factor(city)
  )

age20 <-  read.xlsx("data/prop_age20.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(prop20 = prop) %>%
  select(-year)

age60 <-  read.xlsx("data/prop_age60.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(prop60 =prop) %>%
  select(-year)

agekids <-  read.xlsx("data/prop_schoolkids.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(propkids =prop) %>%
  select(-year)

propmale <-  read.xlsx("data/prop_sex.xlsx") %>%
  rename(propmale =prop_male) 

docs <-  read.xlsx("data/Aerzte1910.xlsx") %>%
  select(Bezirk, MapName, Doc_privat,Doc_spital,docs = Both)


ds <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  select(Bezirk, geometry) %>%
  full_join(dt) %>%
  left_join(gpd) %>%
  left_join(denspop) %>%
  full_join(dlw) %>%
  full_join(hou) %>%
  # full_join(sex)  %>%
  full_join(hosp) %>%
  full_join(tb)  %>%
  full_join(ur)  %>%
  full_join(age60) %>%
  full_join(age20) %>%
  full_join(agekids) %>%
  full_join(propmale) %>%
  full_join(docs) %>%
  filter(!is.na(year)) %>%
  mutate(
    hosp_pop = hospitals/pop*10000,
    hosp_pop_n = normalit( hosp_pop),
    docs_pop = docs/pop*10000,
    docs_pop_n= normalit(docs_pop),
    docs_p_pop= Doc_privat/pop*10000,
    docs_p_pop_n= normalit(docs_p_pop),
    docs_s_pop= Doc_spital/pop*10000,
    docs_s_pop_n= normalit(docs_s_pop),
    propmale_n= normalit(propmale),
    propkids_n= normalit(propkids),
    prop20_n= normalit(prop20),
    prop60_n= normalit(prop60),
    In_prop = 100-LW_prop,
    LW_prop_n= normalit(LW_prop),
    In_prop_n= normalit(In_prop),
    GDP_n= normalit(GDP),
    densPop_n= normalit(densPop),
    tb_d_pop = TB_death/pop*10000,
    tb_d_pop_n = normalit(tb_d_pop),
    Haushalte.pro.Haus_n =normalit(Haushalte.pro.Haus),
    person.pro.Haushalt_n =normalit(person.pro.Haushalt),
    incidence_s = scale(incidence),
    incidence_n= normalit(incidence)
  ) %>%
  as(. , "Spatial")




bw <- bw.gwr(formula =incidence_n~  docs_pop_n,
             approach = "CV",
             kernel="gaussian",
             adaptive = T, 
             data = ds) 

gwr.mod <- gwr.basic(formula =incidence_n~ docs_pop_n,
                     adaptive = T,
                     data = ds, 
                     bw = bw) 



vals <- gwr.mod$SDF@data %>%
  select(2)  %>%
  as.matrix()


# 1. Split values into negatives and positives
neg_vals <- vals[vals < 0]
pos_vals <- vals[vals > 0]

# 2. Run Jenks separately
neg_breaks <- classIntervals(neg_vals, n = 2, style = "jenks")$brks
pos_breaks <- classIntervals(pos_vals, n = 2, style = "jenks")$brks

# 3. Combine, making sure 0 is included
all_breaks <- c(neg_breaks,0,pos_breaks)



tm_shape(gwr.mod$SDF) +
  tm_fill(
    "docs_pop_n",
    palette="-RdBu",
    style = "fixed",
    midpoint = 0,
    breaks = all_breaks ) +
  tm_layout(legend.position = c("right","top"),  title = "GWR") +
  # now add the t-values layer
  tm_borders(col = "black", lwd = 0.5) 


# 
# tm_shape(gwr.mod$SDF) +
#   tm_fill(
#     "person.pro.Haushalt_n",
#     palette="-RdBu",
#     style = "jenks",
#     midpoint = 0,
#     breaks = all_breaks ) +
#   tm_layout(legend.position = c("right","top"),  title = "GWR") +
#   # now add the t-values layer
#   tm_borders(col = "black", lwd = 0.5)




bootstrap_gwr_coef <- function(coef_vector, n_boot = 1000, conf = 0.95, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  n <- length(coef_vector)
  boot_means <- replicate(n_boot, {
    sample_vals <- sample(coef_vector, size = n, replace = TRUE)
    median(sample_vals)
  })
  
  alpha <- (1 - conf) / 2
  ci <- quantile(boot_means, probs = c(alpha, 1 - alpha))
  
  list(
    coef_median = median(coef_vector),
    boot_median = median(boot_means),
    ci_lower = ci[1],
    ci_upper = ci[2]
  )
}


bootstrap_gwr_coef(vals,n_boot = 1000, conf = 0.95, seed = 20250929)

# get intervals with bootstrapping

