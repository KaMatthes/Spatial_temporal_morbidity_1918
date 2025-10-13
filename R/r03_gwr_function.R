function_gwr_data <- function(w, wave_n, var, var_n,n_boot, conf) {

dt <- read.xlsx("data/Faelle_Bezirke_total_pop_wave.xlsx") %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  # filter(!Kanton %in% c("AI")) %>%
  filter(wave==w)


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
    hosp_pop_s = scale( hosp_pop),
    docs_pop = docs/pop*10000,
    docs_pop_n= normalit(docs_pop),
    docs_pop_s= scale(docs_pop),
    docs_p_pop= Doc_privat/pop*10000,
    docs_p_pop_n= normalit(docs_p_pop),
    docs_p_pop_s= scale(docs_p_pop),
    docs_s_pop= Doc_spital/pop*10000,
    docs_s_pop_n= normalit(docs_s_pop),
    docs_s_pop_s= scale(docs_s_pop),
    propmale_n= normalit(propmale),
    propmale_s= scale(propmale),
    propkids_n= normalit(propkids),
    propkids_s= scale(propkids),
    prop20_n= normalit(prop20),
    prop20_s= scale(prop20),
    prop60_n= normalit(prop60),
    prop60_s= scale(prop60),
    In_prop = 100-LW_prop,
    LW_prop_n= normalit(LW_prop),
    In_prop_n= normalit(In_prop),
    In_prop_s= scale(In_prop),
    GDP_n = normalit(GDP),
    GDP_s = scale(GDP),
    densPop_n= normalit(densPop),
    densPop_s= scale(densPop),
    tb_d_pop = TB_death/pop*10000,
    tb_d_pop_n = normalit(tb_d_pop),
    tb_d_pop_s = scale(tb_d_pop),
    Haushalte.pro.Haus_n =normalit(Haushalte.pro.Haus),
    Haushalte.pro.Haus_s =scale(Haushalte.pro.Haus),
    person.pro.Haushalt_n =normalit(person.pro.Haushalt),
    person.pro.Haushalt_s = scale(person.pro.Haushalt),
    incidence_s = scale(incidence),
    incidence_n= normalit(incidence)
  ) %>%
  as(. , "Spatial")

dp <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") 
# %>%
#   filter(!BEZNR %in% 1600)

gnb <- poly2nb(dp)
glw  <- nb2listw(gnb)

# # apply Moran Test (UNBIASED)
# lm.mod <- lm(incidence_s~  prop60_s, data=ds)
# summary(lm.mod)
# moralm <- lm.morantest(lm.mod, listw=glw)
# ds$resid_lm <- residuals(lm.mod)




bw <- bw.gwr(formula =incidence_s~ prop60_s,
             approach = "AICc",
             kernel="gaussian",
             adaptive= T,
             data = ds) 

gwr.mod <- gwr.robust(formula =incidence_s~ prop60_s,
                     adaptive = T,
                     data = ds, 
                     bw = bw) 

res.lm <- summary(gwr.mod$lm)$coefficients %>%
  data.frame() %>%
  mutate(Cl = round(Estimate - 1.96 * Std..Error,2),
         Cu =  round(Estimate + 1.96 * Std..Error,2),
         Coef = round(Estimate, 2),
         Cofactor = var_n, 
         wave = wave_n,
         global= paste0(Coef," (",Cl, " to ", Cu,")"))%>%
  select(Cofactor, wave,global) %>%
  slice(2)

# mora <- moran.test(gwr.mod$SDF$residual, listw=glw)

vals <- gwr.mod$SDF@data %>%
  select(2)  %>%
  as.matrix()



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


res <- bootstrap_gwr_coef(vals,n_boot = 2000, conf = 0.95, seed = 20250929) %>%
  as.data.frame(.) %>%
  mutate(Cofactor = var_n, 
         Coef = round(boot_median,2),
         Cl = round(ci_lower,2),
         Cu = round(ci_upper,2),
         wave = wave_n,
         gwr = paste0(Coef," (",Cl, " to ", Cu,")")) %>%
  select(Cofactor, wave,gwr) %>%
  left_join(res.lm)

}

# get intervals with bootstrapping

tm_shape(gwr.mod$SDF) +
  tm_fill(
    "prop60_s",
    palette="-RdBu",
    style = "cont",
    midpoint = 0) +
  tm_layout(legend.position = c("right","top"),  title = "GWR") +
  # now add the t-values layer
  tm_borders(col = "black", lwd = 0.5) 
