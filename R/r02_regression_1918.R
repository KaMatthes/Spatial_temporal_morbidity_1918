normalit <-function(m){
  (m - min(m))/(max(m)-min(m))
}


dt <- read.xlsx("data/Faelle_Bezirke_total_pop_year.xlsx") %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  # filter(!Kanton %in% c("AI","VS", "FR")) %>%
  filter(year==1918)


dte <- read.xlsx("data/expected_death_inla.xlsx") %>%
  mutate(
    exc_pop = excess/pop*10000,
    Bezirk = as.numeric(Bezirk),
    excess3 = excess2 *100
    ) %>%
  select(Bezirk, excess,exc_pop,excess3, excess2) 

dt <- dt %>%
  left_join(dte) %>%
  mutate(
    inc_n = normalit(incidence),
    exc_n = normalit(excess2),
    exc_n2 = normalit(exc_pop)
  )

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

dt_g <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
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
  full_join(agekids) %>%
  full_join(age20) %>%
  full_join(age60) %>%
  full_join(propmale) %>%
  filter(!is.na(year)) %>%
  mutate(
    hosp_pop= hospitals/pop*10000,
    prop_male_n = normalit( propmale)
  )

# function to summarize
summarize_sacsarlm <- function(model) {
  coefs <- summary(model)$Coef
  rho <- model$rho
  lambda <- model$lambda
  se_rho <- summary(model)$rho.se
  se_lambda <- summary(model)$lambda.se
  
  coef_df <- as.data.frame(coefs)
  coef_df$Term <- rownames(coef_df)
  coef_df <- coef_df[, c("Term", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
  names(coef_df) <- c("Term", "Estimate", "Std_Error", "z_value", "p_value")
  coef_df$CIl <-coef_df$Estimate - 1.96*coef_df$Std_Error
  coef_df$CIu <-coef_df$Estimate + 1.96*coef_df$Std_Error
  coef_df <- coef_df %>%
    select(-Std_Error,-z_value, -p_value)
  
  # spatial_df <- data.frame(
  #   Term = c("Rho (ρ)", "Lambda (λ)"),
  #   Estimate = c(rho, lambda),
  #   Std_Error = c(se_rho, se_lambda),
  #   z_value = c(rho/se_rho, lambda/se_lambda),
  #   p_value = 2 * pnorm(-abs(c(rho/se_rho, lambda/se_lambda)))
  # )
  # 
  # full_df <- rbind(coef_df, spatial_df)
  return(  coef_df)
}


function_spatial <- function(o,k, v) {
 
  dt1 <- dt_g
  
  k <- k
  knn  <- st_knn(dt1$geometry, k = k)
  lw <- nb2listw(knn)
  

  
  formula_str <- paste(o,"~",v)
  formula <- as.formula(formula_str)
  
  # I assume spatial lag and spatial error term
  model <- sacsarlm(formula, listw = lw,data =dt1)
  res <- summarize_sacsarlm(model) %>%
    mutate(
      kn = k,
      Estimate = round(Estimate, 2),
      CIl = round(CIl,2),
      CIu = round(CIu,2)
    ) %>%
    select(Term, kn, Estimate, CIl, CIu) %>%
    filter(
      !Term %in% "(Intercept)")
  
  return(res)
  
}


d1 <- function_spatial("excess3",5,"GDP")
d2 <- function_spatial("excess3",5,"densPop")
d3 <- function_spatial("excess3",5,"LW_prop")
d4 <- function_spatial("excess3",5,"Haushalte.pro.Haus")
d5 <- function_spatial("excess3",5,"person.pro.Haushalt")
d6 <- function_spatial("excess3",5,"hospitals")
d7 <- function_spatial("excess3",5,"TB_death")
d8 <- function_spatial("excess3",5,"TB_inc")
d9 <- function_spatial("excess3",5,"city")
d10 <- function_spatial("excess3",5,"hosp_pop")
d11 <- function_spatial("excess3",5,"propmale")
d12 <- function_spatial("excess3",5,"propkids")
d13 <- function_spatial("excess3",5,"prop20")
d14 <- function_spatial("excess3",5,"prop60")
d15 <- function_spatial("excess3",5,"incidence")

dtotal2 <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10,d11,d12,d13,d14,d15)

i1 <- function_spatial("incidence",5,"GDP")
i2 <- function_spatial("incidence",5,"densPop")
i3 <- function_spatial("incidence",5,"LW_prop")
i4 <- function_spatial("incidence",5,"Haushalte.pro.Haus")
i5 <- function_spatial("incidence",5,"person.pro.Haushalt")
i6 <- function_spatial("incidence",5,"hospitals")
i7 <- function_spatial("incidence",5,"TB_death")
i8 <- function_spatial("incidence",5,"TB_inc")
i9 <- function_spatial("incidence",5,"city")
i10 <- function_spatial("incidence",5,"hosp_pop")
i11 <- function_spatial("incidence",5,"prop_male_n")
i11 <- function_spatial("incidence",5,"propmale")
i12 <- function_spatial("incidence",5,"propkids")
i13 <- function_spatial("incidence",5,"prop20")
i14 <- function_spatial("incidence",5,"prop60")

itotal <- rbind(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10,i11,i12, i13, i14)


wt <- rbind(dtotal, itotal)
write.xlsx( wt ,"output/Regression_total_pop_new.xlsx", row.names=FALSE, overwrite = TRUE)