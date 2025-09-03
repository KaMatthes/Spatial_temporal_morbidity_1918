dt <- read.xlsx("data/Faelle_Bezirke_total_pop_wave.xlsx") %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  filter(!Kanton %in% c("AI"))

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


dt_g <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  select(Bezirk, geometry) %>%
  filter(!Bezirk %in% 1600 ) %>%
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
    docs_pop = ifelse(Bezirk %in% c(1811, 1812,1808), 20, docs_pop),
    docs_pop_n= normalit(docs_pop),
    docs_p_pop= Doc_privat/pop*10000,
    docs_p_pop = ifelse(Bezirk %in% c(1811, 1812,1808),8 ,docs_p_pop),
    docs_p_pop_n= normalit(docs_p_pop),
    docs_s_pop= Doc_spital/pop*10000,
    docs_s_pop = ifelse(docs_s_pop >90 , 90, docs_p_pop),
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
    incidence_n = normalit(incidence),
    incidence_s = scale(incidence)
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


function_spatial <- function(k, w, v) {
 
if(w==1 | w==2) {
  dt1 <- dt_g %>%
  filter(wave ==w)
  }
  
else if(w==3) {
    dtt <- dt_g %>%
      filter(wave == 2) %>%
      as.data.frame() %>%
      select(Bezirk, IncW2 = incidence) 
    
    dt1 <- dt_g %>%
      filter(wave==3) %>%
      full_join(dtt)
 
  }


k <- k
knn  <- st_knn(dt1$geometry, k = k)
lw <- nb2listw(knn)


formula_str <- paste("incidence_s ~",v)
formula <- as.formula(formula_str)

# I assume spatial lag and spatial error term
model <- sacsarlm(formula, listw = lw,data =dt1)
res <- summarize_sacsarlm(model) %>%
  mutate(
    kn = k,
    wave = w,
    Estimate = round(Estimate, 2),
    CIl = round(CIl,2),
    CIu = round(CIu,2)
  ) %>%
  select(Term, kn, wave, Estimate, CIl, CIu) %>%
  filter(
    !Term %in% "(Intercept)")

return(res)

# impacts(model,listw=lw)

}


w11 <- function_spatial(5, 1,"GDP_n")
w12 <- function_spatial(5, 1,"densPop_n")
w13 <- function_spatial(5, 1,"In_prop_n")
w14 <- function_spatial(5, 1,"Haushalte.pro.Haus_n")
w15 <- function_spatial(5, 1,"person.pro.Haushalt_n")
w16 <- function_spatial(5, 1,"hospitals")
w17 <- function_spatial(5, 1,"tb_d_pop_n")
w19 <- function_spatial(5, 1,"city")
w120 <- function_spatial(5, 1,"hosp_pop")
w121 <- function_spatial(5, 1,"propkids_n")
w122 <- function_spatial(5, 1,"prop20_n")
w123 <- function_spatial(5, 1,"prop60_n")
w124 <- function_spatial(5, 1,"propmale_n")
w125 <- function_spatial(5, 1,"docs_pop_n")
w126 <- function_spatial(5, 1,"docs_p_pop_n")
w127 <- function_spatial(5, 1,"docs_s_pop_n")


w1 <- rbind(w11, w12, w13, w14, w15, w16, w17,w19, w120,w121,w122, w123,w124, w125,w126,w127)

w21 <- function_spatial(5, 2,"GDP_n")
w22 <- function_spatial(5, 2,"densPop_n")
w23 <- function_spatial(5, 2,"In_prop_n")
w24 <- function_spatial(5, 2,"Haushalte.pro.Haus_n")
w25 <- function_spatial(5, 2,"person.pro.Haushalt_n")
w26 <- function_spatial(5, 2,"hospitals")
w27 <- function_spatial(5, 2,"tb_d_pop_n")
w29 <- function_spatial(5, 2,"city")
w220 <- function_spatial(5, 2,"hosp_pop")
w221 <- function_spatial(5, 2,"propkids_n")
w222 <- function_spatial(5, 2,"prop20_n")
w223 <- function_spatial(5, 2,"prop60_n")
w224 <- function_spatial(5, 2,"propmale_n")
w225 <- function_spatial(5, 2,"docs_pop_n")
w226 <- function_spatial(5, 2,"docs_p_pop_n")
w227 <- function_spatial(5, 2,"docs_s_pop_n")

w2 <- rbind(w21, w22, w23, w24, w25, w26, w27, w29, w220,w221,w222,w223,w224, w225,w226, w227)

w31 <- function_spatial(5, 3,"GDP_n")
w32 <- function_spatial(5, 3,"densPop_n")
w33 <- function_spatial(5, 3,"In_prop_n")
w34 <- function_spatial(5, 3,"Haushalte.pro.Haus_n")
w35 <- function_spatial(5, 3,"person.pro.Haushalt_n")
w36 <- function_spatial(5, 3,"hospitals")
w37 <- function_spatial(5, 3,"tb_d_pop_n")
w39 <- function_spatial(5, 3,"city")
w320 <- function_spatial(5, 3,"hosp_pop")
w321 <- function_spatial(5, 3,"propkids_n")
w322 <- function_spatial(5, 3,"prop20_n")
w323 <- function_spatial(5, 3,"prop60_n")
w324 <- function_spatial(5, 3,"propmale_n")
w325 <- function_spatial(5, 3,"docs_pop_n")
w326 <- function_spatial(5, 3,"docs_p_pop_n")
w327 <- function_spatial(5, 3,"docs_s_pop_n")
# w321 <- function_spatial(5, 3,"IncW2")

w3<- rbind(w31, w32, w33, w34, w35, w36, w37, w39, w320,w321,w322, w323,w324, w325, w326, w327)

wt <- rbind(w1, w2, w3)
write.xlsx( wt ,"output/Regression_lag_error.xlsx", row.names=FALSE, overwrite = TRUE)

# 
# dt_m <- dt1 %>%
#   mutate(res_docs = model$residuals)
# 
tm_shape(dt_g)  +
  tm_fill("prop20_n")+
  tm_borders(lwd=0.8,col="grey20")

# 
# ggplot()+
#   geom_point(data=dt_m, aes(Bezirk, res_docs))


# 
m.lm <- lm(incidence ~ docs_pop_n, data = dt1)
summary(m.lm )
res <- lm.LMtests(m.lm, lw, test=c("LMerr", "LMlag",
                                                        "RLMerr", "RLMlag", "SARMA"))
summary(res)


ggplot(dt_g) +
  geom_point(aes(Bezirk, docs_s_pop))

ggplot(dt_g)+
  geom_boxplot(aes(y=prop20_n))

