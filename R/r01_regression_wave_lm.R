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
  full_join(age60) %>%
  full_join(age20) %>%
  full_join(agekids) %>%
  full_join(propmale) %>%
  filter(!is.na(year)) %>%
  mutate(
    hosp_pop= hospitals/pop*10000,
    incidence2 = mx *1000
  )

# function to summarize
# summarize_rlm <- function(model) {
#   # coefs <- summary(model)$Coef
#   # rho <- model$rho
#   # lambda <- model$lambda
#   # se_rho <- summary(model)$rho.se
#   # se_lambda <- summary(model)$lambda.se
#   
#   coef_df <- as.data.frame(coefs)
#   coef_df$Term <- rownames(coef_df)
#   coef_df <- coef_df[, c("Term", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
#   names(coef_df) <- c("Term", "Estimate", "Std_Error", "z_value", "p_value")
#   coef_df$CIl <-coef_df$Estimate - 1.96*coef_df$Std_Error
#   coef_df$CIu <-coef_df$Estimate + 1.96*coef_df$Std_Error
#   coef_df <- coef_df %>%
#     select(-Std_Error,-z_value, -p_value)
#   
#   # spatial_df <- data.frame(
#   #   Term = c("Rho (ρ)", "Lambda (λ)"),
#   #   Estimate = c(rho, lambda),
#   #   Std_Error = c(se_rho, se_lambda),
#   #   z_value = c(rho/se_rho, lambda/se_lambda),
#   #   p_value = 2 * pnorm(-abs(c(rho/se_rho, lambda/se_lambda)))
#   # )
#   # 
#   # full_df <- rbind(coef_df, spatial_df)
#   return(  coef_df)
# }

function_rlm <- function(w,v) {
  
  dt1 <- dt_g %>%
  filter(wave ==w)


formula_str <- paste("incidence2~",v)
formula <- as.formula(formula_str)

# I assume spatial lag and spatial error term
model <- lm(formula,data =dt1)
res <- coef(summary(model))

return(res)

}


Mod1 <- coef(summary(rlm(incidence ~ GDP, data=dt_g)))
                     
w11 <- function_rlm(1,"GDP")
w12 <- function_rlm(1,"densPop")
w13 <- function_rlm(1,"LW_prop")
w14 <- function_rlm(1,"Haushalte.pro.Haus")
w15 <- function_rlm(1,"person.pro.Haushalt")
w16 <- function_rlm(1,"hospitals")
w17 <- function_rlm(1,"TB_death")
w18 <- function_rlm(1,"TB_inc")
w19 <- function_rlm(1,"city")
w120 <- function_rlm(1,"hosp_pop")
w121 <- function_rlm(1,"propkids")
w122 <- function_rlm(1,"prop20")
w123 <- function_rlm(1,"prop60")
w124 <- function_rlm(1,"propmale")

w1 <- rbind(w11, w12, w13, w14, w15, w16, w17, w18, w19, w120,w121,w122, w123,w124) %>%
  data.frame() %>%
  mutate(wave = "first")%>%
  mutate(Cofactor=row.names(.))

w21 <- function_rlm(2,"GDP")
w22 <- function_rlm(2,"densPop")
w23 <- function_rlm(2,"LW_prop")
w24 <- function_rlm(2,"Haushalte.pro.Haus")
w25 <- function_rlm(2,"person.pro.Haushalt")
w26 <- function_rlm(2,"hospitals")
w27 <- function_rlm(2,"TB_death")
w28 <- function_rlm(2,"TB_inc")
w29 <- function_rlm(2,"city")
w220 <- function_rlm(2,"hosp_pop")
w221 <- function_rlm(2,"propkids")
w222 <- function_rlm(2,"prop20")
w223 <- function_rlm(2,"prop60")
w224 <- function_rlm(2,"propmale")

w2 <- rbind(w21, w22, w23, w24, w25, w26, w27, w28, w29, w220,w221,w222,w223,w224) %>%
  data.frame() %>%
  mutate(wave = "second") %>%
  mutate(Cofactor=row.names(.))


w31 <- function_rlm(3,"GDP")
w32 <- function_rlm(3,"densPop")
w33 <- function_rlm(3,"LW_prop")
w34 <- function_rlm(3,"Haushalte.pro.Haus")
w35 <- function_rlm(3,"person.pro.Haushalt")
w36 <- function_rlm(3,"hospitals")
w37 <- function_rlm(3,"TB_death")
w38 <- function_rlm(3,"TB_inc")
w39 <- function_rlm(3,"city")
w320 <- function_rlm(3,"hosp_pop")
w321 <- function_rlm(3,"propkids")
w322 <- function_rlm(3,"prop20")
w323 <- function_rlm(3,"prop60")
w324 <- function_rlm(3,"propmale")

# w321 <- function_spatial(5, 3,"IncW2")

w3<- rbind(w31, w32, w33, w34, w35, w36, w37, w38, w39, w320,w321,w322, w323,w324) %>%
  data.frame() %>%
  mutate(wave = "third")  %>%
  mutate(Cofactor=row.names(.))


res_uni2 <- rbind(w1, w2, w3) %>%
  filter( Cofactor=="GDP" | Cofactor=="densPop" |   Cofactor=="LW_prop"| Cofactor=="Haushalte.pro.Haus"| Cofactor=="person.pro.Haushalt"| 
            Cofactor=="hospitals" | Cofactor=="TB_death" |   Cofactor=="TB_inc"| 
            Cofactor==" city1" |   Cofactor=="hosp_pop" |   Cofactor=="propkids" |   Cofactor=="prop20" |   Cofactor=="prop60"|
            Cofactor=="propmale" ) %>%
  mutate(est= round(Estimate,2),
         Cl = round(Estimate - 1.96* Std..Error,2),
         Cu = round(Estimate + 1.96* Std..Error,2),
         Univariate = paste0(est," (",Cl,"-",Cu, ")"),
         Cofactor=recode(Cofactor, 
                         "GDP" = "GDP",
                         "hosp_group.1" = "At least 1 hospital (Ref: no hospitals)",
                         "prop_kids" = "Proportion of 5-14 year-olds",
                         "prop_70" = "Proportion of >=70 year-olds",
                         "prop_men_norm" = "Proportion of men",
                         "prop_child_death" = "Proportion of child mortality",
                         "tbc_inc" = "Tuberculosis mortality",
                         "dens_grouplarge" = "Large population density (Ref: Small)",
                         "city_bezirk1" = "Urbanicity (Ref: Rural)",
                         "station_area" = "Railway stations per inhabitant",
                         "excess_1890" = "Excess mortality 1890"),
         Cofactor=factor(Cofactor, levels=c("GDP" = "GDP / SEP", "Urbanicity (Ref: Rural)","Large population density (Ref: Small)",
                                            "Proportion of 5-14 year-olds","Proportion of >=70 year-olds","Proportion of men",
                                            "Proportion of child mortality",
                                            "At least 1 hospital (Ref: no hospitals)","Railway stations per inhabitant",
                                            "Tuberculosis mortality"))) 



write.xlsx( res_uni2 ,"output/Regression_lm.xlsx", row.names=FALSE, overwrite = TRUE)

