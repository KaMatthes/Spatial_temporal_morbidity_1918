normalit <-function(m){
  (m - min(m))/(max(m)-min(m))
}


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
    incidence = mx*10000,
    hosp_pop= hospitals/pop*10000,
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
    LW_prop_n= normalit(LW_prop),
    GDP_n= normalit(GDP),
    densPop_n= normalit(densPop),
    tb_d_pop = TB_death/pop*10000,
    tb_i_pop = TB_inc
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


formula_str <- paste("incidence~",v)
formula <- as.formula(formula_str)

# I assume spatial lag and spatial error term
model <- rlm(formula,data =dt1)
res <- coef(summary(model))

return(res)

}


Mod1 <- coef(summary(rlm(incidence ~ GDP, data=dt_g)))
                     
w11 <- function_rlm(1,"GDP_n")
w12 <- function_rlm(1,"densPop_n")
w13 <- function_rlm(1,"LW_prop_n")
w14 <- function_rlm(1,"Haushalte.pro.Haus")
w15 <- function_rlm(1,"person.pro.Haushalt")
w16 <- function_rlm(1,"hospitals")
w17 <- function_rlm(1,"tb_d_pop")
w18 <- function_rlm(1,"tb_i_pop")
w19 <- function_rlm(1,"city")
w120 <- function_rlm(1,"hosp_pop")
w121 <- function_rlm(1,"propkids_n")
w122 <- function_rlm(1,"prop20_n")
w123 <- function_rlm(1,"prop60_n")
w124 <- function_rlm(1,"propmale_n")
w125 <- function_rlm(1,"docs_pop_n")
w126 <- function_rlm(1,"docs_p_pop_n")
w127 <- function_rlm(1,"docs_s_pop_n")


w1 <- rbind(w11, w12, w13, w14, w15, w16, w17, w18, w19, w120,w121,w122, w123,w124, w125,w126,w127) %>%
  data.frame() %>%
  mutate(wave = "first")%>%
  mutate(Cofactor=row.names(.))

w21 <- function_rlm(2,"GDP_n")
w22 <- function_rlm(2,"densPop_n")
w23 <- function_rlm(2,"LW_prop_n")
w24 <- function_rlm(2,"Haushalte.pro.Haus")
w25 <- function_rlm(2,"person.pro.Haushalt")
w26 <- function_rlm(2,"hospitals")
w27 <- function_rlm(2,"tb_d_pop")
w28 <- function_rlm(2,"tb_i_pop")
w29 <- function_rlm(2,"city")
w220 <- function_rlm(2,"hosp_pop")
w221 <- function_rlm(2,"propkids_n")
w222 <- function_rlm(2,"prop20_n")
w223 <- function_rlm(2,"prop60_n")
w224 <- function_rlm(2,"propmale_n")
w225 <- function_rlm( 2,"docs_pop_n")
w226 <- function_rlm(2,"docs_p_pop_n")
w227 <- function_rlm(2,"docs_s_pop_n")

w2 <- rbind(w21, w22, w23, w24, w25, w26, w27, w28, w29, w220,w221,w222,w223,w224, w225,w226, w227) %>%
  data.frame() %>%
  mutate(wave = "second") %>%
  mutate(Cofactor=row.names(.))


w31 <- function_rlm(3,"GDP_n")
w32 <- function_rlm(3,"densPop_n")
w33 <- function_rlm(3,"LW_prop_n")
w34 <- function_rlm(3,"Haushalte.pro.Haus")
w35 <- function_rlm(3,"person.pro.Haushalt")
w36 <- function_rlm(3,"hospitals")
w37 <- function_rlm(3,"tb_d_pop")
w38 <- function_rlm(3,"tb_i_pop")
w39 <- function_rlm(3,"city")
w320 <- function_rlm(3,"hosp_pop")
w321 <- function_rlm(3,"propkids_n")
w322 <- function_rlm(3,"prop20_n")
w323 <- function_rlm(3,"prop60_n")
w324 <- function_rlm(3,"propmale_n")
w325 <- function_rlm(3,"docs_pop_n")
w326 <- function_rlm(3,"docs_p_pop_n")
w327 <- function_rlm(3,"docs_s_pop_n")
# w321 <- function_spatial(5, 3,"IncW2")

w3<- rbind(w31, w32, w33, w34, w35, w36, w37, w38, w39, w320,w321,w322, w323,w324, w325, w326, w327) %>%
  data.frame() %>%
  mutate(wave = "third")  %>%
  mutate(Cofactor=row.names(.))


res_uni2 <- rbind(w1, w2, w3) %>%
  filter( Cofactor=="GDP_n" | Cofactor=="densPop_n" |   Cofactor=="LW_prop_n"| Cofactor=="Haushalte.pro.Haus"| Cofactor=="person.pro.Haushalt"| 
            Cofactor=="hospitals" | Cofactor=="tb_d_pop" |   Cofactor=="tb_i_pop"| 
            Cofactor=="city1" |   Cofactor=="hosp_pop" |   Cofactor=="propkids_n" |   Cofactor=="prop20_n" |   Cofactor=="prop60_n"|
            Cofactor=="propmale_n" | Cofactor=="docs_pop_n"  | Cofactor=="docs_p_pop_n" | Cofactor=="docs_s_pop_n") %>%
  mutate(est= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(est," (",Cl,"-",Cu, ")"))
         ,
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



write.xlsx( res_uni2 ,"output/Regression_rlm.xlsx", row.names=FALSE, overwrite = TRUE)

summary(lm(docs_p_pop_n ~ GDP_n), dt_g)

ggplot(dt_g) +
  geom_point(aes(GDP_n, docs_p_pop_n))


ggplot(dt_g) +
  geom_boxplot(aes(city, GDP))

summary(lm(GDP ~city), dt_g)

ggplot(dt_g) +
  geom_boxplot(aes(city,Haushalte.pro.Haus))

summary(lm(Haushalte.pro.Haus~city, dt_g))


ggplot(dt_g) +
  geom_point(aes(LW_prop,person.pro.Haushalt))

summary(lm(person.pro.Haushalt~LW_prop, dt_g))