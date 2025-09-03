normalit <-function(m){
  (m - min(m))/(max(m)-min(m))
}


pop<- read.xlsx("data/Faelle_Bezirke_total_pop_wave.xlsx") %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  filter(!Kanton %in% c("AI")) %>%
  select(Bezirk, wave, pop)


dt1 <- read.xlsx("data/HotSpot_totalpop_W1_06_5N_Table_Tablel.xlsx") %>%
  
  mutate(wave =1)

dt2 <- read.xlsx("data/HotSpot_totalpop_W2_06_5N_Table_Tablel.xlsx") %>%
  mutate(wave =2)

dt3 <- read.xlsx("data/HotSpot_totalpop_1920_06_5N_Table_Tablel.xlsx") %>%
  mutate(wave =3)

dt <- rbind(dt1,dt2, dt3) %>%
  rename( MapName =BEZNA,
          Bezirk  =BEZNR ) %>%
  filter(!Bezirk %in% 1600 ) 

# dt <- read.xlsx("data/Faelle_Bezirke_total_20_40_wave.xlsx") %>%
#   mutate(
#     Bezirk = as.numeric(Bezirk),
#   ) %>%
#   filter(!Kanton %in% c("AI"))


gpd <- read.xlsx("data/GDP.xlsx") %>%
  select(-MapName)


denspop <- read.xlsx("data/DensPop.xlsx") %>%
  select(-pop, -area, -MapName)

dlw <- read.xlsx("data/Landwirtschaft.xlsx") %>%
  rename(LW_prop = Anteil) %>%
  select(-Erwerbende, -LW,-MapName) 

hou <- read.xlsx("data/Household.xlsx") %>%
  select(-pop, -Hauser,-Haushalte,-MapName)


hosp <-  read.xlsx("data/Hospitals.xlsx")%>%
  select(-MapName)


tb <-  read.xlsx("data/TB.xlsx")%>%
  select(-pop, -Tb.Deaths.1911_1920,-MapName) %>%
  rename(TB_death = pro_jahr)

ur <-  read.xlsx("data/Urbanity.xlsx") %>%
  mutate(
    city=as.factor(city)
  ) %>%
  select(-MapName)

age20 <-  read.xlsx("data/prop_age20.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(prop20 = prop) %>%
  select(-year,-MapName)

age60 <-  read.xlsx("data/prop_age60.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(prop60 =prop) %>%
  select(-year,-MapName)

agekids <-  read.xlsx("data/prop_schoolkids.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(propkids =prop) %>%
  select(-year,-MapName)

propmale <-  read.xlsx("data/prop_sex.xlsx") %>%
  rename(propmale =prop_male)  %>%
  select(-MapName)

docs <-  read.xlsx("data/Aerzte1910.xlsx") %>%
  select(Bezirk, MapName, Doc_privat,Doc_spital,docs = Both,-MapName)

dt_g <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  filter(!Bezirk %in% 1600 ) %>%
  select(Bezirk, geometry) %>%
  full_join(pop) %>%
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
  filter(!Bezirk %in% 1600 ) %>%
  filter(!is.na(wave)) %>%
group_by(wave) %>%
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
  ) %>%
  ungroup()


function_rlm <- function(w,v) {
  
  dt1 <- dt_g %>%
  filter(wave ==w)


formula_str <- paste("GiZScore~",v)
formula <- as.formula(formula_str)

# I assume spatial lag and spatial error term
model <- rlm(formula,data =dt1)
res <- coef(summary(model))

return(res)

}


Mod1 <- coef(summary(rlm(incidence ~ GDP, data=dt_g)))
                     
w11 <- function_rlm(1,"GDP_n")
w12 <- function_rlm(1,"densPop_n")
w13 <- function_rlm(1,"In_prop_n")
w14 <- function_rlm(1,"Haushalte.pro.Haus_n")
w15 <- function_rlm(1,"person.pro.Haushalt_n")
w16 <- function_rlm(1,"hospitals")
w17 <- function_rlm(1,"tb_d_pop_n")
w19 <- function_rlm(1,"city")
w120 <- function_rlm(1,"hosp_pop_n")
w121 <- function_rlm(1,"propkids_n")
w122 <- function_rlm(1,"prop20_n")
w123 <- function_rlm(1,"prop60_n")
w124 <- function_rlm(1,"propmale_n")
w125 <- function_rlm(1,"docs_pop_n")
w126 <- function_rlm(1,"docs_p_pop_n")
w127 <- function_rlm(1,"docs_s_pop_n")

w1 <- rbind(w11, w12, w13, w14, w15, w16, w17, w19, w120,w121,w122, w123,w124,w125,w126,w127) %>%
  data.frame() %>%
  mutate(wave = 1)%>%
  mutate(Cofactor=row.names(.))

w21 <- function_rlm(2,"GDP_n")
w22 <- function_rlm(2,"densPop_n")
w23 <- function_rlm(2,"In_prop_n")
w24 <- function_rlm(2,"Haushalte.pro.Haus_n")
w25 <- function_rlm(2,"person.pro.Haushalt_n")
w26 <- function_rlm(2,"hospitals")
w27 <- function_rlm(2,"tb_d_pop_n")
w29 <- function_rlm(2,"city")
w220 <- function_rlm(2,"hosp_pop_n")
w221 <- function_rlm(2,"propkids_n")
w222 <- function_rlm(2,"prop20_n")
w223 <- function_rlm(2,"prop60_n")
w224 <- function_rlm(2,"propmale_n")
w225 <- function_rlm(2,"docs_pop_n")
w226 <- function_rlm(2,"docs_p_pop_n")
w227 <- function_rlm(2,"docs_s_pop_n")


w2 <- rbind(w21, w22, w23, w24, w25, w26, w27, w29, w220,w221,w222,w223,w224,w225,w226,w227) %>%
  data.frame() %>%
  mutate(wave = 2) %>%
  mutate(Cofactor=row.names(.))


w31 <- function_rlm(3,"GDP_n")
w32 <- function_rlm(3,"densPop_n")
w33 <- function_rlm(3,"In_prop_n")
w34 <- function_rlm(3,"Haushalte.pro.Haus_n")
w35 <- function_rlm(3,"person.pro.Haushalt_n")
w36 <- function_rlm(3,"hospitals")
w37 <- function_rlm(3,"tb_d_pop_n")
w39 <- function_rlm(3,"city")
w320 <- function_rlm(3,"hosp_pop_n")
w321 <- function_rlm(3,"propkids_n")
w322 <- function_rlm(3,"prop20_n")
w323 <- function_rlm(3,"prop60_n")
w324 <- function_rlm(3,"propmale_n")
w325 <- function_rlm(3,"docs_pop_n")
w326 <- function_rlm(3,"docs_p_pop_n")
w327 <- function_rlm(3,"docs_s_pop_n")

# w321 <- function_spatial(5, 3,"IncW2")

w3<- rbind(w31, w32, w33, w34, w35, w36, w37, w39, w320,w321,w322, w323,w324,w325,w326,w327) %>%
  data.frame() %>%
  mutate(wave = 3)  %>%
  mutate(Cofactor=row.names(.))


res_uni2 <- rbind(w1, w2, w3) %>%
  filter( Cofactor=="GDP_n" | Cofactor=="densPop_n" |   Cofactor=="In_prop_n"| Cofactor=="Haushalte.pro.Haus_n"| Cofactor=="person.pro.Haushalt_n"| 
            Cofactor=="hospitals" | Cofactor=="tb_d_pop_n" |    Cofactor=="docs_pop_n"  |  Cofactor=="docs_p_pop_n" |  Cofactor=="docs_s_pop_n"|
            Cofactor=="city1" |   Cofactor=="hosp_pop_n" |   Cofactor=="propkids_n" |   Cofactor=="prop20_n" |   Cofactor=="prop60_n"|
            Cofactor=="propmale_n" ) %>%
  mutate(
    est= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(est," (",Cl,"-",Cu, ")"),
         Cofactor=recode(Cofactor, 
                         "GDP_n" = "GDP",
                         "densPop_n" = "Population density",
                         "propkids_n" = "Share of 5-14 year-olds",
                         "prop60_n" = "Share of >=60 year-olds",
                         "prop20_n" = "Share of 20-40 year-olds",
                         "propmale_n" = "Share of men",
                         "In_prop_n" = "Share industry",
                         "tb_d_pop_n" = "Tuberculosis mortality",
                        
                         "city1" = "Urbanicity (Ref: Rural)",
                         "Haushalte.pro.Haus_n" = "Households per house",
                         "person.pro.Haushalt_n" = "Household size",
                         "Hospitals" = "Number of hospitals",
                         "hosp_pop_n" = "Hospitals per 10'000",
                         "docs_pop_n" = "Doctors per 10'000",
                         "docs_p_pop_n" = "Private doctors per 10'000",
                         "docs_s_pop_n" = "Hospital doctors per 10'000")
  )
                        
write.xlsx( res_uni2 ,"output/Regression_rlm_zscore.xlsx", row.names=FALSE, overwrite = TRUE)
