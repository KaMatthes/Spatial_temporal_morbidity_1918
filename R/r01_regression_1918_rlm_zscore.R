normalit <-function(m){
  (m - min(m))/(max(m)-min(m))
}


pop<- read.xlsx("data/Faelle_Bezirke_total_pop_wave.xlsx") %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  filter(!Kanton %in% c("AI"),
         year %in% 1918,
         wave %in% 1) %>%
  select(Bezirk, wave, pop)


dt <- read.xlsx("data/HotSpot_totalpop_1918_06_5N_Table_Tablel.xlsx") %>%
  rename( MapName =BEZNA,
          Bezirk  =BEZNR ) %>%
  filter(!Bezirk %in% 1600 ) 


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
    docs_pop_s= scale(docs_pop),
    docs_p_pop= Doc_privat/pop*10000,
    docs_p_pop = ifelse(Bezirk %in% c(1811, 1812,1808),8 ,docs_p_pop),
    docs_p_pop_n= normalit(docs_p_pop),
    docs_p_pop_s= scale(docs_p_pop),
    docs_s_pop= Doc_spital/pop*10000,
    docs_s_pop = ifelse(docs_s_pop >90 , 90, docs_p_pop),
    docs_s_pop_n= normalit(docs_s_pop),
    docs_s_pop_s= scale(docs_s_pop),
    propmale_n= normalit(propmale),
    propmale_s= scale(propmale),
    propkids_n= normalit(propkids),
    propkids_s =  scale(propkids),
    prop20_n= normalit(prop20),
    prop60_n= normalit(prop60),
    prop20_s= scale(prop20),
    prop60_s= scale(prop60),
    In_prop = 100-LW_prop,
    LW_prop_n= normalit(LW_prop),
    In_prop_n= normalit(In_prop),
    GDP_n= normalit(GDP),
    densPop_n= normalit(densPop),
    In_prop_s= scale(In_prop),
    GDP_s= scale(GDP),
    densPop_s= scale(densPop),
    tb_d_pop = TB_death/pop*10000,
    tb_d_pop_n = normalit(tb_d_pop),
    tb_d_pop_s = scale(tb_d_pop),
    Haushalte.pro.Haus_n =normalit(Haushalte.pro.Haus),
    person.pro.Haushalt_n =normalit(person.pro.Haushalt),
    Haushalte.pro.Haus_s = scale(Haushalte.pro.Haus),
    person.pro.Haushalt_s =scale(person.pro.Haushalt),
      z_n = normalit(GiZScore),
      s_n = scale(GiZScore)) %>%
  ungroup()  

function_rlm <- function(w,v) {
  
 dt1 <- dt_g 

  
#   
# neighbours <- st_knn(dt1$geometry, k = 5)
# listw <- nb2listw(neighbours)
# gi.fixed <- localG(dt1$incidence_1918, listw)
#   
#   dt1 <- dt1 %>%
#     mutate(Gi = round(as.matrix(gi.fixed),5))
#   
# a <- dt1 %>%
#   select(Gi,GiZScore) %>%
#   mutate(Gi=as.numeric(Gi))
# 
# 
# tm_shape(a)  +
#   tm_fill("Gi")
# 
# tm_shape(a)  +
#   tm_fill("GiZScore")

formula_str <- paste("GiZScore~",v)
formula <- as.formula(formula_str)

# I assume spatial lag and spatial error term
model <- rlm(formula,data =dt1)
res <- coef(summary(model))

return(res)

}

                     
w11 <- function_rlm(1,"GDP_s")
w12 <- function_rlm(1,"densPop_s")
w13 <- function_rlm(1,"In_prop_s")
w14 <- function_rlm(1,"Haushalte.pro.Haus_s")
w15 <- function_rlm(1,"person.pro.Haushalt_s")
w17 <- function_rlm(1,"tb_d_pop_s")
w19 <- function_rlm(1,"city")
w121 <- function_rlm(1,"propkids_s")
w122 <- function_rlm(1,"prop20_s")
w123 <- function_rlm(1,"prop60_s")
w124 <- function_rlm(1,"propmale_s")
w125 <- function_rlm(1,"docs_pop_s")
w126 <- function_rlm(1,"docs_p_pop_s")
w127 <- function_rlm(1,"docs_s_pop_s")

w1 <- rbind(w11, w12, w13, w14, w15, w17, w19,w121,w122, w123,w124,w125,w126,w127) %>%
  data.frame() %>%
  mutate(wave = 1)%>%
  mutate(Cofactor=row.names(.)) %>%
  filter( Cofactor=="GDP_s" | Cofactor=="densPop_s" |   Cofactor=="In_prop_s"| Cofactor=="Haushalte.pro.Haus_s"| Cofactor=="person.pro.Haushalt_s"| 
 Cofactor=="tb_d_pop_s" |    Cofactor=="docs_pop_s"  |  Cofactor=="docs_p_pop_s" |  Cofactor=="docs_s_pop_s"|
            Cofactor=="city1"  |   Cofactor=="propkids_s" |   Cofactor=="prop20_s" |   Cofactor=="prop60_s"|
            Cofactor=="propmale_s" ) %>%
  mutate(
    est= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(est," (",Cl,"-",Cu, ")"),
         Cofactor=recode(Cofactor, 
                         "GDP_s" = "GDP",
                         "densPop_s" = "Population density",
                         "propkids_s" = "Share of 5-14 year-olds",
                         "prop60_s" = "Share of >=60 year-olds",
                         "prop20_s" = "Share of 20-39 year-olds",
                         "propmale_s" = "Share of men",
                         "In_prop_s" = "Share industry",
                         "tb_d_pop_s" = "Tuberculosis mortality",
                         "city1" = "Urbanicity (Ref: Rural)",
                         "Haushalte.pro.Haus_s" = "Households per house",
                         "person.pro.Haushalt_s" = "Household size",
                         "docs_pop_s" = "Doctors per 10'000",
                         "docs_p_pop_s" = "Private doctors per 10'000",
                         "docs_s_pop_s" = "Hospital doctors per 10'000")
  )
                        
write.xlsx( w1 ,"output/Regression_rlm_zscore_1918_2.xlsx", row.names=FALSE, overwrite = TRUE)



