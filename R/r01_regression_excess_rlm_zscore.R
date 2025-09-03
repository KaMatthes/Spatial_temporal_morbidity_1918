normalit <-function(m){
  (m - min(m))/(max(m)-min(m))
}


dt<- read.xlsx("data/expected_death_inla.xlsx")  %>%
  mutate(
    Bezirk = as.numeric(Bezirk))
  # ) %>%
  # filter(!Bezirk  %in% c(1600))


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


inc <- read.xlsx("data/HotSpot_totalpop_1918_06_5N_Table_Tablel.xlsx") %>%
  rename( MapName =BEZNA,
          Bezirk  =BEZNR ) %>%
  # filter(!Bezirk %in% 1600 ) %>%
  select(GiZScore, Bezirk)


dt_g <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  # filter(!Bezirk %in% 1600 ) %>%
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
  full_join(inc) %>%
  # filter(!Bezirk %in% 1600 ) %>%
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
    excess_n = normalit(excess2),
    excess_s = scale(excess2),
    GiZScore_n = normalit(GiZScore)
  ) %>%
  ungroup()


function_rlm <- function(v) {
  
  dt1 <- dt_g 

formula_str <- paste("excess_s~",v)
formula <- as.formula(formula_str)

# I assume spatial lag and spatial error term
model <- rlm(formula,data =dt1)
res <- coef(summary(model))

return(res)

}

                     
w11 <- function_rlm("GDP_n")
w12 <- function_rlm("densPop_n")
w13 <- function_rlm("In_prop_n")
w14 <- function_rlm("Haushalte.pro.Haus_n")
w15 <- function_rlm("person.pro.Haushalt_n")
w16 <- function_rlm("hospitals")
w17 <- function_rlm("tb_d_pop_n")
w19 <- function_rlm("city")
w120 <- function_rlm("hosp_pop_n")
w121 <- function_rlm("propkids_n")
w122 <- function_rlm("prop20_n")
w123 <- function_rlm("prop60_n")
w124 <- function_rlm("propmale_n")
w125 <- function_rlm("docs_pop_n")
w126 <- function_rlm("docs_p_pop_n")
w127 <- function_rlm("docs_s_pop_n")
w128 <- function_rlm("GiZScore_n")

w2 <- rbind(w11, w12, w13, w14, w15, w16, w17, w19, w120,w121,w122, w123,w124,w125,w126,w127, w128) %>%
  data.frame() %>%
  mutate(wave = 1)%>%
  mutate(Cofactor=row.names(.)) %>%
  filter( Cofactor=="GDP_n" | Cofactor=="densPop_n" |   Cofactor=="In_prop_n"| Cofactor=="Haushalte.pro.Haus_n"| Cofactor=="person.pro.Haushalt_n"| 
            Cofactor=="hospitals" | Cofactor=="tb_d_pop_n"  |  Cofactor=="docs_pop_n"  |  Cofactor=="docs_p_pop_n" |  Cofactor=="docs_s_pop_n"|
            Cofactor=="city1" |   Cofactor=="hosp_pop_n" |   Cofactor=="propkids_n" |   Cofactor=="prop20_n" |   Cofactor=="prop60_n"|
            Cofactor=="propmale_n" | Cofactor=="GiZScore_n" ) %>%
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
                         "docs_s_pop_n" = "Hospital doctors per 10'000",
                         "GiZScore_n" = "Incidence")
  )
                        
write.xlsx( w2,"output/Regression_rlm_excess_scale.xlsx", row.names=FALSE, overwrite = TRUE)


ggplot(dt_g) +
  geom_point(aes(GiZScore_n, excess_n))


model <- rlm(excess2 ~ densPop, dt_g)
dt_m <- dt_g %>%
  mutate(res_densp = model$residuals)


tm_shape(dt_m)  +
  tm_fill("res_densp")

