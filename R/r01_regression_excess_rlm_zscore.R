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
  select(-pop)

dlw <- read.xlsx("data/Landwirtschaft.xlsx") %>%
  rename(LW_prop = Anteil) %>%
  select(-Erwerbende, -LW,-MapName) 

hou <- read.xlsx("data/Household.xlsx") %>%
  select(-pop, -Hauser,-Haushalte,-MapName)


hosp <-  read.xlsx("data/Hospitals.xlsx")%>%
  select(-MapName)


# tb <-  read.xlsx("data/TB.xlsx")%>%
#   select(-pop, -Tb.Deaths.1911_1920,-MapName) %>%
#   rename(TB_death = pro_jahr)

# ur <-  read.xlsx("data/Urbanity.xlsx") %>%
#   mutate(
#     city=as.factor(city)
#   ) %>%
#   select(-MapName)

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
  full_join(denspop) %>%
  mutate(dens_doc = Doc_privat/area) %>%
  select(Bezirk, MapName, Doc_privat,Doc_spital,docs = Both, dens_doc) 

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
  mutate(
    # city = as.numeric(city),
    # hosp_pop = hospitals/pop*10000,
    # hosp_pop_n = normalit( hosp_pop),
    # hosp_pop_s = scale( hosp_pop),
    # docs_pop = docs/pop*10000,
    # docs_pop_n= normalit(docs_pop),
    # docs_pop_s= scale(docs_pop),
    docs_p_pop= Doc_privat/pop*10000,
    # docs_p_pop_n= normalit(docs_p_pop),
    docs_p_pop_s= scale(docs_p_pop),
    dens_doc_s= scale(dens_doc),
    # docs_s_pop= Doc_spital/pop*10000,
    # docs_s_pop_n= normalit(docs_s_pop),
    # docs_s_pop_s= scale(docs_s_pop),
    # propmale_n= normalit(propmale),
    propmale_s= scale(propmale),
    # propkids_n= normalit(propkids),
    propkids_s= scale(propkids),
    # prop20_n= normalit(prop20),
    prop20_s= scale(prop20),
    # prop60_n= normalit(prop60),
    prop60_s= scale(prop60),
    In_prop = 100-LW_prop,
    # LW_prop_n= normalit(LW_prop),
    # In_prop_n= normalit(In_prop),
    In_prop_s= scale(In_prop),
    # GDP_n = normalit(GDP),
    GDP_s = scale(GDP),
    # densPop_n= normalit(densPop),
    densPop_s= scale(densPop),
    tb_d_pop = TB_death/pop*10000,
    # tb_d_pop_n = normalit(tb_d_pop),
    tb_d_pop_s = scale(tb_d_pop),
    # Haushalte.pro.Haus_n =normalit(Haushalte.pro.Haus),
    Haushalte.pro.Haus_s =scale(Haushalte.pro.Haus),
    # person.pro.Haushalt_n =normalit(person.pro.Haushalt),
    person.pro.Haushalt_s = scale(person.pro.Haushalt),
    # z_n = normalit(GiZScore),
    excess_s = scale(excess2)) 


function_rlm <- function(v) {
  
  dt1 <- dt_g 

formula_str <- paste("excess_s~",v)
formula <- as.formula(formula_str)

# I assume spatial lag and spatial error term
model <- rlm(formula,data =dt1)
res <- coef(summary(model))

return(res)

}

                     
w11 <- function_rlm("GDP_s")
w12 <- function_rlm("densPop_s")
w13 <- function_rlm("In_prop_s")
w14 <- function_rlm("Haushalte.pro.Haus_s")
w15 <- function_rlm("person.pro.Haushalt_s")
w16 <- function_rlm("propkids_s")
w17 <- function_rlm("prop20_s")
w18 <- function_rlm("prop60_s")
w19 <- function_rlm("propmale_s")
w110 <- function_rlm("dens_doc_s")

w1 <- rbind(w11, w12, w13, w14, w15, w16, w17,w18,w19, w110) %>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  filter( Cofactor=="GDP_s" | Cofactor=="densPop_s" |   Cofactor=="In_prop_s"| Cofactor=="Haushalte.pro.Haus_s"| Cofactor=="person.pro.Haushalt_s"| 
            Cofactor=="propkids_s" |   Cofactor=="dens_doc_s" |  Cofactor=="prop20_s" |   Cofactor=="prop60_s"|
            Cofactor=="propmale_s" ) %>%
  mutate(
    est= round(Value,2),
    Cl = round(Value - 1.96* Std..Error,2),
    Cu = round(Value + 1.96* Std..Error,2),
    Univariate = paste0(est," (",Cl," to ",Cu, ")"),
    Cofactor=recode(Cofactor, 
                    "dens_doc_s" = "Private physicians per km2",
                    "propmale_s"  = "Share of men",
                    "propkids_s"  = "Share of 5-14 years old",
                    "prop20_s"    =  "Share of 20-39 years old",
                    "prop60_s"  = "Share of >= 60 years old",
                    "In_prop_s"     = "Share of industry",
                    "GDP_s" = "GDP per capita",
                    "densPop_s"    = "Population density",
                    "Haushalte.pro.Haus_s"  = "Households per house",
                    "person.pro.Haushalt_s" = "Household size")
  )

                        
write.xlsx( w1,"output/Regression_rlm_excess_scale.xlsx", row.names=FALSE, overwrite = TRUE)



