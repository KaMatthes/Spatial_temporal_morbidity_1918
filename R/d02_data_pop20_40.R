rm(list=ls())

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

dt.1918 <- read_excel_allsheets("data/SPARK_Digitalisierung_1918_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(-c(9,12,15,16, 17, 18,19))

dt.1919 <- read_excel_allsheets("data/SPARK_Digitalisierung_1919_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(-c(9,12))


dt.1920 <- read_excel_allsheets("data/SPARK_Digitalisierung_1920_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(-c(9,12))


# add missing informations, rename Bezirke

dt <- rbind(dt.1918, dt.1919, dt.1920) %>%
  ungroup() %>%
  mutate(
    Startdatum = ymd(Startdatum),
    Enddatum = ymd(Enddatum),
    iso_cw = isoweek(Enddatum),
    iso_cw_y = paste0(Jahr, "-", iso_cw),
    `Bezirks-nummer` = as.character(`Bezirks-nummer`),
    Bezirksname = as.character(Bezirksname),
    Gemeindename = as.character(Gemeindename),
    
    Bezirksname = case_when(
      (!is.na(`Bezirks-nummer`) & Kanton == "AI") ~ "Appenzell-Innerrhoden",
      Gemeindename == "Bezirk" & Bezirksname == "AI" ~ "Appenzell-Innerrhoden",
      Kanton == "GE" ~ "Genève",
      Gemeindename == "Kanton" ~ Kanton,
      TRUE ~ Bezirksname
    ),
    
    Bezirksname = ifelse(Bezirksname == "AI", "Appenzell-Innerrhoden", Bezirksname),
    
    `Bezirks-nummer` = recode(`Bezirks-nummer`, "1601" = "1600", "1602" = "1600"),
    `Bezirks-nummer` = ifelse(Bezirksname == "Appenzell-Innerrhoden", "1600", `Bezirks-nummer`),
    `Bezirks-nummer` = ifelse(Bezirksname == "Genève", "2500" , `Bezirks-nummer`),
    
    Kanton = ifelse(Bezirksname == "Appenzell-Innerrhoden", "AI", Kanton),
    Kanton = recode(Kanton, "Sz" ="SZ"),
    
    
    Fallzahl = ifelse(is.na(Fallzahl), 0, Fallzahl)
  ) %>%
  filter(!iso_cw_y %in% c("NA-NA")) %>%
  droplevels() 


# sum up Bezirke, 11648
dt2 <- dt %>%
  dplyr:: mutate(iso_cw_y = as.factor(iso_cw_y),
                 Gemeindename = as.factor(Gemeindename)) %>%
                 # Bezirksname = ifelse(is.na(Bezirksname), "Appenzell-Innerrhoden",Bezirksname ),
                 # `Bezirks-nummer` =ifelse(is.na(`Bezirks-nummer`), 1600, `Bezirks-nummer`)) %>%
  dplyr::filter(Gemeindename %in% c( "Bezirk")) %>%
  group_by(Bezirksname,iso_cw_y,`Bezirks-nummer`) %>%
  summarise(Fallzahl_Bezirk = sum(Fallzahl)) %>%
  ungroup() %>%
  mutate(
    
  )


 # 1600
dt22 <- dt %>%
  dplyr:: mutate(iso_cw_y = as.factor(iso_cw_y),
                 Gemeindename = as.factor(Gemeindename)) %>%
  # Bezirksname = ifelse(is.na(Bezirksname), "Appenzell-Innerrhoden",Bezirksname ),
  # `Bezirks-nummer` =ifelse(is.na(`Bezirks-nummer`), 1600, `Bezirks-nummer`)) %>%
  dplyr::filter(Gemeindename %in% c( "Kanton")) %>%
  group_by(Kanton,iso_cw_y) %>%
  summarise(Fallzahl_Kanton = sum(Fallzahl))


#  1536
dt222 <- dt %>%
  dplyr:: mutate(iso_cw_y = as.factor(iso_cw_y),
                 Gemeindename = as.factor(Gemeindename)) %>%
  # Bezirksname = ifelse(is.na(Bezirksname), "Appenzell-Innerrhoden",Bezirksname ),
  # `Bezirks-nummer` =ifelse(is.na(`Bezirks-nummer`), 1600, `Bezirks-nummer`)) %>%
  dplyr::filter(Gemeindename %in% c( "Bezirk")) %>%
  group_by(Kanton,iso_cw_y) %>%
  summarise(Fallzahl_Bezirk_Kantone = sum(Fallzahl)) %>%
  ungroup()



# 11649
dt3 <- dt %>%
  dplyr:: mutate(iso_cw_y = as.factor(iso_cw_y),
                 Gemeindename = as.factor(Gemeindename)) %>%
  dplyr::filter(!Gemeindename %in% c("Kanton", "Bezirk")) %>%
  dplyr::group_by( `Bezirks-nummer`,iso_cw_y) %>%
  mutate(fallzahl_gemeinde_bezirk = sum(Fallzahl)) %>%
  ungroup() %>%
  dplyr::group_by(Kanton,iso_cw_y) %>%
  mutate(fallzahl_gemeinde_kanton = sum(Fallzahl)) %>%
  ungroup() %>%
  distinct( `Bezirks-nummer`,iso_cw_y, .keep_all = TRUE) %>%
  left_join(dt22) %>%
  full_join(dt2) %>%
  left_join(dt222) %>%
  filter(!is.na(Bezirksname)) %>%
  filter(!iso_cw_y=="NA-NA") %>%
  droplevels() %>%
  mutate(
    Cases_Bezirk = ifelse(Fallzahl_Bezirk>fallzahl_gemeinde_bezirk,Fallzahl_Bezirk, fallzahl_gemeinde_bezirk)
  ) %>%
  group_by(Kanton,iso_cw_y) %>%
  mutate(Cases_Bezirk_Kanton = sum(    Cases_Bezirk)) %>%
  ungroup() %>%
  mutate( Cases_Kanton = ifelse(Fallzahl_Kanton>Cases_Bezirk_Kanton,Fallzahl_Kanton,Cases_Bezirk_Kanton),
          Kanton_bigger = ifelse(Fallzahl_Kanton>Cases_Bezirk_Kanton,1,0)
         # Kanton = ifelse(Bezirksname=="Appenzell-Innerrhoden","AI", Kanton ),
         # Kanton = ifelse(Bezirksname=="Genève","GE", Kanton )
    ) %>%
  rename(year = Jahr,
         Bezirk = `Bezirks-nummer` ) %>%
  select(-Gemeindename)

         
dt.pop <- read_excel_allsheets("data/pop_age.xlsx") %>%
  do.call(bind_rows, .) %>%
  mutate(Bezirk=as.character(Bezirk)) %>%
  select(-MapName) %>%
  filter(Agegroups %in% c("20-24","25-29","30-34","35-39")) %>%
  group_by(Bezirk, year) %>%
  summarise(pop = sum(pop_age)) %>%
  ungroup() %>%
  mutate(iso_cw =1) %>%
  complete(Bezirk, year = 1918:1920, iso_cw = 1:52) %>%
  distinct(Bezirk, year, iso_cw, .keep_all = TRUE) %>%
  arrange(Bezirk, year, iso_cw) %>%
    group_by(Bezirk) %>%
  mutate(
    # Convert year + iso week to a pseudo-date index
    week_index = (year - min(year)) * 52 + iso_cw,
    pop_interp = approx(x = week_index[!is.na(pop)],
                        y = pop[!is.na(pop)],
                        xout = week_index,
                        method = "linear",
                        rule = 2)$y
  ) %>%
  ungroup() %>%
  mutate(iso_cw_y = paste0(year, "-",iso_cw),
         pop2 = round(pop_interp, 0)) %>%
  select(year, Bezirk, iso_cw_y, pop=pop2)

# 11712   
dt4 <- dt3 %>%
  left_join(dt.pop)

# 7763
dt4r <- dt4 %>%
  filter(Kanton_bigger ==0) %>%
  select(year, Startdatum, Enddatum, Kanton, Bezirk, Bezirksname,iso_cw_y,Cases_Bezirk, pop)

# Alle Fälle Cases_Kanton mehr sind, das heisst zu wenig in Cases_Bezirk

dt5 <- dt4 %>%
  filter(Kanton_bigger ==1) %>%
  group_by(Kanton,iso_cw_y) %>%
  mutate(
    pop.kanton = sum(pop)
    ) %>%
  ungroup() %>%
  mutate(
    prop = pop/pop.kanton,
    Cases_prop = prop * Cases_Kanton,
    Cases_Bezirk_new = ifelse(Cases_prop>Cases_Bezirk,Cases_prop,Cases_Bezirk),
    Cases_Bezirk_new = round(Cases_Bezirk_new,0),
    Cases_Bezirk_new_bigger = ifelse(Cases_Bezirk_new > Cases_Bezirk,1,0)
) %>%
  group_by(Cases_Bezirk_new_bigger, Kanton, iso_cw_y) %>%
  mutate(Cases_used = sum(Cases_Bezirk_new)) %>%
  ungroup()

a <- dt5 %>%
  filter(Bezirk=="111") %>%
  filter(iso_cw_y=="1918-27")

#  Cases_Bezirk_new 1332
dt5r <- dt5 %>%
  filter( Cases_Bezirk_new_bigger ==0) %>%
  select(year, Startdatum, Enddatum, Kanton, Bezirk, Bezirksname,iso_cw_y,Cases_Bezirk= Cases_Bezirk_new,pop)

dt6 <- dt5 %>%
  filter(Cases_Bezirk_new_bigger==0) %>%
  distinct(Kanton, iso_cw_y, .keep_all = TRUE) %>%
  select(Kanton, iso_cw_y,Cases_used )

dt7 <- dt5%>%
  filter(Cases_Bezirk_new_bigger ==1) %>%
  select(-Cases_used ) %>%
  left_join(dt6) 

# Cases_Bezirk_new 493
dt8r <- dt7 %>%
  filter(is.na(Cases_used))  %>%
  select(year, Startdatum, Enddatum, Kanton, Bezirk, Bezirksname,iso_cw_y,Cases_Bezirk= Cases_Bezirk_new,pop)

dt9 <- dt7 %>%
  filter(!is.na(Cases_used)) %>%
  mutate( 
    cases_left = Cases_Kanton - Cases_used
    ) %>%
  group_by(Kanton,iso_cw_y) %>%
  mutate(
    pop.kanton.re = sum(pop)
  )  %>%
ungroup() %>%
  mutate(
    prop_re = pop/pop.kanton.re,
    Cases_prop2 = prop_re *  cases_left,
    Cases_Bezirk_new2 = ifelse(Cases_prop2>Cases_Bezirk,Cases_prop2,Cases_Bezirk),
    Cases_Bezirk_new2 = round(Cases_Bezirk_new2,0),
    Cases_Bezirk_new_bigger2 = ifelse(Cases_Bezirk_new2 > Cases_Bezirk,1,0)
  ) %>%
  group_by(Cases_Bezirk_new_bigger2, Kanton, iso_cw_y) %>%
  mutate(Cases_used2 = sum(Cases_Bezirk_new2)) %>%
  ungroup()


# Caes_Bezirk_new2 715
dt9r <- dt9 %>%
  filter(  Cases_Bezirk_new_bigger2 ==0)  %>%
  select(year, Startdatum, Enddatum, Kanton, Bezirk, Bezirksname,iso_cw_y,Cases_Bezirk= Cases_Bezirk_new2,pop)

dt10 <- dt9 %>%
  filter(Cases_Bezirk_new_bigger2==0) %>%
  distinct(Kanton, iso_cw_y, .keep_all = TRUE) %>%
  select(Kanton, iso_cw_y,Cases_used2 )

# 1179
dt11 <- dt9%>%
  filter(Cases_Bezirk_new_bigger2 ==1) %>%
  select(-Cases_used2 ) %>%
  left_join(dt10) 

# Cases_Bezirk_new2 289
dt12r <- dt11 %>%
  filter(is.na(Cases_used2)) %>%
  select(year, Startdatum, Enddatum, Kanton, Bezirk, Bezirksname,iso_cw_y,Cases_Bezirk= Cases_Bezirk_new2,pop)

dt13 <- dt11 %>%
  filter(!is.na(Cases_used2)) %>%
  mutate( cases_left2 = cases_left - Cases_used2) %>%
  group_by(Kanton,iso_cw_y) %>%
  mutate(
    pop.kanton.re2 = sum(pop)
  )  %>%
  ungroup() %>%
  mutate(
    prop_re2 = pop/pop.kanton.re2,
    Cases_prop3 = prop_re2 *  cases_left2,
    Cases_Bezirk_new3= ifelse(Cases_prop3>Cases_Bezirk,Cases_prop3,Cases_Bezirk),
    Cases_Bezirk_new_bigger3 = ifelse(Cases_Bezirk_new3 > Cases_Bezirk,1,0)
  ) %>%
  group_by(Cases_Bezirk_new_bigger3, Kanton, iso_cw_y) %>%
  mutate(Cases_used3 = sum(Cases_Bezirk_new3)) %>%
  ungroup()

# # Cases_Bezirk_new3 196
dt13r <- dt13%>%
  filter(  Cases_Bezirk_new_bigger3 ==0) %>%
  select(year, Startdatum, Enddatum, Kanton, Bezirk, Bezirksname,iso_cw_y,Cases_Bezirk= Cases_Bezirk_new3,pop)

dt14 <- dt13 %>%
  filter(Cases_Bezirk_new_bigger3==0) %>%
  distinct(Kanton, iso_cw_y, .keep_all = TRUE) %>%
  select(Kanton, iso_cw_y,Cases_used3 )

# 1179
dt15 <- dt13%>%
  filter(Cases_Bezirk_new_bigger3 ==1) %>%
  select(-Cases_used3 ) %>%
  left_join(dt14) 

# Cases_Bezirk_new3 485
dt16r <- dt15 %>%
  filter(is.na(Cases_used3)) %>%
  select(year, Startdatum, Enddatum, Kanton, Bezirk, Bezirksname,iso_cw_y,Cases_Bezirk= Cases_Bezirk_new3,pop)

dt17 <- dt15 %>%
  filter(!is.na(Cases_used3)) %>%
  mutate( cases_left3 = cases_left2 - Cases_used3) %>%
  group_by(Kanton,iso_cw_y) %>%
  mutate(
    pop.kanton.re3 = sum(pop)
  )  %>%
  ungroup() %>%
  mutate(
    prop_re3 = pop/pop.kanton.re3,
    Cases_prop4 = prop_re3 *  cases_left3,
    Cases_Bezirk_new4= ifelse(Cases_prop4>Cases_Bezirk,Cases_prop4,Cases_Bezirk),
    Cases_Bezirk_new_bigger4 = ifelse(Cases_Bezirk_new4 > Cases_Bezirk,1,0),
    diff = Cases_Bezirk_new4-Cases_Bezirk,
    diff_prop= diff/Cases_Bezirk_new4*100
  )


# # Cases_Bezirk_new4  439
dt17r <- dt17  %>%
  select(year, Startdatum, Enddatum, Kanton, Bezirk, Bezirksname,iso_cw_y,Cases_Bezirk= Cases_Bezirk_new4,pop)


#10,587
dt4t <- dt4%>%
  select(Bezirk,iso_cw_y)

#11648

dt.pop_all <- read_excel_allsheets("data/Population.xlsx") %>%
  do.call(bind_rows, .) %>%
  mutate(Bezirk=as.character(Bezirk)) %>%
  select(-MapName) %>%
  mutate(iso_cw =1) %>%
  complete(Bezirk, year = 1918:1920, iso_cw = 1:52) %>%
  distinct(Bezirk, year, iso_cw, .keep_all = TRUE) %>%
  arrange(Bezirk, year, iso_cw) %>%
  group_by(Bezirk) %>%
  mutate(
    # Convert year + iso week to a pseudo-date index
    week_index = (year - min(year)) * 52 + iso_cw,
    pop_interp = approx(x = week_index[!is.na(pop)],
                        y = pop[!is.na(pop)],
                        xout = week_index,
                        method = "linear",
                        rule = 2)$y
  ) %>%
  ungroup() %>%
  mutate(iso_cw_y = paste0(year, "-",iso_cw),
         pop2 = round(pop_interp, 0)) %>%
  select(year, Bezirk, iso_cw_y, pop=pop2)

dt_all <- rbind(dt4r, dt5r,dt8r,dt9r,dt12r,dt13r,dt16r,dt17r) %>%
  rename(pop20_40 = pop) %>%
  left_join(dt.pop_all ) %>%
  mutate(
    mx = Cases_Bezirk/pop,
    incidence = mx*10000,
    wave = case_when(Enddatum <=ymd("1918-08-31") ~  1,
                     Enddatum >=ymd("1918-09-01") & Enddatum <=ymd("1919-12-31")~2,
                     # Enddatum >=ymd("1919-01-01") & Enddatum <=ymd("1919-04-30")~3,
                     Enddatum >=ymd("1920-01-01") & Enddatum <=ymd("1920-05-30")~3)
  )

write.xlsx( dt_all ,"data/Faelle_Bezirke_total_20_40.xlsx", row.names=FALSE, overwrite = TRUE)

dt.pop.y <- read_excel_allsheets("data/Population.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(-MapName) %>%
  mutate(
    Bezirk=as.character(Bezirk)
  )

dt_year <- dt_all %>%
group_by(year, Bezirk, Bezirksname) %>%
  mutate(
    Cases_Bezirk_year = sum(Cases_Bezirk)
    ) %>%
  ungroup() %>%
  select(year, Kanton, Bezirk, Bezirksname, Cases_Bezirk_year) %>%
  distinct(year, Bezirk, Bezirksname, .keep_all = TRUE) %>%
  left_join(dt.pop.y) %>%
  mutate(
    mx = Cases_Bezirk_year/pop,
    incidence = mx*10000
  )

write.xlsx( dt_year ,"data/Faelle_Bezirke_total_20_40_year.xlsx", row.names=FALSE, overwrite = TRUE)

dt_wave <- dt_all %>%
  group_by(wave, Bezirk, Bezirksname) %>%
  mutate(
    Cases_Bezirk_wave = sum(Cases_Bezirk)
  ) %>%
  ungroup() %>%
  select(year,wave, Kanton, Bezirk, Bezirksname, Cases_Bezirk_wave) %>%
  distinct(wave, Bezirk, Bezirksname,   Cases_Bezirk_wave,.keep_all = TRUE) %>%
  left_join(dt.pop.y) %>%
  mutate(
    mx = Cases_Bezirk_wave/pop,
    incidence = mx*10000
  ) 

write.xlsx( dt_wave ,"data/Faelle_Bezirke_total_20_40_wave.xlsx", row.names=FALSE, overwrite = TRUE)

dt_max <- dt_all %>%
  group_by(wave, Bezirk) %>%
  slice_max(., order_by= incidence)  %>%
  ungroup() %>%
  mutate(
    incidence= ifelse(incidence==0, NA, incidence)
  )

write.xlsx( dt_max ,"data/Faelle_Bezirke_total_20_40_zeitreihe.xlsx", row.names=FALSE, overwrite = TRUE)
