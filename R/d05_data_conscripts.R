rm(list=ls())

read_excel_allsheets2 <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,col_types = "text"))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

dt.1918.2 <- read_excel_allsheets2("data/SPARK_Digitalisierung_1918_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select("Kaserne","Kaserne_nr")

dt.1919.2 <- read_excel_allsheets2("data/SPARK_Digitalisierung_1919_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select("Kaserne","Kaserne_nr")

dt.1920.2 <- read_excel_allsheets2("data/SPARK_Digitalisierung_1920_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select("Kaserne","Kaserne_nr")

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

dt.1918 <- read_excel_allsheets("data/SPARK_Digitalisierung_1918_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(-c(9,12,15,16, 17, 18,19, "Kaserne","Kaserne_nr")) %>%
  cbind(dt.1918.2)

a <- dt.1918 %>%
  filter(Kaserne ==1)

dt.1919 <- read_excel_allsheets("data/SPARK_Digitalisierung_1919_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(-c(9,12, "Kaserne","Kaserne_nr")) %>%
  cbind(dt.1919.2)


dt.1920 <- read_excel_allsheets("data/SPARK_Digitalisierung_1920_bearbeitet.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(-c(9,12, "Kaserne","Kaserne_nr")) %>%
  cbind(dt.1920.2)

# add missing informations, rename Bezirke

dt <- rbind(dt.1918, dt.1919, dt.1920) %>%
  filter(Kaserne %in% "1") %>%
  ungroup() %>%
  mutate(
    Kaserne_nr = as.numeric(Kaserne_nr),
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
    `Bezirks-nummer` = ifelse(Bezirksname == "ZG", "900" , `Bezirks-nummer`),
    
    Kanton = ifelse(Bezirksname == "Appenzell-Innerrhoden", "AI", Kanton),
    Kanton = recode(Kanton, "Sz" ="SZ"),
    Bezirksname = recode(Kanton, "Zug" ="Kt. Zug"),
    

  ) %>%
  filter(!iso_cw_y %in% c("NA-NA")) %>%
  droplevels() %>%
  rename(Bezirk=  `Bezirks-nummer`)

dt2 <- dt %>%
  group_by(Startdatum) %>%
  summarise(Cases = sum(Kaserne_nr)) %>%
  ungroup() 

write.xlsx( dt2 ,"data/Faelle_Bezirke_Conscrips.xlsx", row.names=FALSE, overwrite = TRUE)


dt3 <- dt %>%
  mutate(
    wave = case_when(Enddatum <=ymd("1918-08-31") ~  1,
                     Enddatum >=ymd("1918-09-01") & Enddatum <=ymd("1919-12-31")~2,
                     # Enddatum >=ymd("1919-01-01") & Enddatum <=ymd("1919-04-30")~3,
                     Enddatum >=ymd("1920-01-01") & Enddatum <=ymd("1920-05-30")~3)
  ) %>%
  group_by(wave,Bezirk, Bezirksname) %>%
  summarise(Cases = sum(Kaserne_nr)) %>%
  ungroup()

  
write.xlsx( dt3 ,"data/Faelle_Bezirke_Conscrips_wave.xlsx", row.names=FALSE, overwrite = TRUE)