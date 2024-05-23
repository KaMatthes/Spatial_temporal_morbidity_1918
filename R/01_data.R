rm(list=ls())

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

dt.1918 <- read_excel_allsheets("data/SPARK_Digitalisierung_1918.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(1:8, 10:11)

dt.1919 <- read_excel_allsheets("data/SPARK_Digitalisierung_1919.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(1:8, 10:11)

dt.1920 <- read_excel_allsheets("data/SPARK_Digitalisierung_1920.xlsx") %>%
  do.call(bind_rows, .) %>%
  select(1:8, 10:11)

dt <- rbind(dt.1918,dt.1919,dt.1920) %>%
  ungroup()


dt <- dt %>%
  mutate(Startdatum = ymd(Startdatum),
         Enddatum = ymd(Enddatum),
         iso_cw = isoweek(Enddatum),
         iso_cw_y = paste0(Jahr, "-",iso_cw),
         Bezirksname = ifelse(!is.na(`Bezirks-nummer`) & Kanton=="AI", "Appenzell-Innerrhoden",Bezirksname),
         `Bezirks-nummer` = as.character(`Bezirks-nummer`),
         `Bezirks-nummer` = ifelse(Kanton=="GE","2500", `Bezirks-nummer`),
          Bezirksname = ifelse( `Bezirks-nummer` =="2500", "Genève",Bezirksname),
         `Bezirks-nummer` = recode(`Bezirks-nummer`, "1601"="1600", "1602" ="1600"),
         Gemeindename = ifelse(Gemeindename=="Kanton" & Kanton=="AI", "Bezirk",Gemeindename),
         Gemeindename = ifelse( is.na(Gemeindename), "Bezirk",Gemeindename),
         Gemeindename = ifelse(is.na(Bezirksname), "Kanton",Gemeindename),
         Gemeindename = ifelse(Bezirksname =="Genève" &  !Gemeindename =="Bezirk", "Genève",Gemeindename),
         Fallzahl = ifelse(is.na(Fallzahl), 0, Fallzahl)) 



dt2<- dt %>%
  dplyr:: mutate(iso_cw_y = as.factor(iso_cw_y),
                 Gemeindename = as.factor(Gemeindename),
                 Bezirksname = ifelse(is.na(Bezirksname),"Appenzell-Innerrhoden",Bezirksname ),
                 `Bezirks-nummer` =ifelse(is.na(`Bezirks-nummer`), 1600, `Bezirks-nummer`)) %>%
  dplyr::filter(Gemeindename %in% c( "Bezirk")) %>%
  group_by(Bezirksname,iso_cw_y,`Bezirks-nummer`) %>%
  summarise(Fallzahl_t = sum(Fallzahl))


dt3 <- dt %>%
  dplyr:: mutate(iso_cw_y = as.factor(iso_cw_y),
                 Gemeindename = as.factor(Gemeindename)) %>%
  dplyr::filter(!Gemeindename %in% c("Kanton", "Bezirk")) %>%
  dplyr::group_by( `Bezirks-nummer`,iso_cw_y) %>%
  mutate(fallzahl_bezirk = sum(Fallzahl)) %>%
  ungroup() %>%
  distinct( `Bezirks-nummer`,iso_cw_y, .keep_all = TRUE)  %>%
  full_join(dt2) %>%
  filter(!is.na(Bezirksname)) %>%
  mutate(
         Cases = ifelse(Fallzahl_t>fallzahl_bezirk,Fallzahl_t, fallzahl_bezirk))

write.xlsx(dt3 ,paste0("Faelle_Bezirk.xlsx"), row.names=FALSE, overwrite = TRUE)


