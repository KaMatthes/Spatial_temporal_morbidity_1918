read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

dt.1918 <- read_excel_allsheets("data/SPARK_Digitalisierung_1918.xlsx") %>%
  do.call(rbind.fill, .) %>%
  select(1:8, 10:11)

dt.1919 <- read_excel_allsheets("data/SPARK_Digitalisierung_1919.xlsx") %>%
  do.call(rbind.fill, .) %>%
  select(1:8, 10:11)

dt.1920 <- read_excel_allsheets("data/SPARK_Digitalisierung_1920.xlsx") %>%
  do.call(rbind.fill, .) %>%
  select(1:8, 10:11)

dt <- rbind(dt.1918,dt.1919,dt.1920)

dt$Bezirksname = ifelse(!is.na(dt$`Bezirks-nummer`) & dt$Kanton=="AI", "Appenzell-Innerrhoden",
                        dt$Bezirksname)
dt <- dt %>%
  mutate(Startdatum = ymd(Startdatum),
         Enddatum = ymd(Enddatum),
         iso_cw = isoweek(Enddatum),
         iso_cw_y = paste0(Jahr, "-",iso_cw),
         `Bezirks-nummer` = as.character(`Bezirks-nummer`),
         `Bezirks-nummer` = ifelse(Kanton=="GE","2500", `Bezirks-nummer`),
         # `Bezirks-nummer` = ifelse( `Bezirks-nummer `=="1601","1600", `Bezirks-nummer`),
         Gemeindename = ifelse( is.na(Gemeindename), "Bezirk",Gemeindename),
         Gemeindename2 = Gemeindename,
         Gemeindename3 = ifelse(is.na(Bezirksname), "Kanton",Gemeindename2)) %>%
  select(- Gemeindename,- Gemeindename2) %>%
  dplyr::rename(Gemeindename=Gemeindename3)


dt$`Bezirks-nummer` = recode(dt$`Bezirks-nummer`, 
                    "1601"="1600",
                    "1602" ="1600") 


dt$Gemeindename = ifelse(dt$Gemeindename=="Kanton" & dt$Kanton=="AI", "Bezirk",
                        dt$Gemeindename)

  
dt2 <- dt %>%
  filter(!Gemeindename %in% c("Kanton", "Bezirk")
  group_by(Bezirksname,iso_cw_y) %>%
  mutate(cases_b = sum())
  