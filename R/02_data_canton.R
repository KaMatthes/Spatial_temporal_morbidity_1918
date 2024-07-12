rm(list=ls())

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

dt.1918 <- read_excel_allsheets("data/Kantonsebene_1918.xlsx") %>%
  do.call(bind_rows, .) 

dt.1919 <- read_excel_allsheets("data/Kantonsebene_1919.xlsx") %>%
  do.call(bind_rows, .) 

dt.1920 <- read_excel_allsheets("data/Kantonsebene_1920.xlsx") %>%
  do.call(bind_rows, .) 

dt <- rbind(dt.1918,dt.1919,dt.1920) %>%
  ungroup() %>%
  select(-Bemerkungen) %>%
  mutate(Startdatum = ymd(as.Date(Startdatum,"%d.%m.%Y")),
         Enddatum = ymd(as.Date(Enddatum,"%d.%m.%Y")),
         iso_cw = isoweek(Enddatum),
         iso_cw_y = paste0(Jahr, "-",iso_cw)) %>%
  # arrange(Startdatum)
  select(Kanton, Kantonsnummer, Kantonsname,iso_cw_y,Cases=Fallzahl)

write.xlsx(dt ,"data/Faelle_Kanton.xlsx", row.names=FALSE, overwrite = TRUE)

dt2 <- read.xlsx("data/Faelle_Bezirke.xlsx") %>%
  group_by( Kanton,iso_cw_y) %>%
  mutate(Cases_Kanton = sum(Cases)) %>%
  ungroup() %>%
  distinct(Kanton, iso_cw_y, .keep_all = TRUE) %>%
  select(Kanton, iso_cw_y, Cases_Bezirk = Cases_Kanton) %>%
  left_join(dt) %>%
  select(Kanton, Kantonsname, iso_cw_y, Cases_Kanton=Cases, Cases_Bezirk)


write.xlsx(dt2 ,"data/Faelle_Kanton_Vergleich.xlsx", row.names=FALSE, overwrite = TRUE)
