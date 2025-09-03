# data death
dt <- read.xlsx("data/Death1913_1918.xlsx") %>%
  gather(., year, death, `1913`:`1918`) %>%
  mutate(
    year = as.numeric(year)
  )


# data population - interpolate population
dtp <- read.xlsx("data/Pop1910_1920.xlsx") %>%
  gather(., year, pop, `1910`:`1920`) %>%
  mutate(
    year = as.numeric(year)
  ) %>%
  complete(year = seq(1910,1920, by=1)) %>%
  complete(year, nesting(Bezirk)) %>%
  group_by(Bezirk) %>%
  arrange(Bezirk, year) %>%
  mutate(pop = round(zoo::na.approx(pop, na.rm=FALSE),0)) %>%
  filter(!is.na(pop)) %>%
  select(-Bezirksname)

dt <- dt %>%
  left_join(dtp) %>%
  rename(dx=death) %>%
  mutate(
    mx = dx/pop
  )


funtion_excess <- function(b){
  
  dt1 <- dt %>%
    filter(Bezirk %in% b) %>%
    mutate( 
      t= 1:n(),
      w= ifelse(year==1918, 0, 1))
  
# Model_year <- glm(death ~  year ,family=quasipoisson,offset=log(pop), data=  dt1)

mod <- gam(dx ~ s(t, k = 6, bs= "ps") + offset(log(pop)),select=TRUE,
           data = dt1,
           weights = w,
           family = quasipoisson)


res <- predict(mod, type = "response", se.fit = TRUE)

dt2 <- 
  dt1 %>% 
  mutate(bsn = res$fit,
         bsn_l = bsn - 1.96*res$se.fit,
         bsn_u = bsn + 1.96*res$se.fit,
         excess = dx - bsn,
         excess2 = excess/bsn) %>%
  filter(year==1918)

}

dat.a <-list()

h <- 0L 
for( b in unique(dt$Bezirk)) {
  print(b)
    h  <- h + 1L
    dat.a[[h]] <-  funtion_excess(b)
  } 



dt3 <- do.call(rbind,dat.a)

write.xlsx(dt3 ,"data/excess_mortality.xlsx", row.names=FALSE, overwrite = TRUE)

  
