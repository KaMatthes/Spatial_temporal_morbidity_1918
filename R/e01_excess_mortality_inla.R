function_inla_total <- function(Year_Pan,Year_max, Year_min) {
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


nc.sids <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) 

row.names(nc.sids) <- nc.sids$Bezirk

nc.nb <- poly2nb(nc.sids, nc.sids$Bezirk) 
nb2INLA("Bezirk_Inla", nc.nb)

region.names <- poly2nb(nc.sids, nc.sids$Bezirk) %>%
  attr("region.id") %>%
  as.data.frame() %>%
  rename(Bezirk = ".") %>%
  mutate(Region = 1:183,
         Bezirk=as.character(Bezirk))

dat.excess <- dt %>%
  mutate(Bezirk=as.character(Bezirk)) %>%
  full_join(region.names) %>%
  rename(Year = year,
         death =dx) %>%
  filter(Year >=Year_min & Year <=Year_max ) %>%
  arrange(Region) %>%
  mutate(Bezirk= as.numeric(Bezirk),
         death = ifelse(death < 0, 0, death),
         death = ifelse(is.na(death), 0, death),
         Region.struct= Region,
         Region.beta = Region) %>%
  select(Year,death, pop,Region, Region.struct)

year_smooth <- 4
year_from <- min(dat.excess$Year)
year_reg <- year_from + year_smooth


control.family <- inla.set.control.family.default()

  formula <- death ~ 1 + offset(log(pop)) +
    f(Year, model='iid', constr = TRUE) +
    f(Region, model="bym", graph="Bezirk_Inla", scale.model = TRUE)

  expected_deaths <- list()
  
  for (YEAR in year_reg:Year_max){
    
    print(YEAR)
    
    if (YEAR==Year_Pan) {
    reg_data <-  dat.excess %>% 
      filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
      mutate(death=ifelse (Year ==YEAR, NA, death))
    }
    
    else {
      reg_data <-  dat.excess %>% 
        filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
        mutate(death=ifelse (Year ==YEAR, NA, death)) %>%
        filter(!Year == Year_Pan)
    }
    
    set.seed(20220421)
   
    inla.mod <- inla(formula,
                     data=reg_data,
                     # family="nbinomial",
                     family = "zeroinflatednbinomial0",
                     # family = "zeroinflatednbinomial1",
                     #verbose = TRUE,
                     control.family = control.family,
                     control.compute = list(config = TRUE),
                     control.mode = list(restart = TRUE),
                      # num.threads = round(parallel::detectCores() * .2),
                     control.predictor = list(compute = TRUE, link = 1))
  
  post.samples <- inla.posterior.sample(n = 1000, result = inla.mod, seed=20220421)
  predlist <- do.call(cbind, lapply(post.samples, function(X)
    exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  
  predlist2 <- do.call(cbind, lapply(post.samples, function(X)
      X$hyperpar))
    
  
  rate.drawsMed<-array(unlist( predlist), dim=c(dim(reg_data)[1], 1000)); dim(rate.drawsMed) 
  dM = as.data.frame(rate.drawsMed)
  # Add to the data and save
  Data= cbind(reg_data,dM)
  
  mean.samples <- Data %>%
    select(starts_with("V"), "Region", "Year") %>%
    rowwise(Region) %>%
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select(Region, fit, LL, UL, Year) %>%
    filter(Year==YEAR) %>%
    arrange(Region,Year) %>%
    left_join(region.names) %>%
    mutate(Bezirk= as.numeric(Bezirk)) %>%
    rename(year = Year) %>%
    left_join(dt)
    
  
  
  expected_deaths[[YEAR]] <-  mean.samples
  expected_deaths <- expected_deaths[-which(sapply(expected_deaths, is.null))] 
  
  }
  
  expected_deaths <- expected_deaths %>%
    bind_rows(., .id = "column_label") %>%
    filter(year ==1918) %>%
    rename(bsn = fit,
           bsn_l = LL,
           bsn_u = UL) %>%
    mutate(
           excess = dx - bsn,
           excess2 = excess/bsn)  %>%
    select(Bezirksname,Bezirk,year,dx,mx,pop,bsn,bsn_l,bsn_u,excess,excess2)
  

  
  write.xlsx(expected_deaths,paste0("data/expected_death_inla",Year_Pan,".xlsx"), row.names=FALSE, overwrite = TRUE)
  save(expected_deaths,file=paste0("data/expected_death_inla",Year_Pan,".RData"))
  }


function_inla_total(Year_Pan=1918, Year_max=1918, Year_min=1913)


