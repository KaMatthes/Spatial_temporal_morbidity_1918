
rm(list=ls())
source("R/00_setup.R")


# plot parameter
lwd_size <- 3
legend_size <- 30
size_axis <- 30
title_size <- 20
pd <-position_dodge(width=0.3)
fatten_size <- 8


# load data
dta<- read.xlsx("data/cases_district_year.xlsx") %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  filter(year==1918)

dte<- read.xlsx("data/expected_death_inla.xlsx")  %>%
  mutate(
    Bezirk = as.numeric(Bezirk)) %>%
  select(Bezirk,excess = excess2)


dtd <- read.csv("data/Determinants.csv", sep=";") %>%
  rename(Bezirk = district_nr) %>%
  select(-district_name)


dt <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  select(Bezirk, geometry) %>%
  full_join(dta) %>%
  left_join(dte) %>%
  left_join(dtd) %>%
  mutate(
    dens_doc= scale(dens_doc),
    share_male= scale(share_male),
    share_5_14= scale(share_5_14),
    share_20_39= scale(share_20_39),
    share_60= scale(share_60),
    share_industry= scale(share_industry),
    gdp = scale(gdp),
    dens_pop= scale(dens_pop),
    houshold_house =scale(houshold_house),
    household_size= scale(household_size),
    incidence = scale(incidence),
    excess = scale(excess)) %>%
  gather(.,"Cofactor","value",dens_pop:share_60) %>%
  mutate(
    value = as.numeric(value),
    Cofac_name = Cofactor,
    Cofac_name = recode( Cofac_name, 
                         "dens_doc" = "Private physicians per km2",
                         "share_male"  = "Share of men",
                         "share_5_14"  = "Share of 5-14 years old",
                         "share_20_39"    =  "Share of 20-39 years old",
                         "share_60"  = "Share of >= 60 years old",
                         "share_industry"     = "Share of industry",
                         "gdp" = "GDP per capita",
                         "dens_pop"    = "Population density",
                         "houshold_house"  = "Households per house",
                         "household_size" = "Household size"))

# robust regression analyis

function_rlm <- function(v,o) {
  
  dt1 <- dt %>%
  filter(Cofac_name== v)
  
  formula_str <- paste(o,"~value")
  formula <- as.formula(formula_str)
 
  model <- rlm(formula,data =dt1)
 
 res <- coef(summary(model)) %>%
   as.data.frame() %>%
   slice(2) %>%
   mutate( Cofactor = v,
           Output = o)
 return(res)

}

outn <- c("incidence", "excess")

dat.a <-list()
h <- 0L 
for( o in  outn) {
  print(o)
  for( v in unique(dt$Cofac_name)) {
    print(v)
    h  <- h + 1L
    dat.a[[h]] <-  function_rlm ( v,o)
    
  }
}


dtt<- do.call(rbind,dat.a) %>%
  mutate(
    est= round(Value,2),
    Cl = round(Value - 1.96* `Std. Error`,2),
    Cu = round(Value + 1.96* `Std. Error`,2),
    Univariate = paste0(est," (",Cl," to ",Cu, ")"))

                      

# plot 

ggplot(dtt, aes(x=factor(Cofactor, 
                        levels = c(  "Share of >= 60 years old",
                                     "Share of 20-39 years old" ,
                                     "Share of 5-14 years old",
                                     "Share of men",
                                     "Household size",
                                     "Households per house",
                                     "Private physicians per km2",
                                     "Share of industry",
                                     "GDP per capita",
                                     "Population density"
                        ))
               ,ymin=Cl, ymax=Cu,y=est, col=Output),position=pd) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwd_size -1.8 ) + 
  geom_pointrange(position=pd, fatten=fatten_size,lwd=lwd_size) +
  xlab("") +
  ylab("Regression coefficients of z-values and 95% CI") +
  
  scale_color_manual("",
                     # values= c("grey30","grey60"),
                     labels=c("Excess Mortality","Incidence"),
                     guide = guide_legend(reverse = TRUE),
                     values= col2)+
  theme_bw()+
  theme(
    text = element_text(family = "serif", colour ="black"),
    axis.text.y = element_text(size=size_axis),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(size = size_axis),
    legend.position = "bottom",
    legend.text=element_text(size=legend_size),
    axis.text.x = element_text(size=size_axis,hjust=1),
    axis.title = element_text(size=size_axis),
    title =element_text(size=title_size))+
  coord_flip()

ggsave("figures/Figure5.png",h=15,w=18)
ggsave("figures/Figure5.pdf",h=15,w=18)

# save data

dtr <- dtt %>%
  select(Cofactor, Output, Univariate) %>%
  spread(., Output, Univariate)

write.xlsx( dtr ,"output/Regression_1918.xlsx", row.names=FALSE, overwrite = TRUE)
