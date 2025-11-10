rm(list=ls())
source("R/00_setup.R")

# plot parameter

lwd_size <- 3
legend_size <- 30
axis_legend_size <- 30
title_size <- 20

size_axis <- 30
pd <-position_dodge(width=0.5)
fatten_size <- 8

# load data

dta <- read.xlsx("data/cases_district_wave.xlsx") %>%
  mutate(Bezirk = as.numeric(Bezirk))

dtd <- read.csv("data/Determinants.csv", sep=";") %>%
  rename(Bezirk = district_nr) %>%
  select(-district_name)


ds <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  select(Bezirk, geometry) %>%
  full_join(dta) %>%
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
  )  %>%
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
                              "household_size" = "Household size"),
    wave_name = wave,
    wave_name = recode(wave_name,
                       "1" = "Jul 1918 - Aug 1918",
                       "2" = "Sep 1918 - May 1919",
                       "3" = "Jan 1920 - May 1920")
  )



w="Jan 1920 - May 1920"
v="Population density"
n_boot=2000
conf=0.95

function_gwr_data <- function(w, v,n_boot, conf) {
  
  dt <- ds %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  filter(wave_name==w,
         Cofac_name== v) %>%
  as(. , "Spatial")

# estimate otimal bandwith
bw <- bw.gwr(formula =incidence ~ value,
             approach = "AICc",
             kernel="gaussian",
             adaptive= T,
             data = dt) 

# gwr regression
gwr.mod <- gwr.robust(formula = incidence ~ value,
                     adaptive = T,
                     data = dt,
                     bw = bw)

# get results for OLS
res.lm <- summary(gwr.mod$lm)$coefficients %>%
  data.frame() %>%
  mutate(
    Cl = round(Estimate - 1.96 * Std..Error,2),
    Cu =  round(Estimate + 1.96 * Std..Error,2), 
    Coef = round(Estimate, 2),
    Cofactor = v, 
    wave = w,
    global= paste0(Coef," (",Cl, " to ", Cu,")"))%>%
  select(Cofactor, wave,global) %>%
  slice(2)

# get results and CI from GWR with bootstrapping
vals <- gwr.mod$SDF@data %>%
  select(2)  %>%
  as.matrix()

median.function <- function(x, index) {
  d <- x[index]    
  return( median(d))  
}
boot <- boot(data = vals, statistic =  median.function, R=2000)
boot_ci <- boot.ci( boot, level=.95, type='bca')$bca

# combine results
res <- tibble( 
  Cofactor = v,
  Coef = round(boot$t0, 2),
  Clb = round(boot_ci[4], 2),
  Cub = round(boot_ci[5], 2),
  wave = w,
  gwr = paste0(round(Coef, 2), " (", round(Clb, 2), " to ", round( Cub, 2), ")")
  )%>%
  left_join(res.lm)

print(res)

}


# run function for all waves and variables
dat.a <-list()

h <- 0L 
for( w in unique(ds$wave_name)) {
  print(w)
  for( v in unique(ds$Cofac_name)) {
    print(v)
    h  <- h + 1L
    dat.a[[h]] <-  function_gwr_data(w, v,2000, 0.95)

  }
}


dtt<- do.call(rbind,dat.a) %>%
  mutate(Cofacter = factor(Cofactor, 
                           levels = c("Population density",
                                      "GDP per capita",
                                      "Share of industry",
                                      "Private physicians per km2",
                                      "Household size",
                                      "Households per house",
                                      "Share of men",
                                      "Share of 5-14 years old",
                                      "Share of 20-39 years old" ,
                                      "Share of >= 60 years old")),
         Cofacter = factor(Cofactor, 
                            levels = c("Jul 1918 - Aug 1918",
                                       "Sep 1918 - May 1919",
                                       "Jan 1920 - May 1920"))
         )
         
write.xlsx(dtt, "output/gwr_results_inc.xlsx")


####################
# Coefficient plot #
###################

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
                                      "Population density")),
                ymin=Clb, 
                ymax=Cub,
                y= Coef, 
                col=factor(wave, 
                           levels = c( "Jan 1920 - May 1920",
                                       "Sep 1918 - May 1919",
                                       "Jul 1918 - Aug 1918"))),
                position=pd) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwd_size-1.8 ) + 
  geom_pointrange(position=pd, fatten=fatten_size,lwd=lwd_size) +
  xlab("") +
  ylab("Median regression coefficients of incidence and 95% CI") +
  
  scale_color_manual("",
                     guide = guide_legend(reverse = TRUE),
                     values= col_c)+
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
    axis.title = element_text(size=axis_legend_size),
    title =element_text(size=title_size))+
  coord_flip()

ggsave("figures/Figure4.pdf",h=15,w=18)
# ggsave("figures/Figure4.png",h=15,w=18)

#################
# Maps for GWR  #
#################


function_gwr_plot <- function(w, v) {
  
  
  
  dt <- ds %>%
    mutate(
      Bezirk = as.numeric(Bezirk),
    ) %>%
    filter(wave_name==w,
           Cofac_name== v) %>%
    as(. , "Spatial")
  
  bw <- bw.gwr(formula =incidence~ value,
               approach = "AICc",
               kernel="gaussian",
               adaptive= T,
               data = dt) 
  
  gwr.mod <- gwr.robust(formula =incidence~ value,
                        adaptive = T,
                        data = dt,
                        bw = bw)
  
  
  map_data <- gwr.mod$SDF %>%
    st_as_sf(.) 
    ggplot(map_data) +
    geom_sf(aes(fill = value), color = "black", size = 0.2) +
    scale_fill_gradientn(
      name = "",  # legend title
      colours = c(muted("blue"), "white", muted("red")),
      values = rescale(c(min(map_data$value, na.rm = TRUE),
                         0,
                         max(map_data$value, na.rm = TRUE))),
      labels = c(round(min(map_data$value, na.rm = TRUE), 0),0,round(max(map_data$value, na.rm = TRUE), 0)),
      breaks = c(min(map_data$value, na.rm = TRUE),0,max(map_data$value, na.rm = TRUE)))+
    
      # limits = c(min(map_data$value, na.rm = TRUE), max(map_data$value, na.rm = TRUE))
    labs(title = v) +
    theme_bw()+
    theme(
      text = element_text(family = "serif", colour ="black"),
      plot.title = element_text(size = 30,  hjust = 0, vjust = 1),
      legend.position = c(0.1, 0.8),
      legend.title = element_text(color = "black"),
      legend.text=element_text(size=20),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )

}

# first wave

plot11 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v="Population density")
plot12 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v= "GDP per capita")
plot13 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v= "Share of industry")
plot14 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v="Private physicians per km2")
plot15 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v= "Households per house")
plot16 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v= "Household size")
plot17 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v=  "Share of men")
plot18 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v=  "Share of 5-14 years old")
plot19 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v=  "Share of 20-39 years old" )
plot20 <- function_gwr_plot(w="Jul 1918 - Aug 1918", v=  "Share of >= 60 years old" )

map1 <- ggarrange(plot11, plot12, 
          plot13, plot14,
          plot15, plot16,
          plot17, plot18,
          plot19, plot20,
          ncol = 2, nrow=5,
          align = 'h')


text1 <- "July 1918 - August 1918"

# Create a text grob
tgrob <- text_grob(text1,size = 40, family = "serif")
# Draw the text
plot_0 <- as_ggplot(tgrob) + 
  theme(plot.margin = margin(0,0,-5,0, "cm"))

ggarrange(plot_0 ,map1,
          ncol = 1,nrow = 2,heights = c(1,8))
# ggsave("figures/Figure_gwr_firstwave.png",h=35,w=20)

ggsave("figures/Figure_S7.pdf",h=35,w=20)
# second wave

plot21 <- function_gwr_plot(w= "Sep 1918 - May 1919", v="Population density")
plot22 <- function_gwr_plot(w= "Sep 1918 - May 1919", v= "GDP per capita")
plot23 <- function_gwr_plot(w= "Sep 1918 - May 1919", v= "Share of industry")
plot24 <- function_gwr_plot(w= "Sep 1918 - May 1919", v="Private physicians per km2")
plot25 <- function_gwr_plot(w="Sep 1918 - May 1919", v= "Households per house")
plot26 <- function_gwr_plot(w="Sep 1918 - May 1919", v= "Household size")
plot27 <- function_gwr_plot(w="Sep 1918 - May 1919", v=  "Share of men")
plot28 <- function_gwr_plot(w="Sep 1918 - May 1919", v=  "Share of 5-14 years old")
plot29 <- function_gwr_plot(w="Sep 1918 - May 1919", v=  "Share of 20-39 years old" )
plot30 <- function_gwr_plot(w="Sep 1918 - May 1919", v=  "Share of >= 60 years old" )

map2 <- ggarrange(plot21, plot22, 
                  plot23, plot24,
                  plot25, plot26,
                  plot27, plot28,
                  plot29, plot30,
                  ncol = 2, nrow=5,
                  align = 'h')


text2 <- "September 1918 - May 1919"

# Create a text grob
tgrob <- text_grob(text2,size = 40, family = "serif")
# Draw the text
plot_0 <- as_ggplot(tgrob) + 
  theme(plot.margin = margin(0,0,-5,0, "cm"))

ggarrange(plot_0 ,map2,
          ncol = 1,nrow = 2,heights = c(1,8))

# ggsave("figures/Figure_gwr_secondwave.png",h=35,w=20)

ggsave("figures/Figure_S8.pdf",h=35,w=20)

# third wave

plot31 <- function_gwr_plot(w= "Jan 1920 - May 1920", v="Population density")
plot32 <- function_gwr_plot(w= "Jan 1920 - May 1920", v= "GDP per capita")
plot33 <- function_gwr_plot(w= "Jan 1920 - May 1920", v= "Share of industry")
plot34 <- function_gwr_plot(w= "Jan 1920 - May 1920", v="Private physicians per km2")
plot35 <- function_gwr_plot(w="Jan 1920 - May 1920", v= "Households per house")
plot36 <- function_gwr_plot(w="Jan 1920 - May 1920", v= "Household size")
plot37 <- function_gwr_plot(w="Jan 1920 - May 1920", v=  "Share of men")
plot38 <- function_gwr_plot(w="Jan 1920 - May 1920", v=  "Share of 5-14 years old")
plot39 <- function_gwr_plot(w="Jan 1920 - May 1920", v=  "Share of 20-39 years old" )
plot40 <- function_gwr_plot(w="Jan 1920 - May 1920", v=  "Share of >= 60 years old" )

map3 <- ggarrange(plot31, plot32, 
                  plot33, plot34,
                  plot35, plot36,
                  plot37, plot38,
                  plot39, plot40,
                  ncol = 2, nrow=5,
                  align = 'h')


text3 <- "January 1920 - May 1920"

# Create a text grob
tgrob <- text_grob(text3,size = 40, family = "serif")
# Draw the text
plot_0 <- as_ggplot(tgrob) + 
  theme(plot.margin = margin(0,0,-5,0, "cm"))

ggarrange(plot_0 ,map3,
          ncol = 1,nrow = 2,heights = c(1,8))

# ggsave("figures/Figure_gwr_thirdwave.png",h=35,w=20)

ggsave("figures/Figure_S9.pdf",h=35,w=20)

# write data

function_gwr_write <- function(w, v) {
  
  dt <- ds %>%
    mutate(
      Bezirk = as.numeric(Bezirk),
    ) %>%
    filter(wave_name==w,
           Cofac_name== v) %>%
    as(. , "Spatial")
  
  bw <- bw.gwr(formula =incidence~ value,
               approach = "AICc",
               kernel="gaussian",
               adaptive= T,
               data = dt) 
  
  gwr.mod <- gwr.robust(formula =incidence~ value,
                        adaptive = T,
                        data = dt,
                        bw = bw)

  dp <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") 
  # get results and CI from GWR with bootstrapping
 res <- gwr.mod$SDF %>%
   st_as_sf(.) %>%
   st_join(dp) %>%
   as.data.frame() %>%
   mutate(    Cofactor = v, 
              wave = w) %>%
   select(Cofactor, wave, Bezirk=BEZNR, value)
}


dat.a <-list()

h <- 0L 
for( w in unique(ds$wave_name)) {
  print(w)
  for( v in unique(ds$Cofac_name)) {
    print(v)
    h  <- h + 1L
    dat.a[[h]] <- function_gwr_write(w, v)
    
  }
}


dtt<- do.call(rbind,dat.a)

write.xlsx(dtt, "output/gwr_results.xlsx")
