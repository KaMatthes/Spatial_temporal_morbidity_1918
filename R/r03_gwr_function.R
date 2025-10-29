lwd_size <- 3
lwd_size_vline <- 0.8
text_size <- 25
text_size_heat_map <- 15
legend_size <- 30
axis_legend_size <- 30
axis_legend_size_heat_map <- 15
title_size <- 20

size_axis <- 30
size_axis_title <- 15
pd <-position_dodge(width=0.5)
fatten_size <- 8

dta <- read.xlsx("data/Faelle_Bezirke_total_pop_wave.xlsx") %>%
  mutate(Bezirk = as.numeric(Bezirk))

gpd <- read.xlsx("data/GDP.xlsx") 

denspop <- read.xlsx("data/DensPop.xlsx") %>%
  select(-pop)

dlw <- read.xlsx("data/Landwirtschaft.xlsx") %>%
  rename(LW_prop = Anteil) %>%
  select(-Erwerbende, -LW) 

hou <- read.xlsx("data/Household.xlsx") %>%
  select(-pop, -Hauser,-Haushalte)


hosp <-  read.xlsx("data/Hospitals.xlsx")

tb <-  read.xlsx("data/TB.xlsx")%>%
  select(-pop, -Tb.Deaths.1911_1920) %>%
  rename(TB_death = pro_jahr)

ur <-  read.xlsx("data/Urbanity.xlsx") %>%
  mutate(
    city=as.factor(city)
  )

age20 <-  read.xlsx("data/prop_age20.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(prop20 = prop) %>%
  select(-year)

age60 <-  read.xlsx("data/prop_age60.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(prop60 =prop) %>%
  select(-year)

agekids <-  read.xlsx("data/prop_schoolkids.xlsx") %>%
  filter(year %in% 1918) %>%
  rename(propkids =prop) %>%
  select(-year)

propmale <-  read.xlsx("data/prop_sex.xlsx") %>%
  rename(propmale =prop_male) 

docs <-  read.xlsx("data/Aerzte1910.xlsx") %>%
  full_join(denspop) %>%
  mutate(dens_doc = Doc_privat/area) %>%
  select(Bezirk, MapName, Doc_privat,Doc_spital,docs = Both, dens_doc) 

ds <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") %>%
  rename(Bezirk = BEZNR,
         Bezirksname = BEZNA) %>%
  select(Bezirk, geometry) %>%
  full_join(dta) %>%
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
  filter(!is.na(year)) %>%
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
    # person.pro.Haushalt.alt_s = scale(person.pro.Haushalt.alt),
    incidence_s = scale(incidence)
    # incidence_n= normalit(incidence)
  )  %>%
  select(-docs_p_pop, -tb_d_pop,-GDP, -area, -densPop, -LW_prop, -Haushalte.pro.Haus,
         -person.pro.Haushalt,-hospitals, -TB_death,-TB_inc,-prop60, -prop20, -propkids,-propmale,
         -Doc_spital, -docs, -dens_doc,-In_prop,-tb_d_pop,-Doc_privat, -docs_p_pop_s) %>%
  gather(.,"Cofactor","value",city:person.pro.Haushalt_s) %>%
  mutate(
    value = as.numeric(value),
    Cofac_name = Cofactor,
    Cofac_name = recode( Cofac_name, 
                              "city" = "Urbanicity",
                              "docs_p_pop_s" ="Private physicians per 10’000",
                              "dens_doc_s" = "Private physicians per km2",
                              "propmale_s"  = "Share of men",
                              "propkids_s"  = "Share of 5-14 years old",
                              "prop20_s"    =  "Share of 20-39 years old",
                              "prop60_s"  = "Share of >= 60 years old",
                              "In_prop_s"     = "Share of industry",
                              "GDP_s" = "GDP per capita",
                              "densPop_s"    = "Population density",
                              "tb_d_pop_s" = "Tuberculosis mortality per 10’000",
                              "Haushalte.pro.Haus_s"  = "Households per house",
                              "person.pro.Haushalt_s" = "Household size"),
                         # "person.pro.Haushalt.alt_s" = "Household size alt"),
    wave_name = wave,
    wave_name = recode(wave_name,
                       "1" = "Jul 1918 - Aug 1918",
                       "2" = "Sep 1918 - Mai 1919",
                       "3" = "Jan 1920 - Mai 1920")
  ) %>%
  filter(!Cofac_name %in% c("Urbanicity","Tuberculosis mortality per 10’000","Private physicians per 10’000"))

dp <- read_sf("data/Polygonbasis_183/Polygonbasis_183_eli.shp") 
# %>%
#   filter(!BEZNR %in% 1600)

gnb <- poly2nb(dp)
glw  <- nb2listw(gnb)

w="Sep 1918 - Mai 1919"
v="Population density"
n_boot=2000
conf=0.95


function_gwr_data <- function(w, v,n_boot, conf) {

dt <- ds %>%
  # filter(!Cofac_name %in% "Urbanicity") %>%
  mutate(
    Bezirk = as.numeric(Bezirk),
  ) %>%
  # filter(!Kanton %in% c("AI")) %>%
  filter(wave_name==w,
         Cofac_name== v) %>%
  as(. , "Spatial")

bw <- bw.gwr(formula =incidence~ value,
             approach = "AICc",
             kernel="gaussian",
             adaptive= T,
             data = dt) 

gwr.mod <- gwr.robust(formula = incidence ~ value,
                     adaptive = T,
                     data = dt,
                     bw = bw)

res.lm <- summary(gwr.mod$lm)$coefficients %>%
  data.frame() %>%
  mutate(Cl = round(Estimate - 1.96 * Std..Error,2),
         Cu =  round(Estimate + 1.96 * Std..Error,2),
         Coef = round(Estimate, 2),
         Cofactor = v, 
         wave = w,
         global= paste0(Coef," (",Cl, " to ", Cu,")"))%>%
  select(Cofactor, wave,global) %>%
  slice(2)

# mora <- moran.test(gwr.mod$SDF$residual, listw=glw)

vals <- gwr.mod$SDF@data %>%
  select(2)  %>%
  as.matrix()


bootstrap_gwr_coef <- function(coef_vector, n_boot = n_boot, conf = conf, seed = 17102025) {
  if (!is.null(seed)) set.seed(seed)
  
  n <- length(coef_vector)
  boot_means <- replicate(n_boot, {
    sample_vals <- sample(coef_vector, size = n, replace = TRUE)
    median(sample_vals)
  })
  
  alpha <- (1 - conf) / 2
  ci <- quantile(boot_means, probs = c(alpha, 1 - alpha))
  
  list(
    coef_median = median(coef_vector),
    boot_median = median(boot_means),
    ci_lower = ci[1],
    ci_upper = ci[2]
  )
}

median.function <- function(x, index) {
  d <- x[index]     # This first line will go in ever bootstrap function you make.
  return( median(d))  
}
boot2 <- boot(data = vals, statistic =  median.function, R=2000)

boot_ci <- boot.ci( boot2, level=.95, type='bca')$bca
boot_ci2 <- boot.ci( boot2, level=.95, type='perc')$perc
boot_ci3 <- boot.ci( boot2, level=.95, type='norm')$norm

res <- bootstrap_gwr_coef(vals,n_boot =n_boot, conf = conf, seed = 17102025) %>%
  as.data.frame(.) %>%
  mutate(Cofactor =v, 
         Coef = round(boot_median,2),
         Cl = round(ci_lower,2),
         Cu = round(ci_upper,2),
         Clb = round(boot_ci[4],2),
         Cub = round(boot_ci[5],2),
         Clnorm = round(boot_ci3[2],2),
         Cunorm = round(boot_ci3[3],2),
         wave = w,
         gwr = paste0(Coef," (",Cl, " to ", Cu,")")) %>%
  select(Cofactor, wave,gwr, Coef, Cl,Clb, Clnorm, Cu, Cub, Cunorm) %>%
  left_join(res.lm)

print(res)

}

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
                                      # "Household size alt",
                                      "Households per house",
                                      "Share of men",
                                      "Share of 5-14 years old",
                                      "Share of 20-39 years old" ,
                                      "Share of >= 60 years old")),
         Cofacter = factor(Cofactor, 
                            levels = c("Jul 1918 - Aug 1918",
                                       "Sep 1918 - Mai 1919",
                                       "Jan 1920 - Mai 1920"))
         )
         
write.xlsx(dtt, "output/gwr_results_inc.xlsx")


ggplot(dtt, aes(x=factor(Cofactor, 
                         levels = c(  "Share of >= 60 years old",
                                      "Share of 20-39 years old" ,
                                      "Share of 5-14 years old",
                                      "Share of men",
                                      "Household size",
                                      # "Household size alt",
                                      "Households per house",
                                      "Private physicians per km2",
                                      "Share of industry",
                                      "GDP per capita",
                                      "Population density")),
                ymin=Clb, 
                ymax=Cub,
                y= Coef, 
                col=factor(wave, 
                           levels = c( "Jan 1920 - Mai 1920",
                                       "Sep 1918 - Mai 1919",
                                       "Jul 1918 - Aug 1918"))),
                position=pd) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwd_size-1.8 ) + 
  geom_pointrange(position=pd, fatten=fatten_size,lwd=lwd_size) +
  xlab("") +
  ylab("Median regression coefficients of incidence and 95% CI") +
  
  scale_color_manual("",
                     # values= c("grey30","grey60"),
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

ggsave("figures/Figure4.png",h=15,w=18)

# ggplot(dtt, aes(x=Cofactor,ymin=Cl, ymax=Cu,y=Coef),show.legend = FALSE) + 
#   geom_hline(yintercept=0, colour="grey", lwd=lwd_size ) + 
#   geom_pointrange(lwd=3,position=pd, fatten=fatten_size,lwd=lwd_size,show.legend = FALSE) +
#   facet_wrap(~wave) +
#   xlab("") +
#   ylab("Regression coefficients of z-values and 95% CI") +
#   
#   scale_color_manual("",
#                      values= c("grey30","grey30"),
#   )+
#   # scale_y_continuous(trans = "log10") + 
#   # guides(linetype = guide_legend(override.aes = list(size = 50)))+
#   # ggtitle("2020")+
#   # scale_color_manual(" ",
#   #                    breaks=c("age_group>=40","age_group<40"),
#   #                    labels=c("Age >=40 (Ref)","Age <40"),
#   #                    values = c(mypalette7[3],mypalette7[2]))+
#   # theme_bw(base_family = "Georgia")+
#   theme_bw()+
#   theme(
#     text = element_text(family = "serif", colour ="black"),
#     axis.text.y = element_text(size=text_size),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     strip.text = element_text(size = text_size),
#     legend.position = "bottom",
#     legend.text=element_text(size=legend_size),
#     axis.text = element_text(size=size_axis,hjust=1),
#     axis.title = element_text(size=axis_legend_size),
#     title =element_text(size=title_size))+
#   coord_flip()
# # coord_flip(ylim=c(-30, 30))


# get intervals with bootstrapping

function_gwr_plot <- function(w, v) {
  
  dt <- ds %>%
    # filter(!Cofac_name %in% "Urbanicity") %>%
    mutate(
      Bezirk = as.numeric(Bezirk),
    ) %>%
    # filter(!Kanton %in% c("AI")) %>%
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
ggsave("figures/Figure_gwr_firstwave.png",h=35,w=20)

# second wave

plot21 <- function_gwr_plot(w= "Sep 1918 - Mai 1919", v="Population density")
plot22 <- function_gwr_plot(w= "Sep 1918 - Mai 1919", v= "GDP per capita")
plot23 <- function_gwr_plot(w= "Sep 1918 - Mai 1919", v= "Share of industry")
plot24 <- function_gwr_plot(w= "Sep 1918 - Mai 1919", v="Private physicians per km2")
plot25 <- function_gwr_plot(w="Sep 1918 - Mai 1919", v= "Households per house")
plot26 <- function_gwr_plot(w="Sep 1918 - Mai 1919", v= "Household size")
plot27 <- function_gwr_plot(w="Sep 1918 - Mai 1919", v=  "Share of men")
plot28 <- function_gwr_plot(w="Sep 1918 - Mai 1919", v=  "Share of 5-14 years old")
plot29 <- function_gwr_plot(w="Sep 1918 - Mai 1919", v=  "Share of 20-39 years old" )
plot30 <- function_gwr_plot(w="Sep 1918 - Mai 1919", v=  "Share of >= 60 years old" )

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

ggsave("figures/Figure_gwr_secondwave.png",h=35,w=20)



# third wave

plot31 <- function_gwr_plot(w= "Jan 1920 - Mai 1920", v="Population density")
plot32 <- function_gwr_plot(w= "Jan 1920 - Mai 1920", v= "GDP per capita")
plot33 <- function_gwr_plot(w= "Jan 1920 - Mai 1920", v= "Share of industry")
plot34 <- function_gwr_plot(w= "Jan 1920 - Mai 1920", v="Private physicians per km2")
plot35 <- function_gwr_plot(w="Jan 1920 - Mai 1920", v= "Households per house")
plot36 <- function_gwr_plot(w="Jan 1920 - Mai 1920", v= "Household size")
plot37 <- function_gwr_plot(w="Jan 1920 - Mai 1920", v=  "Share of men")
plot38 <- function_gwr_plot(w="Jan 1920 - Mai 1920", v=  "Share of 5-14 years old")
plot39 <- function_gwr_plot(w="Jan 1920 - Mai 1920", v=  "Share of 20-39 years old" )
plot40 <- function_gwr_plot(w="Jan 1920 - Mai 1920", v=  "Share of >= 60 years old" )

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

ggsave("figures/Figure_gwr_thirdwave.png",h=35,w=20)





