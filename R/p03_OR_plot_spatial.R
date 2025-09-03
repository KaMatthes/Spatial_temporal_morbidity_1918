
lwd_size <- 1.5
lwd_size_vline <- 0.8
text_size <- 25
text_size_heat_map <- 15
legend_size <- 25
axis_legend_size <- 30
axis_legend_size_heat_map <- 15
title_size <- 20

size_axis<- 25
size_axis_title <- 15
pd <-position_dodge(width=0.5)
fatten_size <- 12

dt <- read.xlsx("output/Regression_lag_error.xlsx") %>%
  rename(Cofactor = Term,
         est= Estimate,
         Cl = CIl,
         Cu =CIu) %>%
  mutate(
    Cofactor=recode(Cofactor, 
                  "GDP_n" = "GDP",
                  "densPop_n" = "Population density",
                  "propkids_n" = "Share of 5-14 year-olds",
                  "prop60_n" = "Share of >=60 year-olds",
                  "prop20_n" = "Share of 20-40 year-olds",
                  "propmale_n" = "Share of men",
                  "In_prop_n" = "Share industry",
                  "tb_d_pop_n" = "Tuberculosis mortality",
                  "city1" = "Urbanicity (Ref: Rural)",
                  "Haushalte.pro.Haus_n" = "Households per house",
                  "person.pro.Haushalt_n" = "Household size",
                  "Hospitals" = "Number of hospitals",
                  "hosp_pop_n" = "Hospitals per 10'000",
                  "docs_pop_n" = "Doctors per 10'000",
                  "docs_p_pop_n" = "Private doctors per 10'000",
                  "docs_s_pop_n" = "Hospital doctors per 10'000")
) %>%
  filter(!Cofactor %in% c("hospitals","Hospitals per 10'000")) %>%
  mutate(wave = as.factor(wave),
         wave = recode(wave, 
                       "1" = "Jul - Aug 1918 (first)",
                       "2" = "Sep 1918 - May 1919 (second)",
                       "3" = "Jan 1920 - Apr 1920 (later)"),
         wave=factor(wave, levels = c("Jul - Aug 1918 (first)","Sep 1918 - May 1919 (second)","Jan 1920 - Apr 1920 (later)")),
         Cofactor=factor(Cofactor, levels = c("Tuberculosis mortality","Share of >=60 year-olds","Share of 20-40 year-olds","Share of 5-14 year-olds",
                                              "Share of men","Households per house","Household size","GDP","Share industry","Doctors per 10'000","Private doctors per 10'000","Hospital doctors per 10'000","Population density","Urbanicity (Ref: Rural)")),
         col_plot = recode(Cofactor,
  
           "Tuberculosis mortality" = "2",
           "Share of >=60 year-olds"= "1",
           "Share of 20-40 year-olds"= "2",
           "Share of 5-14 year-olds"= "1",
           "Share of men"= "2",
           "Households per house"= "1",
           "Household size"= "2",
           "GDP" = "1",
           "Share industry"= "2",
           "Doctors per 10'000"= "1",
           "Population density" = "2",
           "Urbanicity (Ref: Rural)" = "1")
  )

  

ggplot(dt, aes(x=Cofactor,ymin=Cl, ymax=Cu,y=est),show.legend = FALSE) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwd_size ) + 
  geom_pointrange(lwd=3,position=pd, fatten=fatten_size,lwd=lwd_size,show.legend = FALSE) +
  facet_wrap(~wave) +
  xlab("") +
  ylab("Regression coefficients and 95% CI") +

scale_color_manual("",
                   values= c("grey30","grey30"),
                   )+
  # scale_y_continuous(trans = "log10") + 
  # guides(linetype = guide_legend(override.aes = list(size = 50)))+
  # ggtitle("2020")+
  # scale_color_manual(" ",
  #                    breaks=c("age_group>=40","age_group<40"),
  #                    labels=c("Age >=40 (Ref)","Age <40"),
  #                    values = c(mypalette7[3],mypalette7[2]))+
  # theme_bw(base_family = "Georgia")+
  theme_bw()+
    theme(
      text = element_text(family = "serif", colour ="black"),
      axis.text.y = element_text(size=text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = text_size),
          legend.position = "bottom",
          legend.text=element_text(size=legend_size),
          axis.text = element_text(size=size_axis,hjust=1),
          axis.title = element_text(size=axis_legend_size),
          title =element_text(size=title_size))+
    coord_flip()
  # coord_flip(ylim=c(-30, 30))



ggsave("figures/Figure_incidence_regression.png",h=15,w=18)
