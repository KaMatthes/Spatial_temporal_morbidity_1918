
lwd_size <- 1.5
lwd_size_vline <- 0.8
text_size <- 25
text_size_heat_map <- 15
legend_size <- 25
axis_legend_size <- 25
axis_legend_size_heat_map <- 15
title_size <- 20

size_axis_x <- 20
size_axis <-15
size_axis_title <- 15
pd <-position_dodge(width=0.5)
fatten_size <- 12


dt1 <- read.xlsx("output/Regression_rlm_zscore_1918.xlsx") %>%
  mutate(fac="Incidence")

dt2 <- read.xlsx("output/Regression_rlm_excess_scale.xlsx") %>%
  mutate(fac="Excess Mortality")

dt <- rbind(dt1, dt2) %>%
  filter(!Cofactor %in% c("hospitals","Hospitals per 10'000","Doctors per 10'000","Hospital doctors per 10'000", "Incidence")) %>%
  mutate(Cofactor=factor(Cofactor, levels = c("Tuberculosis mortality","Share of >=60 year-olds","Share of 20-40 year-olds","Share of 5-14 year-olds",
                                              "Share of men","Households per house","Household size",
                                              "GDP","Share industry","Private doctors per 10'000",
                                              "Population density","Urbanicity (Ref: Rural)")),
         Cofactor = recode(Cofactor,"Private doctors per 10'000"="Doctors per 10'000"),ac=factor(fac, levels = c("Excess Mortality","Incidence"))
  )

  

ggplot(dt, aes(x=Cofactor,ymin=Cl, ymax=Cu,y=est, col=fac),position=pd) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwd_size ) + 
  geom_pointrange(lwd=3,position=pd, fatten=fatten_size,lwd=lwd_size) +
  xlab("") +
  ylab("Regression coefficients of z-scores and 95% CI") +

scale_color_manual("",
                   # values= c("grey30","grey60"),
                   guide = guide_legend(reverse = TRUE),
                   values= c(cbp1[3],cbp1[2]))+
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
          axis.text.x = element_text(size=size_axis_x,hjust=1),
          axis.title = element_text(size=axis_legend_size),
          title =element_text(size=title_size))+
    coord_flip()
  # coord_flip(ylim=c(-30, 30))



ggsave("figures/Figure_excess1918_regression2.png",h=15,w=18)
