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
pd <-position_dodge(width=0.3)
fatten_size <- 8


dt1 <- read.xlsx("output/Regression_rlm_zscore_1918.xlsx") %>%
  mutate(fac="Incidence")

dt2 <- read.xlsx("output/Regression_rlm_excess_scale.xlsx") %>%
  mutate(fac="Excess Mortality")

dt <- rbind(dt1, dt2) %>%
  mutate(Cofacter = factor(Cofactor, 
                           levels = c(  "Share of >= 60 years old",
                                        "Share of 20-39 years old" ,
                                        "Share of 5-14 years old",
                                        "Share of men",
                                        "Households per house",
                                        "Household size",
                                        "Private physicians per km2",
                                        "Share of industry",
                                        "GDP per capita",
                                        "Population density"
                                        )),
         fac=factor(fac, levels = c("Excess Mortality","Incidence"))
  )


ggplot(dt, aes(x=factor(Cofactor, 
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
               ,ymin=Cl, ymax=Cu,y=est, col=fac),position=pd) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwd_size -1.8 ) + 
  geom_pointrange(position=pd, fatten=fatten_size,lwd=lwd_size) +
  xlab("") +
  ylab("Regression coefficients of z-values and 95% CI") +

scale_color_manual("",
                   # values= c("grey30","grey60"),
                   guide = guide_legend(reverse = TRUE),
                   values= col2)+
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
  # coord_flip(ylim=c(-30, 30))



ggsave("figures/Figure5.png",h=15,w=18)
