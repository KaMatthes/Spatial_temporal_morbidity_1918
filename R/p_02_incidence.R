
lwd_size <- 1.5
lwd_size_vline <- 0.8
text_size <- 25
text_size_heat_map <- 15
legend_size <- 20
axis_legend_size <- 25
axis_legend_size_heat_map <- 15
title_size <- 20

size_axis_x <- 20
size_axis <-15
size_axis_title <- 15
pd <-position_dodge(width=0.5)
fatten_size <- 12

dt <- read.xlsx("data/Faelle_Bezirke_total_pop.xlsx", detectDates = TRUE) %>%
  group_by(Kanton,Startdatum) %>%
  mutate(
    Cases_canton = sum(Cases_Bezirk),
    Pop_canton = sum(pop)
    ) %>%
  ungroup() %>%
  mutate(
    inc_canton = Cases_canton/Pop_canton *10000
  ) %>%
  distinct(Kanton,Startdatum, .keep_all = TRUE) 

dt_t <- dt %>%
  group_by(Startdatum) %>%
  mutate(
    Cases_total = sum(Cases_canton),
    Pop_total = sum(Pop_canton)
  ) %>%
  ungroup() %>%
  mutate(
    inc_total = Cases_total/Pop_total *10000
  ) %>%
  distinct(Startdatum, .keep_all = TRUE) 

ggplot(dt_t) +
  geom_line(aes(y=inc_total ,x= Startdatum), lwd=lwd_size) +
  scale_x_date(labels = date_format("%Y/%m/%d"), 
               breaks = date_breaks("4 weeks")) +

  # annotate("rect",xmin=ymd("1918-07-15"),xmax=ymd("1918-08-15"),ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # scale_color_manual(name = "",
  #                    label =c("City of Zurich","Canton Zurich"),
  #                    values = c(col_pal[1],  col_pal[4]))+
  xlab("Week/Year")+
  ylab("per 10'000 inhab.")+
  # ggtitle("Incidence") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(
    text = element_text(family = "serif", colour ="black"),
    axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(size = text_size),
    legend.position = c(.62, .82),
    legend.key.size = unit(1.2, "cm"),
    legend.text=element_text(size=legend_size),
    axis.text.x = element_text(size=size_axis_x,angle =45,hjust=1),
    axis.title = element_text(size=axis_legend_size),
    title =element_text(size=title_size))


ggsave("figures/Figure_inc.png",h=10,w=18)


ggplot(dt) +
  geom_line(aes(y=  inc_canton,x= Startdatum), lwd=lwd_size) +
  facet_wrap(~Kanton, ncol=5) +
  scale_x_date(labels = date_format("%m/%y"), 
               breaks = date_breaks("4 month")) +
  
  # annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # scale_color_manual(name = "",
  #                    label =c("City of Zurich","Canton Zurich"),
  #                    values = c(col_pal[1],  col_pal[4]))+
  xlab("Month/Year")+
  ylab("per 10'000 inhab.")+
  # ggtitle("Incidence") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(
    text = element_text(family = "serif", colour ="black"),
    axis.text.y = element_text(size=text_size+10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(size = text_size+10),
    legend.position = c(.62, .82),
    legend.key.size = unit(1.2, "cm"),
    legend.text=element_text(size=legend_size+10),
    axis.text.x = element_text(size=size_axis_x+10,angle =45,hjust=1),
    axis.title = element_text(size=axis_legend_size+10),
    title =element_text(size=title_size))


ggsave("figures/Figure_inc_canton.png",h=18,w=25)

coeff<- 4
dt_mort <- readRDS("data/Switzerland_results_month_last_7_notrim.Rds") %>%
  filter(Year %in% c(1918:1920),
         Model %in% "Global Serfling (Stan, NB, last 7 no trim)") %>%
  mutate(excess = Deaths - pred_total_deaths,
         exc_rel = excess/pred_total_deaths*100*coeff) %>%
  select(month_year = Date, exc_rel )

dt_m <- dt_t %>%
  # filter(year %in% 1918) %>%
  mutate(
    Month = month(Startdatum),
    month_year = ymd(paste0(year,"-", Month, "-01"))
  ) %>%
  group_by(month_year) %>%
  mutate(
    cases_all = sum(Cases_total),
    ) %>%
  ungroup() %>%
  distinct(month_year, .keep_all = TRUE) %>%
  mutate(inc_month = cases_all/Pop_total*10000) %>%
  left_join(dt_mort) %>%
  select(month_year, inc_month,exc_rel) %>%
  gather(., con, value, inc_month:exc_rel) %>%
  filter(month_year < ymd("1920-05-01"))


ggplot(dt_m) +
  geom_bar(aes(x = month_year, y = value, col=con, fill=con), stat = "identity",  position = "dodge", linewidth = lwd_size) +
  scale_x_date(labels = date_format("%m/%y"), 
               breaks = date_breaks("1 month")) +
  scale_color_manual("",
    labels=c("Incidence", "Excess Mortality"),
    values= c(cbp1[2],cbp1[3])
    )+
  scale_fill_manual("",
    labels=c("Incidence", "Excess Mortality"),
    values= c(cbp1[2],cbp1[3]))+
  scale_y_continuous(
    name = "Incidence per 10'000",
    sec.axis = sec_axis(~./coeff, name = "Excess mortality")) +
  xlab("Month/Year")+
  theme_bw()+
  theme(
    text = element_text(family = "serif", colour ="black"),
    axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(size = text_size),
    legend.position = c(0.6, 0.8),
    legend.key.size = unit(1.2, "cm"),
    legend.text=element_text(size=legend_size),
    axis.text.x = element_text(size=size_axis_x,angle =45,hjust=1),
    axis.title = element_text(size=axis_legend_size),
    title =element_text(size=title_size))

ggsave("figures/Figure_inc_excess.png",h=10,w=18)
