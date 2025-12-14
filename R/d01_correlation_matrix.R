
rm(list=ls())
source("R/00_setup.R")


# plot parameter

legend_size <- 25
size_axis <- 25



# load data

dtd <- read.csv("data/Determinants.csv", sep=";") %>%
  rename(Bezirk = district_nr) %>%
  select(-district_name, -Bezirk) 

cor_mat <- cor(dtd)

# Convert to long format
cor_long <- as.data.frame(cor_mat) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "cor") %>%
  mutate(
    cor=round(cor,2),
    var1 = recode(var1, 
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
    var2 = recode(var2, 
                  "dens_doc" = "Private physicians per km2",
                  "share_male"  = "Share of men",
                  "share_5_14"  = "Share of 5-14 years old",
                  "share_20_39"    =  "Share of 20-39 years old",
                  "share_60"  = "Share of >= 60 years old",
                  "share_industry"     = "Share of industry",
                  "gdp" = "GDP per capita",
                  "dens_pop"    = "Population density",
                  "houshold_house"  = "Households per house",
                  "household_size" = "Household size")) %>%
  filter(as.numeric(factor(var1)) > as.numeric(factor(var2))) 

vu <- unique(cor_long$var1)



ggplot(cor_long) +
  geom_tile(aes(var1, var2, fill = cor)) +
  scale_fill_gradient2(
    low = "darkblue", mid = "white", high = "darkred",
    midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(var1, var2,label = cor),family = "serif",size = 8,
            show.legend = FALSE) +
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(
    text = element_text(family = "serif", colour ="black"),
    axis.text.y= element_text(size=size_axis),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(size = size_axis),
    legend.position = "none",
    legend.text=element_text(size=legend_size),
    axis.text.x = element_text(size=size_axis,hjust=1, angle = 45),
    axis.title = element_text(size=size_axis))+
  coord_fixed()

ggsave("figures/Figure_corr.png",h=15,w=15)
ggsave("figures/Figure_corr.pdf",h=15,w=15)
