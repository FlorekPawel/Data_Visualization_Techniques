library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)


df <- read.csv("C:/PW/TWD/Projekty/Projekt 1/prc_fsc_idx__custom_8188216_page_linear.csv")
View(df)
dim(df)
str(df)
summary(df)
colnames(df)

df <- df %>% 
  select(Geopolitical.entity..reporting., TIME_PERIOD, OBS_VALUE)
View(df)

df1 <- df %>% 
  pivot_wider(names_from = TIME_PERIOD, values_from = OBS_VALUE)

colnames(df1) <- c("Country", "Y2019", "Y2023")
View(df1)

df2 <- df1 %>% 
  mutate(change = ((Y2023 - Y2019)/Y2019)*100)
View(df2)

df2[6,1] <- "Czech Republic"


world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
View(Europe)

eu_data <- merge(Europe, df2, by.x = "admin", by.y = "Country", all.x = TRUE)

ggplot(eu_data) +
  geom_sf(aes(fill = change)) +
  scale_fill_distiller(direction = 1) +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) + 
  theme_void() +
  theme(axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(title = "% change of food prices (EU, Island and Norway)")
