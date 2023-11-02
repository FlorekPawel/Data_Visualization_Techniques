###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 4            ###
###########################################


library(dplyr)
library(ggplot2)
library(SmarterPoland)
library(tidyverse)

countries[countries$country == "Niue", ] # https://en.wikipedia.org/wiki/Niue
sum(countries$population) # `population` jest w tysiącach

## Zadanie 1
# 1. Ograniczyć zbiór krajów do tych, których nazwa jest krótsza niż 8 znaków (nchar).
# 2. Stworzyć zmienną logarytm populacji (*1000) 
#    i posortować względem niej poziomy zmiennej kraju (forcats::fct_reorder).
# 3. Zrobić wykres słupkowy pokazujący logarytm populacji w poszczególnych krajach 
#    i zaznaczyć kolorem kontynent (wykres poziomy).

countries[nchar(countries$country) < 8,] %>% 
  mutate(logpop = log(population*1000),
         country = forcats::fct_reorder(country, logpop)) %>%
  ggplot(aes(x= logpop, y= country, fill=continent)) +
  geom_bar(position = "dodge", stat = "identity") -> p_col
  


### Skale (scale)


## Osie (x & y) 

p_col + scale_y_discrete(position = "right")

p_col + scale_y_discrete(guide = guide_axis(n.dodge = 2, angle = 5, title = "Count")) +
  theme(axis.text.y = element_text(size = 6))


p_point <-  ggplot(countries, aes(x = birth.rate,
                                  y = death.rate,
                                  color = continent)) +
  geom_point()

p_point + 
  scale_x_continuous(position = "top")
  
p_point +
  scale_y_reverse() + scale_x_reverse()

p_point + scale_y_continuous(expand = c(0,0))



p_point + scale_y_log10()
p_point + scale_y_sqrt()


## Kolor (color & fill)

p_point + scale_color_manual(values = c("black", "red", "darkgreen", "violet", "blue"))


# color brewer http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
# install.packages("RColorBrewer")
library(RColorBrewer)
RColorBrewer::brewer.pal(n = 5, name = "Blues")

ggplot(countries, aes(x= log(population),
                      y= death.rate,
                      color= birth.rate)) +
  geom_point(size = 2) + 
  scale_color_gradient(low = "lightblue", high = "darkblue")


## Zadanie 2
# 1. Ograniczyć zbiór krajów, do tych z Azji i Europy (można wykorzystać dane z Zad1).
# 2. Policzyć stosunek współczynnika zgonów do współczynnika urodzeń.
# 3. Zrobić wykres słupkowy pokazujący logarytm populacji w poszczególnych krajach 
#    i zaznaczyć kolorem wskaźnik.
countries %>% 
  filter(continent %in% c("Asia", "Europe")) %>% 
  mutate(death_to_birth_rate = death.rate/birth.rate,
         country = forcats::fct_reorder(country, log(population))) %>%
  ggplot(aes(x= log(population),
             y= country,
             fill= death_to_birth_rate)) +
  geom_col() +
  #scale_color_gradient(low = "lightblue", high = "darkblue") +
  theme(axis.text.y = element_text(size = 4)) +
  scale_fill_gradient2(
    low= "navyblue",
    mid= "white",
    high= "red",
    limits= c(0, 1.5),
    midpoint= 0.5
  )

### Legenda (theme & legend)
p_point +
  theme(legend.position = "bottom")
p_point +
  theme(legend.position = "top")
p_point +
  theme(legend.text = element_blank())

p_point +
  theme(legend.position = "top", 
        legend.title = element_text(color= "blue",
                                    size= 15),
        legend.text = element_text(color= "red",
                                   face= "bold"))
### Koordynaty (coord)

p_point + coord_flip()

p_point + coord_polar()

# wykres kołowy (DANGER ZONE)
ggplot2::geom_pie()

countries %>% 
  group_by(continent) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=continent)) +
  geom_col()+
  coord_polar("y")

## Zadanie 3
# 1. Stworzyć zmienną wielkość kraju, która przyjmuje K wartości w zależności
#    od podziału zmiennej populacji (np. K = 3, można wykorzystać dane z Zad1).
# 2. Zrobić wykres punktowy pokazujący zależność death.rate od birth.rate 
#    i zaznaczyć kolorem wielkość kraju.

countries %>% 
  mutate(logpop = log(population*1000)) %>% 
  mutate(size = case_when(quantile(logpop, 0.33) >= logpop ~ "small",
                       quantile(logpop, 0.66) >= logpop & quantile(logpop, 0.33) < logpop ~ "medium",
                       TRUE ~ "big")) %>% 
  ggplot(aes(x= death.rate, y= birth.rate, color= size))+
  geom_point() -> p

### Panele (facet)

p + facet_wrap(~continent, scale= "free_x")

p + facet_wrap(~continent, scale= "free_y")

p + facet_wrap(~continent, scale= "free")

p + facet_wrap(size~continent, ncol= 3)


### How to plot? --->>> https://www.r-graph-gallery.com
