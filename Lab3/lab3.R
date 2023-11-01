###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 3            ###
###########################################

library(SmarterPoland)
library(dplyr)
?countries
View(countries)

# Zadanie 0.
# Określ typy zmiennych:
# country - jakościowa nominalna
# birth.rate - ilościowa ilorazowa
# death.rate - ilościowa ilorazowa
# population - ilościowa ilorazowa
# continent - jakościowa nominalna

# ggplot2
library(ggplot2)

## 1. Główna funkcja ggplot2 ----> ggplot()
ggplot()
ggplot(countries)

## 2. Mapowania ----> aes()
ggplot(data = countries, mapping = aes(x= birth.rate,
                                       y= death.rate))
## 3. Operator '+'
ggplot(data = countries, mapping = aes(x= birth.rate,
                                       y= death.rate)) +
  geom_point()

p <- countries %>% 
  ggplot(mapping = aes(x= birth.rate,
                        y= death.rate)) +
  geom_point()

# Stosujemy podobne formatowanie jak w dplyr

# Łączenie przez pipes %>% (skrót Ctrl + Shift + m)

## 4. Wykresy do badania rozkładu jednej zmiennej (ilościowej)

# a) histogram, geom_histogram() lub stat_bin()
ggplot(countries, aes(x= population))

options(scipen = 12)

ggplot(countries, aes(x= population)) + 
  geom_histogram(bins= 100)

ggplot(countries, aes(x= population)) + 
  geom_histogram(binwidth = 5000) + 
  ggtitle("Histogram dla zmiennej populacji",
          subtitle = "rok 2013")

ggplot(countries, aes(x= population)) + 
  geom_histogram(binwidth = 20000, color= "red", fill= "blue") + 
  labs(title= "Histogram dla zmiennej populacji", subtitle = "Rok 2013",
       x= "populacja",
       y= "częstość")
  
# b) jądrowy estymator gęstości, geom_density() lub stat_density()
countries %>% 
  ggplot(aes(x= population, color= continent)) +
  geom_density(adjust= 5) + 
  xlim(0, 50000)

countries %>% 
  ggplot(aes(x= population, fill= continent)) +
  geom_density(adjust= 5, alpha =  0.5) + 
  xlim(0, 50000)

# c) boxplot, geom_boxplot() lub stat_boxplot()

countries %>% 
  ggplot(aes(x= population, y= continent)) +
  geom_boxplot(outlier.color = "red") +
  xlim(0, 50000) +
  coord_flip()

countries %>% 
  ggplot(aes(x= population, y= continent)) +
  geom_violin() +
  xlim(0, 50000) +
  coord_flip()

countries %>% 
  ggplot(aes(x= population, y= continent)) +
  geom_violin() +
  geom_boxplot() +
  xlim(0, 50000) +
  coord_flip()
  
# Zadanie 1
# Narysuj histogram wskaźnika urodzeń dla państw położonych w Europie,
# zadbaj o czytelność wykresu.
countries %>% 
  filter(continent == "Europe") %>% 
  ggplot(aes(x= birth.rate)) +
  geom_histogram(color= "white")

# Zadanie 2
# Narysuj wykres gęstości wskaźnika śmierci, zadbaj o czytelność wykresu.

countries %>% 
  ggplot(aes(x= death.rate, color= continent)) +
  geom_density(adjust= 1, alpha= 0.5)

## 5. Wykresy do badania rozkładu jednej zmiennej (jakościowej)

# wykres słupkowy, geom_bar(), geom_col()

countries %>% 
  ggplot(aes(x= continent)) + 
  geom_bar()

#tmp <- table(countries)
#tmp
#ggplot(tmp, aes(x= Var1, y= Freq))

## 6. Wykresy do badania rozkładu dwóch zmiennych (dwie zmienne ilościowe - numeryczne)

# a) dwie zmienne numeryczne bez porządku (np. bez zależności od czasu)
countries %>% 
  ggplot(aes(x= birth.rate, y= death.rate, color= continent)) +
  geom_point()

# wykres punktowy

# b) jedna zmienna ilościowa, jedna jakościowa
countries %>% 
  ggplot(aes(x= birth.rate, y= continent)) +
  geom_violin()
  


spotify_2023 <- read.csv('C:/PW/TWD/HW1/spotify-2023.csv')


# Zadanie 3
# Zbadaj rozkład taneczności piosenek w poszczególnych miesiącach.
spotify_2023 %>% 
  ggplot(aes(x= danceability_., y= as.factor(released_month))) +
  geom_boxplot() + 
  coord_flip()

# Zadanie 4
# Ile wydano piosenek w poszczególnych latach?
spotify_2023 %>% 
  ggplot(aes(x= released_year)) +
  geom_bar(color= "white") +
  xlim(1950, 2024)

# Zadanie 5
# Jak wygląda zależnośc pomiędzy energią a tanecznością utworów? Dodaj podział 
# ze względu na skalę (mode).
spotify_2023 %>% 
  ggplot(aes(x=energy_., y= danceability_., color= mode)) +
  geom_point()


