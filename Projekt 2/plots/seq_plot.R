library(plotly)
library(dplyr)

df <- read.csv("C:/PW/TWD/Projekt 2/seq_data.csv")

df$sequence = tolower(df$sequence)

df <- df %>% 
  filter(sequence != "paweł pozorski") %>% 
  group_by(sequence) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(10)

plot_ly(
  data = df,
  x = ~sequence,
  y = ~count,
  type= "bar",
  marker = list(color = "#FF00FF")
) %>% 
  layout(
    title =  list(text = "Wykres najpopularniejszych sekwencji słów", font = list(color = "#FFFFFF")),
    xaxis = list(title = "Sekwencja",
                 fixedrange = TRUE, 
                 categoryorder = "total descending",
                 tickfont = list(color = "#FFFFFF"), 
                 titlefont = list(color = "#FFFFFF", size = 20)), 
    yaxis = list(title = "Liczba wystąpień",
                 fixedrange = TRUE,
                 tickfont = list(color = "#FFFFFF"), 
                 titlefont = list(color = "#FFFFFF", size = 20)),
         paper_bgcolor = "#121212",
         plot_bgcolor = "#121212") %>%
  config(displayModeBar = FALSE)


