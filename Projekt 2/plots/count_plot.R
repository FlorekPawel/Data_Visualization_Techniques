library(plotly)
library(dplyr)

df = read.csv("C:/PW/TWD/Projekt 2/data_csv/data.csv")

df <- df %>% 
  filter(is_group == "False") %>% 
  filter(conv_id != "treehappyfriends") %>% 
  select(conv_id, content)

df <- df %>%   
  group_by(conv_id) %>% 
  summarise(count = n()) %>% 
  right_join(df) %>% 
  select(conv_id, count) %>% 
  unique() %>% 
  arrange(desc(count)) %>% 
  head(10)

df$conv_id <- c('africa1', 'africa2', 'africa3', 'africa4', 'africa5','africa6', 'africa7', 'africa8', 'africa9', 'africa10')

plot_ly(data = df, 
        x = ~conv_id, 
        y=~count, 
        type = "bar",
        marker = list(color = "#FF00FF")
        ) %>% 
  layout(title =  list(text = "Wykres osób z największą liczbą wiadomości", font = list(color = "#FFFFFF")),
         xaxis = list(title = "Osoba",
                      fixedrange = TRUE, 
                      categoryorder = "total descending",
                      tickfont = list(color = "#FFFFFF"), 
                      titlefont = list(color = "#FFFFFF", size = 20)), 
         yaxis = list(title = "Liczba wiadomości",
                      fixedrange = TRUE,
                      tickfont = list(color = "#FFFFFF"), 
                      titlefont = list(color = "#FFFFFF", size = 20),
                      type = 'log'),
         paper_bgcolor = "#121212",
         plot_bgcolor = "#121212") %>%
  config(displayModeBar = FALSE) 

        