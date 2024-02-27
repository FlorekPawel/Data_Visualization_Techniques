library(dplyr)
library(plotly)

df <- read.csv("C:/PW/TWD/Projekt 2/data_csv/data.csv")

df_total <- df %>%   
  group_by(conv_id) %>% 
  summarise(total_count = n()) %>% 
  right_join(df) %>% 
  select(conv_id, total_count, is_group) %>% 
  unique()

df_sent <- df %>% 
  select(conv_id, sender, content) %>% 
  filter(sender == "Paweł Florek") %>% 
  group_by(conv_id) %>% 
  summarise(sent_count = n()) %>% 
  right_join(df) %>% 
  select(conv_id, sent_count) %>% 
  unique()
df_sent <- replace(df_sent, is.na(df_sent), 0)

df_total <- df_total %>% 
  left_join(df_sent) %>% 
  mutate(perc_sent = round(sent_count/total_count *100, 2))

df_total <- replace(df_total, df_total == "True", "Grupa")
df_total <- replace(df_total, df_total == "False", "Prywatna")

plot_ly(
  data = df_total,
  x = ~total_count,
  y = ~perc_sent,
  color = ~is_group,
  type = "scatter",
  mode = 'markers',
  colors = c("#FFFFFF", "#FF00FF"),
  hoverinfo = "text",
  text = ~paste("Conv_id:", conv_id, "<br>Liczba wiadomości: ", total_count, "<br>Wysłane przez nas: ", perc_sent, "%", "<br>Rodzaj: ", is_group)
) %>% 
  layout(title =  list(text = "Zależność liczby wysłanych wiadomości i % wysyłanych przez nas", font = list(color = "#FFFFFF")),
         xaxis = list(title = "Liczba wiadomości",
                      fixedrange = TRUE, 
                      categoryorder = "total descending",
                      tickvals = c(1, 10, 100, 1000, 10000, 100000),
                      tickfont = list(color = "#FFFFFF"), 
                      titlefont = list(color = "#FFFFFF", size = 20),
                      type = 'log'), 
         yaxis = list(title = "% wysłanych przez nas",
                      fixedrange = TRUE,
                      tickfont = list(color = "#FFFFFF"), 
                      titlefont = list(color = "#FFFFFF", size = 20)),
         legend = list(
           title = list(text = "Rodzaj konwersacji", 
                        font = list(size = 14, 
                                    color = '#FFFFFF')),
           font = list(color = '#FFFFFF', size = 12)
         ),
         paper_bgcolor = "#121212",
         plot_bgcolor = "#121212") %>%
  config(displayModeBar = FALSE)

