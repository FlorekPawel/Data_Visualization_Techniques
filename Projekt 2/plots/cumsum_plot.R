library(dplyr)
library(plotly)

df <- read.csv("C:/PW/TWD/Projekt 2/data.csv")

df$date <- format(as.Date(df$date, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")


df1 <- df %>% 
  select(date) %>% 
  group_by(date) %>% 
  mutate(count = n()) %>% 
  distinct() %>% 
  arrange(date)

df_cumsum <- apply(df1[, -1], 2, cumsum)

df_result <-  data.frame(cbind(date = df1$date, df_cumsum))
df_result$count <- as.numeric(df_result$count)
df_result$date <- as.Date(df_result$date)


plot_ly(
  data = df_result,
  x = ~date,
  y = ~count,
  type = 'scatter', 
  mode = 'lines',
  line = list(color = "#FF00FF")
) %>% 
  layout(title =  list(text = "Przyrost liczby wiadomości", font = list(color = "#FFFFFF")),
         xaxis = list(title = "Data",
                      fixedrange = TRUE, 
                      categoryorder = "total ascending",
                      tickfont = list(color = "#FFFFFF"), 
                      titlefont = list(color = "#FFFFFF", size = 20)), 
         yaxis = list(title = "Liczba wiadomości",
                      fixedrange = TRUE,
                      tickmode = "linear",
                      dtick = 50000,
                      categoryorder = "total ascending",
                      tickfont = list(color = "#FFFFFF"), 
                      titlefont = list(color = "#FFFFFF", size = 20)),
         paper_bgcolor = "#121212",
         plot_bgcolor = "#121212") %>%
  config(displayModeBar = FALSE)
