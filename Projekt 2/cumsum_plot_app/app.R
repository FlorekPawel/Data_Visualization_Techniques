#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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

# Define UI for application that draws a histogram
ui <- fluidPage(
  plotlyOutput("linePlot"),
  sliderInput("dateInput","",
              min = min(df_result$date), max = max(df_result$date),
              value = c(min(df_result$date), max(df_result$date)),
              step = 1)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linePlot <- renderPlotly({
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
                            range = c(input$dateInput[1], input$dateInput[2]),
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
