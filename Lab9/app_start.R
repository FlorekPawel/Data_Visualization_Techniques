library(shiny)
library(palmerpenguins)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
data("penguins")


ui <- fluidPage(
  
  titlePanel("Analiza danych o pingwinach"),
  
  
  fluidRow(
    column(6, 
           textOutput("text"),
           
           checkboxGroupInput("gatunki",
                              "Jakie gatunki pingwinów wybierasz",
                              c(unique(penguins$species)[order(unique(penguins$species))]),
                              selected = unique(penguins$species)),
           
           sliderInput("rok", "Rok",
                       min = min(penguins$year), max = max(penguins$year),
                       value = c(min(penguins$year), max(penguins$year)), step = 1)
    ),
    column(6,
           
           selectInput("zmienna_1",
                       "Dla jakiej zmiennej narysować rozkład?",
                       c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")),
           
           selectInput("zmienna_2",
                       "Druga zmienna",
                       c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"))
           
    )
  ),
  fluidRow(
    column(6,
           plotlyOutput("pointPlot")
    ),
    column(6,
           plotlyOutput("histPlot")
    )
  ),
  fluidRow(
    column(6, 
            tableOutput("table")
    )
  )
)


server <- function(input, output) {
  
  output$table <- renderTable({
   
  })
  
  output$text <- renderText({
    n = length(penguins$species)
    minMasa = min(penguins$body_mass_g, na.rm = TRUE)
    
    paste("Aplikacja zawiera wstępną analizę zbioru danych o pingwinach. 
           W zbiorze danych jest ", n ," pingwinów.
           Najmniejszy waży ", minMasa, " gramów!")
    
  })
  
  output$pointPlot <- renderPlotly({
    penguins %>% 
      filter(year %in% input$rok) %>%
      filter(species %in% input$gatunki) %>% 
      plot_ly(
      x = ~flipper_length_mm,
      y = ~body_mass_g,
      color = ~species,
      colors = "Set1",
      type = "scatter"
    ) %>% 
      layout(
        title = "Zależność długości skrzydła a masą ciała", 
        xaxis = list(title = 'Długość skrzydła [mm]', range = c(160, 240), nticks = 8),
        yaxis = list(title = 'Masa [g]', range = c(2500, 6500), nticks = 9)
        
      )
   
  })
  
  output$histPlot <- renderPlotly({
    
    p <- penguins %>% 
      filter(year %in% input$rok) %>%
      filter(species %in% input$gatunki) %>% 
      ggplot(aes_string(x = input$zmienna_1, fill = "species")) +
      geom_histogram() +
      ggtitle(paste("Rozkład dla zmiennej", input$zmienna_1))
    
    ggplotly(p)
    
   
  })
  
  
  
}


shinyApp(ui = ui, server = server)

