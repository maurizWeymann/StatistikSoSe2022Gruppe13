library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(DT)
library(highcharter)


df <- data.table::fread("C:/Users/49177/git/htw/Statistik/StatistikSoSe2022Gruppe13/titanic_data.csv") 
colnames(df)


df %>%
  group_by(Survived) %>%
  summarise(n = n())


ui <- dashboardPage(
  dashboardHeader(title = "Titanic Dashboard", titleWidth = 400),
  dashboardSidebar(
    h2("Wähle deine Variablen"),
    selectizeInput("var_a", "Option A", choices = colnames(df)[2:7], selected = colnames(df)[2]),
    selectizeInput("var_b", "Option B", choices = colnames(df)[2:7], selected = colnames(df)[3]),
    sliderInput("kluster_input_slider","Wähle die Anzahl der Cluster", min = 2, max = 6, value = 3, ticks = FALSE)
  ),
  dashboardBody(
    highchartOutput("chart"),
    highchartOutput("chart2")
  )
)

server <- function(input, output, session) {
  chart_data <-  reactive({
    subdf <- df %>%
      select(c(input$var_a, input$var_b))
    
    subdf$cluster <- kmeans(subdf, input$kluster_input_slider)$cluster
    subdf
  })
  
  output$chart <- renderHighchart({
    highchart() %>%
      hc_add_series(chart_data(), type="bar", hcaes(x = !!sym(input$var_a), y = !!sym(input$var_b), group = "cluster"))
  })
  output$chart2 <- renderHighchart({
    highchart() %>%
      hc_add_series(chart_data(), type="scatter", hcaes(x = !!sym(input$var_a), y = !!sym(input$var_b), group = "cluster"))
  })
  
}

shinyApp(ui, server)

## sample code von https://github.com/Data-Mastery/RShiny_Iris/blob/main/app.R ##
