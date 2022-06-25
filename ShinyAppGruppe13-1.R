library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(DT)
library(highcharter)


titanic_data <- data.table::fread("C:/Users/49177/git/htw/Statistik/StatistikSoSe2022Gruppe13/titanic_data.csv") 
colnames(titanic_data)
#Daten aufbereitung
titanic_data <-  titanic_data %>% mutate(Age = replace(Age, Age>0 & Age<1, NA))#Statt Na 0?
titanic_data$Age <- as.integer(titanic_data$Age)
titanic_data$Survived <- factor(titanic_data$Survived,
                        levels = c(0,1),
                        labels = c("Died","Survived"))
titanic_data$Pclass <- factor(titanic_data$Pclass)
titanic_data$Sex<- factor(titanic_data$Sex)
titanic_data$Embarked <- factor(titanic_data$Embarked,
                        levels = c("C","Q","S"),
                        labels = c("Cherbourg","Queenstown","Southampton"))

#titanic_data %>%
#  group_by(Survived) %>%
#  summarise(n = n())


ui <- dashboardPage(
  dashboardHeader(title = "Titanic Dashboard", titleWidth = 400),
  dashboardSidebar(
    h2("Wähle deine Variablen"),
    selectizeInput("var_a", "Option A", choices = colnames(titanic_data)[3:7], selected = colnames(titanic_data)[3]),
    selectizeInput("var_b", "Option B", choices = colnames(titanic_data)[3:7], selected = colnames(titanic_data)[4]),
    sliderInput("kluster_input_slider","Wähle die Anzahl der Cluster", min = 2, max = 6, value = 3, ticks = FALSE)
  ),
  dashboardBody(
    highchartOutput("chart2"),
    #highchartOutput("chart")
  )
)

server <- function(input, output, session) {
  chart_data <-  reactive({
    subdf <- titanic_data %>%
      select(c(input$var_a, input$var_b))
    
    subdf$cluster <- kmeans(subdf, input$kluster_input_slider)$cluster
    subdf
  })

#  output$chart <- renderHighchart({
#   highchart() %>%
#      hc_add_series(chart_data(), type="bar", hcaes(x = !!sym(input$var_a), y = !!sym(input$var_b), group = "cluster"))
#  })
  
  output$chart2 <- renderHighchart({
    highchart() %>%
      hc_add_series(chart_data(), type="bar", hcaes(x = !!sym(input$var_a), y = !!sym(input$var_b), group = "cluster"))
    #Survival rate
    ggplot(titanic_data, aes(x = Survived)) + 
      geom_bar()+
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
      labs(y = "Number of Passengers",
           title = "Survival Rates")
    })
  
}

shinyApp(ui, server)

## sample code von https://github.com/Data-Mastery/RShiny_Iris/blob/main/app.R ##
