library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(DT)
library(highcharter)
library(ggplot2)


titanic_data <- data.table::fread("C:/Users/49177/git/htw/Statistik/StatistikSoSe2022Gruppe13/titanic_data.csv")
#Daten aufbereitung
titanic_data <- titanic_data %>%  transmute(
  Survived =  factor(Survived, 
                     levels = c(0,1), 
                     labels = c("Died","Survived") 
                     ),
  Class = factor(Pclass), 
  Sex = factor(Sex),
  Age = as.integer(Age),
  Siblings = SibSp,
  Parch,
  Fare = round(Fare,2),
  Cabin = gsub("[^a-zA-Z]", "", Cabin),
  Embarked = factor(Embarked,
                     levels = c("C","Q","S"),
                     labels = c("Cherbourg","Queenstown","Southampton"))
  )

ui <- dashboardPage (
  # Application title
  dashboardHeader("Titanic Gruppe"),
  dashboardSidebar(
    h2("Wähle deine Variablen"),
    selectizeInput("var_a", "Option A", choices = colnames(titanic_data)[3:7], selected = colnames(titanic_data)[3]),
  ),
  dashboardBody(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Überlebenswarscheinlichkeit Gesamt",
        fluidRow(
          selectizeInput("var_b", "Option B", choices = colnames(titanic_data)[3:7], selected = colnames(titanic_data)[4]),
          sliderInput("kluster_input_slider","Wähle die Anzahl der Cluster", min = 2, max = 6, value = 3, ticks = FALSE),
        ),
        fluidRow(
          plotOutput("overall"),
        )
      ),
      tabPanel(
        "Überlebenswarscheinlichkeit je Klasse",
        plotOutput("rateVSclass"),
        
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$overall <- renderPlot({
    #Survival rate
    ggplot(titanic_data, aes(x = Survived)) + 
      geom_bar()+
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
      labs(y = "Number of Passengers",
           title = "Survival Rates")
    })
  output$rateVSclass <- renderPlot({
  #Survival rate vs class
  ggplot(titanic_data, aes(x = Class, fill = Survived)) +
    geom_bar() +
    labs(y = "Number of Passengers",
         x = "Passenger class",
         title = "Survival Rates vs Class")
    })
  
}
# Run the application 
shinyApp(ui = ui, server = server)



titanic_data$Age <- if (is.integer(titanic_data$Age)) 
titanic_data














