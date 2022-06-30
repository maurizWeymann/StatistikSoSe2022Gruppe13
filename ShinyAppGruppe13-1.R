if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,shiny,shinydashboard,tidyverse,data.table,DT,highcharter,ggplot2) 
#Oskar
titanic_data <- data.table::fread("titanic_data.csv")
#Mauriz
#titanic_data <- data.table::fread("C:/Users/49177/git/htw/Statistik/StatistikSoSe2022Gruppe13/titanic_data.csv")
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
titanic_data[titanic_data == ""] <- NA
#titanic_data %>%
#  group_by(Survived) %>%
#  summarise(n = n()) 


ui <- dashboardPage(
  
  dashboardHeader(title = "Titanic Dashboard", titleWidth = 250),
  
  dashboardSidebar( width = 250,
    h3("Wähle deine Variablen"),
    radioButtons("plot", "Plot?", choices = c("Yes","No"), selected = "No" ),
    selectInput("feature", "Feature", choices = colnames(titanic_data)[2:9], selected = colnames(titanic_data)[2]),
    sliderInput("kluster_input_slider","Wähle die Anzahl der Cluster", min = 2, max = 6, value = 3, ticks = FALSE),
    conditionalPanel(condition = "input.feature == colnames(titanic_data)[2]",
                     sliderInput(inputId = "f", label = "Smoother span:",
                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                 animate = animationOptions(interval = 100)))
  ),
  dashboardBody(
    plotOutput("all"),
    plotOutput("flexPlot"),
    plotOutput("sexAgeAndFare")
  )

)

server <- function(input, output, session) {
      tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: white;
      }
      .h3, h3 {
        font-family: 'Yusei Magic', sans-serif;
            padding: 100px;
       
      }
      .shiny-input-container {
        color: #474747;
      }"))
  output$flexPlot <- renderPlot({
    if( input$plot == "Yes" ){
      #flexPlot
      ggplot(titanic_data, aes(x = Survived)) + 
        geom_bar()+
        geom_text(
          aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + labs(y = "Number of Passengers", title = paste("Number of Passengers", "vs. Survival Rate")
      ) 
    }
  })
  output$all <- renderPlot({
    
    if( input$feature == colnames(titanic_data)[2] ){
      #Survival rate vs class
      ggplot(titanic_data, aes(x = Class, fill = Survived)) +
        geom_bar() +
        labs(y = "Number of Passengers",
             x = "Passenger class",
             title = "Survival Rates vs Class")
      
    }else if( input$feature == colnames(titanic_data)[3] ){
      #Survival rate vs Sex
      ggplot(titanic_data, aes(x = Sex, fill = Survived,label = scales::percent(prop.table(stat(count))))) +
        geom_bar() +
        geom_text(stat = 'count',
                  vjust = -0.4, 
                  size = 3) + 
        labs(y = "Number of Passengers",
             title = "Survival Rates vs Sex")
      
    }else if( input$feature == colnames(titanic_data)[4] ){
      #Survival rate vs age
      ggplot(titanic_data, aes(x = Age, fill = Survived)) +
        geom_histogram(binwidth = 10) +
        labs(y = "Number of Passengers",
             x = "Age (binwidth = 10)",
             title = "Survival Rates vs Age")
      ggplot(titanic_data, aes(x = Age, fill = Survived)) +
        geom_density(alpha= 0.7) +
        labs(y = "Number of Passengers",
             x = "Age",
             title = "Survival Rates vs Age")
      ggplot(titanic_data, aes(x = Survived, y = Age)) +
        geom_boxplot() +
        labs(y = "Age",
             x = "Survived",
             title = "Survival Rates vs Age")
      
    }else if( input$feature == colnames(titanic_data)[5] ){
      #Survival rate vs Number of Siblings/Spouses Aboard
      ggplot(titanic_data, aes(x = Siblings, fill = Survived)) +
        geom_bar()+
        labs(y = "Number of Passengers",
             x = "Number of Siblings/Spouses Aboard",
             title = "Survival Rates vs Number of Siblings/Spouses Aboard")
    }else if( input$feature == colnames(titanic_data)[6] ){
      #Survival rate vs Number of Parents/Children Aboard             
      ggplot(titanic_data, aes(x = Parch, fill = Survived)) +
        geom_bar()+
        labs(y = "Number of Passengers",
             x = "Number of Parents/Children Aboard",
             title = "Survival Rates vs Number of Parents/Children Aboard")
      #Mit prozent
      # ggplot(titanic_data, aes(x = Parch, fill = Survived,label = scales::percent(prop.table(stat(count))))) +
      #   geom_bar()+
      #   geom_text(stat = 'count',
      #             vjust = -0.6, 
      #             size = 3) + 
      #   labs(y = "Number of Passengers",
      #        x = "Number of Parents/Children Aboard",
      #        title = "Survival Rates vs Number of Parents/Children Aboard")
    }else if( input$feature == colnames(titanic_data)[7] ){
      #Survival rate vs Fare
      # ggplot(titanic_data, aes(x = Fare, fill = Survived)) +
      #   geom_histogram(binwidth = 10) +
      #   labs(y = "Number of Passengers",
      #        x = "Fare (binwidth = 10)",
      #        title = "Survival Rates vs Fare")
      
      ggplot(titanic_data, aes(x = Fare, fill = Survived)) +
        geom_histogram(binwidth = 10) +
        labs(y = "Number of Passengers",
             x = "Fare (binwidth = 10)",
             title = "Survival Rates vs Fare")+
        xlim(0,300)
    }else if( input$feature == colnames(titanic_data)[8] ){
      #Survival rate vs Cabin
      ggplot(titanic_data %>% drop_na(), aes(x = Cabin, fill = Survived)) +
        geom_bar() +
        labs(y = "Number of Passengers",
             x = "Cabin",
             title = "Survival Rates vs Cabin")
      
      ggplot(titanic_data %>% drop_na(), aes(x = Cabin, fill = Survived)) +
        geom_bar() +
        facet_grid(Class ~.)+
        labs(y = "Number of Passengers",
             x = "Cabin",
             title = "Survival Rates vs Cabin")
    }else if( input$feature == colnames(titanic_data)[9] ){
      #Survival rate vs Embarked
      ggplot(titanic_data %>% drop_na(), aes(x = Embarked, fill = Survived,label = scales::percent(prop.table(stat(count))))) +
        geom_bar() +
        geom_text(stat = 'count',
                  vjust = -0.6, 
                  size = 3) + 
        labs(y = "Number of Passengers",
             x = "Passenger class",
             title = "Survival Rates vs Class")
    }
    
  })
  output$sexAgeAndFare <- renderPlot({
    #Survival Rates vs Sex Age and Fare
    ggplot(titanic_data,aes(x=Class,y=Fare,fill= Survived))+
      geom_boxplot()+
      facet_grid(Sex ~ .)+
      ylim(0,180)+
      labs(x = "Passenger class",
           title = "Survival Rates vs Sex Age and Fare")
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
