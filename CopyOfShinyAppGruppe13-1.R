if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,shiny,shinydashboard,tidyverse,data.table,DT,highcharter,ggplot2,plotly) 
#Oskar
titanic_data <- data.table::fread("titanic_data.csv")
#Mauriz
#titanic_data <- data.table::fread("C:/Users/49177/git/htw/Statistik/StatistikSoSe2022Gruppe13/titanic_data.csv")
#Daten aufbereitung
titanic_data <- data.table::fread("titanic_data.csv")
titanic_data <-  titanic_data %>% mutate(Age = replace(Age, Age>0 & Age<1, NA))
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
  Cabin = substr( gsub("[^a-zA-Z]", "", Cabin), 1, 1),
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
    selectizeInput(inputId = "state",label = "Bundesland",choices = NULL),
    sliderInput(inputId =  "num_features", label = "Choose number of features", value = 1, min = 1, max = 4, ticks = FALSE),
    selectInput("feature", "Feature 1", choices = colnames(titanic_data)[2:9], selected = colnames(titanic_data)[2]),
    # Display only if can be plotted relative
    conditionalPanel(condition = "input.feature == 'Class' || input.feature == 'Sex' || input.feature == 'Age' || input.feature == 'Siblings' || input.feature == 'Parch' || input.feature == 'Fare' || input.feature == 'Cabin'  || input.feature == 'Embarked' ",
                    #h2(input.feature),
                    radioButtons("relAbs", "relativ oder absolut?", choices = c("relativ","absolut"), selected = "absolut" ),
                    HTML("Not available for all"),
                    conditionalPanel(condition =  "input.num_features == '2' ",
                                     selectInput("feature2", "Feature 2", choices = colnames(titanic_data)[2:9] , selected = colnames(titanic_data)[2]),
                    ),
                    conditionalPanel(condition =  "input.num_features == '3' ",
                                     selectInput("feature2", "Feature 2", choices = colnames(titanic_data)[2:9] , selected = colnames(titanic_data)[2]),
                                     selectInput("feature3", "Feature 3", choices = colnames(titanic_data)[2:9] , selected = colnames(titanic_data)[2]),
                    ),
                    conditionalPanel(condition =  "input.num_features == '4' ",
                                     selectInput("feature2", "Feature 2", choices = colnames(titanic_data)[2:9] , selected = colnames(titanic_data)[2]),
                                     selectInput("feature3", "Feature 3", choices = colnames(titanic_data)[2:9] , selected = colnames(titanic_data)[2]),
                                     selectInput("feature4", "Feature 4", choices = colnames(titanic_data)[2:9] , selected = colnames(titanic_data)[2]),
                    ),
    )
  ),
  dashboardBody(
    plotOutput("all"),
    #tableOutput("changingTable"),
    dataTableOutput("changingTable"),
    #plotOutput("flexPlot"),
    #plotOutput("sexAgeAndFare"),
    #plotOutput("density")
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

  
output$changingTable <- renderDataTable(   
    if( input$feature == colnames(titanic_data)[2] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        titanic_data %>% group_by(Survived)
        

      }else{
        #Survival rate vs class - ABSOLUTE
        table(titanic_data$Survived,titanic_data$Class) #%>% group_by(titanic_data$Survived)
        
      }
      
    }else if( input$feature == colnames(titanic_data)[3] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        
      }else{
        #Survival rate vs class - ABSOLUTE
        
      }
    }else if( input$feature == colnames(titanic_data)[4] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        
      }else{
        #Survival rate vs class - ABSOLUTE
        
      }
    }else if( input$feature == colnames(titanic_data)[5] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        
      }else{
        #Survival rate vs class - ABSOLUTE
        
      }
    }else if( input$feature == colnames(titanic_data)[6] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        
      }else{
        #Survival rate vs class - ABSOLUTE
        
      }
    }else if( input$feature == colnames(titanic_data)[7] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        
      }else{
        #Survival rate vs class - ABSOLUTE
        
      }
    }else if( input$feature == colnames(titanic_data)[8] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        
      }else{
        #Survival rate vs class - ABSOLUTE
        
      }
    
    }else if( input$feature == colnames(titanic_data)[9] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        
      }else{
        #Survival rate vs class - ABSOLUTE
        
      }
      
    }
    #, hover = TRUE
)
    
  
  output$flexPlot <- renderPlot({
    if( input$plot == "Yes" && input$plot == "Yes" ){
    }  
    else if( input$plot == "Yes" ){
      #flexPlot
      ggplot(titanic_data, aes(x = Survived)) + 
        geom_bar()+
        geom_text(
          aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + labs(y = "Number of Passengers", title = paste("Number of Passengers", "vs. Survival Rate")
      ) 
    }
  })
  output$all <- renderPlot(
    
    if( input$feature == colnames(titanic_data)[2] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs class - RELETIVE
        ggplot(titanic_data, aes(x= Survived, group=Class)) + 
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust = -.5) +
          labs(y = "Percent", fill="Survived?",title = "Survival rate vs class") +
          facet_grid(~Class) +
          scale_y_continuous(labels = scales::percent)+
          guides(fill="none")
        
      }else{
        #Survival rate vs class - ABSOLUTE
        
        # vielleicht lieber neben einander als uebereinander???
        ggplot(titanic_data, aes(x = Class, fill = Survived)) +
          geom_bar() +
          labs(y = "Number of Passengers",
               x = "Passenger class",
               title = "Survival Rates vs Class")
      }
      
    }else if( input$feature == colnames(titanic_data)[3] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs Sex - RELETIVE
        ggplot(titanic_data, aes(x= Survived, group=Sex)) + 
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust = -.5) +
          labs(y = "Percent", fill="Survived?",title = "Survival vs Sex") +
          facet_grid(~Sex) +
          scale_y_continuous(labels = scales::percent)+
          guides(fill="none")
        
      }else{
        #Survival rate vs Sex - ABSOLUTE
        ggplot(titanic_data, aes(x = Sex, fill = Survived,label = scales::percent(prop.table(stat(count))))) +
          #geom_bar() +
          geom_bar(position = "dodge") +
          geom_text(stat = 'count',
                    vjust = -0.4, 
                    size = 3) + 
          labs(y = "Number of Passengers",
             title = "Survival Rates vs Sex")
      }
      
    }else if( input$feature == colnames(titanic_data)[4] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs Age - RELETIVE
        
      }else{
        #Survival rate vs Age - ABSOLUTE
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
      }
      
    }else if( input$feature == colnames(titanic_data)[5] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs Number of Siblings/Spouses Aboard - RELETIVE
        ggplot(titanic_data, aes(x= Survived, group=Siblings)) + 
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust = -.5) +
          labs(y = "Percent", fill="Survived?",title = "Survival vs number of siblings/spouses aboard") +
          facet_grid(~Siblings) +
          scale_y_continuous(labels = scales::percent)+
          guides(fill="none")
        
      }else{
        #Survival rate vs Number of Siblings/Spouses Aboard - ABSOLUTE
        ggplot(titanic_data, aes(x = Siblings, fill = Survived)) +
          geom_bar()+
          labs(y = "Number of Passengers",
             x = "Number of Siblings/Spouses Aboard",
             title = "Survival Rates vs Number of Siblings/Spouses Aboard")
      }
    }else if( input$feature == colnames(titanic_data)[6] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs Number of Siblings/Spouses Aboard - RELETIVE
        ggplot(titanic_data, aes(x= Survived, group=Parch)) + 
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust = -.5) +
          labs(y = "Percent",title = "Survival vs number of parents/children aboard") +
          facet_grid(~Parch) +
          scale_y_continuous(labels = scales::percent)+
          guides(fill="none")
      }else{
        #Survival rate vs Number of Parents/Children Aboard  - ABSOLUTE           
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
      }
    }else if( input$feature == colnames(titanic_data)[7] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs Fare - RELETIVE
        
      }else{
        #Survival rate vs Fare - ABSOLUTE
        
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
          xlim(0,100)
      }
    }else if( input$feature == colnames(titanic_data)[8] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs cabin - RELETIVE
        ggplot(titanic_data %>% drop_na(), aes(x= Survived, group=Cabin)) + 
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust = -.5) +
          labs(y = "Percent",title = "Survival rate vs port of embarkation") +
          facet_grid(~Cabin) +
          scale_y_continuous(labels = scales::percent)+
          guides(fill="none")
        
      }else{
        #Survival rate vs cabin - ABSOLUTE
        ggplot(titanic_data %>% drop_na(), aes(x = Cabin, fill = Survived)) +
          geom_bar(position = "dodge") +
          labs(y = "Number of Passengers",
               x = "Cabin",
               title = "Survival vs cabin")
      }
    }else if( input$feature == colnames(titanic_data)[9] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs Embarked - RELETIVE
        ggplot(titanic_data %>% drop_na(), aes(x= Survived, group=Embarked)) + 
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust = -.5) +
          labs(y = "Percent",title = "Survival rate vs port of embarkation") +
          facet_grid(~Embarked) +
          scale_y_continuous(labels = scales::percent)+
          guides(fill="none")
      }else{
        #Survival rate vs Embarked - - ABSOLUTE
        ggplot(titanic_data %>% drop_na(), aes(x = Embarked, fill = Survived,label = scales::percent(prop.table(stat(count))))) +
          geom_bar() +
          geom_text(stat = 'count',
                    vjust = -0.6, 
                    size = 3) + 
          labs(y = "Number of Passengers",
               x = "Passenger class",
               title = "Survival Rates vs Class")
        }
    }
   
  )
  output$sexAgeAndFare <- renderPlot({
    #Survival Rates vs Sex Age and Fare
    ggplot(titanic_data,aes(x=Class,y=Fare,fill= Survived))+
      geom_boxplot()+
      facet_grid(Sex ~ .)+
      ylim(0,180)+
      labs(x = "Passenger class",
           title = "\ vs Sex Age and Fare")
  })
  
  output$density <- renderPlot({
    ggplot(titanic_data, aes(x = Age, fill = Survived)) +
      geom_density(alpha= 0.7) +
      facet_grid(Sex ~ .)+
      labs(y = "Number of Passengers",
           x = "Age",
           title = "Survival Rates vs Age and Sex")
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
