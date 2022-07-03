library(DT)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(highcharter)
library(ggplot2)
library(plotly)

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
  SibSp,
  Parch,
  Fare = round(Fare,2),
  Cabin = substr( gsub("[^a-zA-Z]", "", Cabin), 1, 1),
  Embarked = factor(Embarked,
                    levels = c("C","Q","S"),
                    labels = c("Cherbourg","Queenstown","Southampton"))
)
titanic_data[titanic_data == ""] <- NA  

ui <- dashboardPage(
  
  dashboardHeader(title = "Titanic Dashboard", titleWidth = 250),
  
  dashboardSidebar( width = 250,
                    #h3("Wähle deine Variablen"),
                    sliderInput(inputId =  "num_features", label = "Choose number of features", value = 0, min = 0, max = 4, ticks = FALSE),
                    conditionalPanel(condition =  "input.num_features == '2' ",
                                                selectInput( "feature_2", "Survived vs", choices = c("cabin and class","class and sex","class and age","age and sex"), selected = c("cabin and age")),
                                     conditionalPanel(condition =  "input.feature_2=='cabin and class' ||input.feature_2=='class and sex'",
                                                      radioButtons("relAbs2", "relativ vs absolut?", choices = c("relativ","absolut"), selected = "absolut" ),
                                                      ),

                    ),
                    conditionalPanel(condition =  "input.num_features == '3' ",
                                     selectInput( "feature_3", "Survived vs", choices = c("sex and fare and class","sex and age and class"), selected = c("sex and fare and class")),                                     
                    ),
                    conditionalPanel(condition =  "input.num_features == '4' ",
                                     selectInput("feature_4", "Survival vs ", choices = c("Age vs Sex vs Embarked vs Class","Sex vs Fare vs Class vs Embarked"), selected = c("Age vs Sex vs Embarked vs Class")),
                                     
                    ),
                    conditionalPanel(condition =  "input.num_features == '0' ",
                                     selectInput("feature_test", "Select Feature to Plot", choices = colnames(titanic_data)[1:9], selected = colnames(titanic_data)[1]),
                                     
                                     ),
                    conditionalPanel(condition =  "input.num_features == '1' ",
                                     selectInput("feature", "Survived vs", choices = colnames(titanic_data)[2:9], selected = colnames(titanic_data)[2]),
                                    
                                     conditionalPanel(condition =  "input.feature != 'Age' && input.feature != 'Fare' ",
                                                      radioButtons("relAbs", "relativ vs absolut?", choices = c("relativ","absolut"), selected = "absolut" ),
                                                      ),
                                     conditionalPanel(condition =  "input.feature == 'Age' ",
                                                      radioButtons("age_toggle", "Boxplot vs Histogram", choices = c("Boxplot","Histogram"), selected = "Boxplot" ),
                                                      sliderInput(inputId =  "age_limit", label = "Choose the maximum Age", value = 80, min = 0, max = 80, ticks = FALSE),
                                                      
                                                      conditionalPanel(condition =  "input.age_toggle == 'Histogram'&& input.feature == 'Age' ",
                                                                       sliderInput(inputId =  "age_binsize", label = "Choose size of bins", value = 10, min = 1, max = 40, ticks = FALSE),
                                                                       radioButtons("Age_relAbs", "Relativ or absolut?", choices = c("relativ","absolut"), selected = "absolut" ),
                                                                       ),
                                                      ),
                                     conditionalPanel(condition =  "input.feature == 'Fare' ",
                                                      radioButtons("Fare_toggle", "Boxplot oder Histogram", choices = c("Boxplot","Histogram"), selected = "Boxplot" ),
                                                      sliderInput(inputId =  "fare_limit", label = "Choose the maximum fare", value = 180, min = 0, max = 513, ticks = FALSE),
                                                      
                                                      conditionalPanel(condition =  "input.Fare_toggle == 'Histogram'&&",
                                                                       sliderInput(inputId =  "fare_binsize", label = "Choose size of bins", value = 10, min = 1, max = 200, ticks = FALSE),
                                                                       radioButtons("Fare_relAbs", "Relativ or absolut?", choices = c("relativ","absolut"), selected = "absolut" ),
                                                                       ),
                                                     
                                                       ),
                                     )
                  
    
  ),
  dashboardBody(
    uiOutput("plots"),
    htmlOutput("cramer"),
    conditionalPanel(condition =  "input.num_features == '0' ",
                     plotlyOutput("test"),
    ),
    conditionalPanel(condition =  "input.num_features == '1' ",
                     
                     plotlyOutput("all")
                    
                     ),

    
    #tableOutput("changingTable"),
    #plotlyOutput("flexPlot"),
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
  
  
  output$plots <- renderUI(
    
    
    if (input$num_features==0) {
      output$cramer<- renderUI({ h2("")  })
      renderPlotly(  plot_ly(data = titanic_data,x = titanic_data[[input$feature_test]]) )
      
    }
    else if (input$num_features==1) {
        
      
      if( input$feature == colnames(titanic_data)[2] ){
        if( input$relAbs ==  "relativ"){
          output$cramer<- renderUI({ h2("Quelle: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/")  })
          
          ggplotly(ggplot(titanic_data, aes(x= Survived, group=Class)) + 
                     geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                     geom_text(aes( label = scales::percent(..prop..),
                                    y= ..prop.. ), stat= "count", vjust = -.5) +
                     labs(y = "Percent", fill="Survived?",title = "Survival vs Class",x = "Passenger class",) +
                     facet_grid(~Class) +
                     scale_y_continuous(labels = scales::percent)+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
          
        }else{
          output$cramer<- renderUI({ h2("SDasdsaSD")  })
          #Survival rate vs class - ABSOLUTE
          ggplotly(ggplot(titanic_data, aes(x = Survived,group = Class,label = scales::percent(prop.table(stat(count))))) +
                     geom_bar(position="dodge",aes(fill=  factor(..x..))) +
                     geom_text(stat = 'count',
                               vjust = -.5) + 
                     facet_grid(~Class) +
                     labs(y = "Number of Passengers",
                          x = "Passenger class",
                          title = "Survival vs Class")+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
        }
        
      }else if( input$feature == colnames(titanic_data)[3] ){
        if( input$relAbs ==  "relativ"){
          output$cramer<- renderUI({ h2("Quelle: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/")  })
          #Survival rate vs Sex - RELETIVE
          ggplotly(ggplot(titanic_data, aes(x= Survived, group=Sex)) + 
                     geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                     geom_text(aes( label = scales::percent(..prop..),
                                    y= ..prop.. ), stat= "count", vjust = -.5) +
                     labs(y = "Percent", fill="Survived?",title = "Survival vs Sex",x="Sex") +
                     facet_grid(~Sex) +
                     scale_y_continuous(labels = scales::percent)+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
          
        }else{
          output$cramer<- renderUI({ h2(" ")  })
          #Survival rate vs Sex - ABSOLUTE
          ggplotly(ggplot(titanic_data, aes(x = Survived,group=Sex,label = scales::percent(prop.table(stat(count))))) +
                     geom_bar(position = "dodge",aes(fill=  factor(..x..))) +
                     geom_text(stat = 'count',
                               vjust = -.5) + 
                     facet_grid(~Sex) +
                     labs(y = "Number of Passengers",
                          title = "Survival vs Sex",
                          x="Sex")+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
          
        }
        
      }else if( input$feature == colnames(titanic_data)[4] ){
        
        if( input$age_toggle ==  "Histogram"){
          if( input$Age_relAbs ==  "relativ"){
            output$cramer<- renderUI({ h2("")  })
            #Survival rate vs Age - Historgramm RELETIVE
            ggplotly(ggplot(titanic_data, aes(x = Age, fill = Survived)) +
                       geom_histogram(binwidth = input$age_binsize,aes(y = after_stat(count / sum(count)))) +
                       labs(y = "Number of Passengers",
                            title = "Survival vs Age")+
                       scale_y_continuous(labels = scales::percent)+
                       xlim(0,input$age_limit),tooltip = "y")
          }
          else{
            output$cramer<- renderUI({ h2(" ")  })
            #Survival rate vs Age - Historgramm ABSOLUTE
            ggplotly(ggplot(titanic_data, aes(x = Age, fill = Survived)) +
                       geom_histogram(binwidth = input$age_binsize) +
                       labs(y = "Number of Passengers",
                            title = "Survival vs Age")+
                       xlim(0,input$age_limit),tooltip = "y")
          }
          
        }else{
          
          output$cramer<- renderUI({ h2(" ")  })
          #Survival rate vs Age - ABSOLUTE
          ggplot(titanic_data, aes(x = Age, fill = Survived)) +
            geom_density(alpha= 0.7) +
            labs(y = "Number of Passengers",
                 x = "Age",
                 title = "Survival vs Age")
          ggplotly(ggplot(titanic_data, aes(x = Survived, y = Age)) +
                     geom_boxplot(aes(fill= Survived)) +
                     labs(y = "Age",
                          x = "Survived",
                          title = "Survival vs Age")+
                     ylim(0,input$age_limit))
        }
        
      }else if( input$feature == colnames(titanic_data)[5] ){
        if( input$relAbs ==  "relativ"){
          output$cramer<- renderUI({ h2("Quelle: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/")  })
          #Survival rate vs Number of Siblings/Spouses Aboard - RELETIVE
          ggplotly(ggplot(titanic_data, aes(x= Survived, group=SibSp)) + 
                     geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                     geom_text(aes( label = scales::percent(..prop..),
                                    y= ..prop.. ), stat= "count", vjust = -.5) +
                     labs(y = "Percent", fill="Survived?",title = "Survival vs number of siblings/spouses aboard",
                          x = "Number of siblings/spouses aboard") +
                     facet_grid(~SibSp) +
                     scale_y_continuous(labels = scales::percent)+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
          
          
        }else{
          output$cramer<- renderUI({ h2(" ")  })
          #Survival rate vs Number of Siblings/Spouses Aboard - ABSOLUTE
          ggplotly(ggplot(titanic_data, aes(x = Survived,group=SibSp,label = scales::percent(prop.table(stat(count))))) +
                     geom_bar(position = "dodge",aes(fill=  factor(..x..))) +
                     geom_text(stat = 'count',
                               vjust = -.5) + 
                     facet_grid(~SibSp) +
                     labs(y = "Number of Passengers",
                          x = "Number of siblings/spouses aboard",
                          title = "Survival vs number of siblings/spouses aboard")+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
        }
      }else if( input$feature == colnames(titanic_data)[6] ){
        if( input$relAbs ==  "relativ"){
          output$cramer<- renderUI({ h2("Quelle: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/")  })
          #Survival rate vs Number of Parents/Children Aboard - RELETIVE
          ggplotly(ggplot(titanic_data, aes(x= Survived, group=Parch)) + 
                     geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                     geom_text(aes( label = scales::percent(..prop..),
                                    y= ..prop.. ), stat= "count", vjust = -.5) +
                     labs(y = "Percent",title = "Survival vs number of parents/children aboard",
                          x = "Number of parents/children aboard") +
                     facet_grid(~Parch) +
                     scale_y_continuous(labels = scales::percent)+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
        }else{
          output$cramer<- renderUI({ h2(" ")  })
          #Survival rate vs Number of Parents/Children Aboard  - ABSOLUTE           
          ggplotly(ggplot(titanic_data, aes(x = Survived,group=Parch,label = scales::percent(prop.table(stat(count))))) +
                     geom_bar(position = "dodge",aes(fill=  factor(..x..))) +
                     geom_text(stat = 'count',
                               vjust = -.5) + 
                     facet_grid(~Parch) +
                     labs(y = "Number of Passengers",
                          x = "Number of parents/children aboard",
                          title = "Survival vs number of parents/children aboard")+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
          
        }
      }else if( input$feature == colnames(titanic_data)[7] ){      
        if( input$Fare_toggle ==  "Histogram"){
          if( input$Fare_relAbs ==  "relativ"){
            output$cramer<- renderUI({ h2(" ")  })
            #Survival rate vs Fare - Historgramm RELETIVE
            ggplotly(ggplot(titanic_data, aes(x = Fare, fill = Survived)) +
                       geom_histogram(binwidth = input$fare_binsize,aes(y = after_stat(count / sum(count)))) +
                       labs(y = "Percent",
                            title = "Survival vs Fare")+
                       scale_y_continuous(labels = scales::percent)+
                       xlim(0,input$fare_limit),tooltip = "y")
          }
          else{
            output$cramer<- renderUI({ h2(" ")  })
            #Survival rate vs Fare - Historgramm ABSOLUTE
            ggplotly(ggplot(titanic_data, aes(x = Fare, fill = Survived)) +
                       geom_histogram(binwidth = input$age_binsize) +
                       labs(y = "Number of Passengers",
                            title = "Survival vs Fare")+
                       xlim(0,input$fare_limit),tooltip = "y")
          }
          
        }else{
          output$cramer<- renderUI({ h2("")  })
          #Survival rate vs Fare - ABSOLUTE
          ggplotly(ggplot(titanic_data, aes(x = Survived, y = Fare)) +
                     geom_boxplot(aes(fill= Survived)) +
                     labs(y = "Fare",
                          x = "",
                          title = "Survival vs Fare")+
                     ylim(0,input$fare_limit))
        }
      }else if( input$feature == colnames(titanic_data)[8] ){
        if( input$relAbs ==  "relativ"){
          output$cramer<- renderUI({ h2("Quelle: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/")  })
          #Survival rate vs cabin - RELETIVE
          ggplotly(ggplot(titanic_data, aes(x= Survived, group=Cabin)) + 
                     geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                     geom_text(aes( label = scales::percent(..prop..),
                                    y= ..prop.. ), stat= "count", vjust = -.5) +
                     labs(y = "Percent",title = "Survival vs Cabin",
                          x = "Cabin") +
                     facet_grid(~Cabin) +
                     scale_y_continuous(labels = scales::percent)+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
          
        }else{
          output$cramer<- renderUI({ h2(" ")  })
          #Survival rate vs cabin - ABSOLUTE
          ggplotly(ggplot(titanic_data, aes(x = Cabin, fill = Survived)) +
                     geom_bar(position = "dodge") +
                     labs(y = "Number of Passengers",
                          x = "Cabin",
                          title = "Survival vs Cabin"),tooltip = "y")
        }
      }else if( input$feature == colnames(titanic_data)[9] ){
        if( input$relAbs ==  "relativ"){
          output$cramer<- renderUI({ h2("Quelle: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/")  })
          #Survival rate vs Embarked - RELETIVE
          ggplotly(ggplot(titanic_data, aes(x= Survived, group=Embarked)) + 
                     geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                     geom_text(aes( label = scales::percent(..prop..),
                                    y= ..prop.. ), stat= "count", vjust = -.5) +
                     labs(y = "Percent",title = "Survival vs port of embarkation",
                          x = "Port of embarkation") +
                     facet_grid(~Embarked) +
                     scale_y_continuous(labels = scales::percent)+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
        }else{
          output$cramer<- renderUI({ h2(" ")  })
          #Survival rate vs Embarked - - ABSOLUTE
          ggplotly(ggplot(titanic_data, aes(x = Survived,group=Embarked,label = scales::percent(prop.table(stat(count))))) +
                     geom_bar(position = "dodge",aes(fill=  factor(..x..))) +
                     geom_text(stat = 'count',
                               vjust = -.5) + 
                     facet_grid(~Embarked) +
                     labs(y = "Number of Passengers",
                          x = "Port of embarkation",
                          title = "Survival vs port of embarkation")+
                     guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
        }
      }
    }
    else if (input$num_features==2) {
      if (input$feature_2=="cabin and class") {
       if( input$relAbs2 ==  "relativ"){
         output$cramer<- renderUI({ h2("Online haben wir herausgefunden das fast nur Daten zur Cabin aus der ersten Klasse bekannt sind weil sie am Körper eines Stewards gefunden wurden sind. Das erklärt auch die höhere Überlebensrate der Leute mit Cabin. ")  })
          ggplotly(ggplot(titanic_data, aes(x= Survived, group=Cabin)) + 
                     geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                     geom_text(aes( label = scales::percent(..prop..),
                                    y= ..prop.. ), stat= "count", vjust = -.5) +
                     labs(y = "Percent",title = "Survival vs Class vs Cabin",
                          x = "") +
                     facet_grid(Class~Cabin) +
                     scale_y_continuous(labels = scales::percent)+
                     guides(fill=guide_legend("red=Died\nblue=Survived")),tooltip = "y")
        }else{
          output$cramer<- renderUI({ h2("Online haben wir herausgefunden das fast nur Daten zur Cabin aus der ersten Klasse bekannt sind weil sie am Körper eines Stewards gefunden wurden sind. Das erklärt auch die höhere Überlebensrate der Leute mit Cabin. ")  })
          ggplotly(ggplot(titanic_data, aes(x = Cabin, fill = Survived)) +
                     geom_bar(position = "dodge") +
                     facet_grid(~Class) +
                     labs(y = "Number of Passengers",
                          x = "Cabin",
                          title = "Survival vs Cabin vs Class"),tooltip = "y")
        }

      }else if (input$feature_2=="age and sex") {
        output$cramer<- renderUI({ h2(" ")  })
        ggplotly(ggplot(titanic_data, aes(x = Age, fill = Survived)) +
                   geom_density(alpha= 0.7) +
                   facet_grid(Sex ~ .)+
                   labs(y = "Density",
                        x = "Age",
                        title = "Survival vs Age vs Sex"),tooltip = "y")
      }else if (input$feature_2=="class and sex") {
        if( input$relAbs2 ==  "relativ"){
          output$cramer<- renderUI({ h2(" ")  })
          ggplotly(ggplot(titanic_data, aes(x= Survived, group=Class)) + 
                     geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                     geom_text(aes( label = scales::percent(..prop..),
                                    y= ..prop.. ), stat= "count", vjust = -.5) +
                     labs(y = "Percent",title = "Survival vs Class vs Sex",
                          x = "class") +
                     facet_grid(Sex~Class) +
                     scale_y_continuous(labels = scales::percent)+
                     guides(fill=guide_legend("red=Died\nblue=Survived")),tooltip = "y")
          
        }else{
          output$cramer<- renderUI({ h2(" ")  })
          ggplotly(ggplot(titanic_data, aes(x = Class, fill = Survived)) +
                     geom_bar(position = "dodge") +
                     facet_grid(Sex ~ .)+
                     labs(y = "Number of passengers",
                          x = "Class",
                          title = "Survival vs Class vs Sex"),tooltip = "y")
        }
        
      }else if (input$feature_2=="class and age") {
        output$cramer<- renderUI({ h2(" ")  })
        ggplotly(ggplot(titanic_data, aes(x = Age, fill = Survived)) +
                   geom_density(alpha= 0.7) +
                   facet_grid(Class ~ .)+
                   labs(y = "Density",
                        x = "Age",
                        title = "Survival vs Age vs Class"),tooltip = "y")
      }
    }
    else if (input$num_features==3) {
      if (input$feature_3=="sex and fare and class") {
        output$cramer<- renderUI({ h2(" ")  })
        ggplotly(ggplot(titanic_data,aes(x=Class,y=Fare,fill= Survived))+
                   geom_boxplot()+
                   facet_grid(Sex ~ Survived)+
                   ylim(0,180)+
                   labs(x = "Class",
                        title = "Survival vs Sex vs Fare vs Class"),tooltip = "y")
      }else if (input$feature_3=="sex and age and class") {
        output$cramer<- renderUI({ h2(" ")  })
        ggplotly(ggplot(titanic_data,aes(x=Class,y=Age,fill= Survived))+
                   geom_boxplot()+
                   facet_grid(Sex ~ Survived)+
                   ylim(0,80)+
                   labs(x = "Class",
                        title = "Survival vs Sex vs Age vs Class"),tooltip = "y")
      }
    }
    else if (input$num_features==4) {
      if (input$feature_4=="Age vs Sex vs Embarked vs Class") {
        output$cramer<- renderUI({ h2(" ")  })
        renderPlot( ggplot(titanic_data,aes(x=Embarked,y=Age,fill= Survived))+
                    geom_boxplot()+
                    facet_grid(Sex ~ Class)+
                    ylim(0,80)+
                    labs(x = "",
                         title = "Survival vs Age vs Sex vs Embarked vs Class") )
      }else if (input$feature_4=="Sex vs Fare vs Class vs Embarked") {
        output$cramer<- renderUI({ h2(" ")  })
        renderPlot( ggplot(titanic_data,aes(x=Embarked,y=Fare,fill= Survived))+
                    geom_boxplot()+
                    facet_grid(Sex ~ Class)+
                    ylim(0,180)+
                    labs(x = "",
                         title = "Survival vs Sex vs Fare vs Class vs Embarked") )
      }
    }
    
   
    
  )  
  
  output$changingTable <- renderTable(   
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
  


  
}

shinyApp(ui, server)

## sample code von https://github.com/Data-Mastery/RShiny_Iris/blob/main/app.R ##
