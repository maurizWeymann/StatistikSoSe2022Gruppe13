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
  SibSp,
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
    sliderInput(inputId =  "num_features", label = "Choose number of features", value = 1, min = 1, max = 4, ticks = FALSE),
    selectInput("feature", "Feature 1", choices = colnames(titanic_data)[2:9], selected = colnames(titanic_data)[2]),
    # Display only if can be plotted relative
    conditionalPanel(condition = "input.feature == 'Class' || input.feature == 'Sex' || input.feature == 'Age' || input.feature == 'SibSp' || input.feature == 'Parch' || input.feature == 'Fare' || input.feature == 'Cabin'  || input.feature == 'Embarked' ",
                    #h2(input.feature),
                    conditionalPanel(condition =  "input.feature != 'Age'&&input.feature != 'Fare' ",
                                     radioButtons("relAbs", "relativ oder absolut?", choices = c("relativ","absolut"), selected = "absolut" ),
                    ), conditionalPanel(condition =  "input.feature == 'Age' ",
                                        radioButtons("age_toggle", "Boxplot oder Histogram", choices = c("Boxplot","Histogram"), selected = "Boxplot" ),
                                       ),
                    conditionalPanel(condition =  "age_toggle == 'Histogram' ",
                                                          sliderInput(inputId =  "age_limit", label = "Maximales alter wählen", value = 80, min = 0, max = 80, ticks = FALSE),
                                                          sliderInput(inputId =  "age_binsize", label = "Binsize wählen", value = 10, min = 1, max = 400, ticks = FALSE),),
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
    #plotlyOutput("plot100"),
    plotlyOutput("all"),
    #tableOutput("changingTable"),
    #tableOutput("changingTable"),
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



output$changingTable <- renderPrint(   
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

output$plot100 = renderPlotly(
 ggplotly(ggplot(titanic_data, aes(x= Survived, group=Class)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", fill="Survived?",title = "Survival rate vs class") +
            facet_grid(~Class) +
            scale_y_continuous(labels = scales::percent)+
            guides(fill="none"))
) 

output$plot101 = renderPlotly(
  plot_ly(titanic_data, x = ~Survived, y = ~Age, type = 'bar') %>%
    layout(title = 'A Figure Displaying Itself',
           plot_bgcolor='#e5ecf6', 
           xaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), 
           yaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'))
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
  output$all <- renderPlotly(
    if( input$feature == colnames(titanic_data)[2] ){
      if( input$relAbs ==  "relativ"){
        ggplotly(ggplot(titanic_data, aes(x= Survived, group=Class)) + 
                   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                   geom_text(aes( label = scales::percent(..prop..),
                                  y= ..prop.. ), stat= "count", vjust = -.5) +
                   labs(y = "Percent", fill="Survived?",title = "Survival rate vs class",x = "Passenger class",) +
                   facet_grid(~Class) +
                   scale_y_continuous(labels = scales::percent)+
                   guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
      }else{
        #Survival rate vs class - ABSOLUTE
        ggplotly(ggplot(titanic_data, aes(x = Survived,group = Class,label = scales::percent(prop.table(stat(count))))) +
                   geom_bar(position="dodge",aes(fill=  factor(..x..))) +
                   geom_text(stat = 'count',
                             vjust = -.5) + 
                   facet_grid(~Class) +
                   labs(y = "Number of Passengers",
                        x = "Passenger class",
                        title = "Survival rate vs Class")+
                   guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
      }
      
    }else if( input$feature == colnames(titanic_data)[3] ){
      if( input$relAbs ==  "relativ"){
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
        ggplotly(ggplot(titanic_data, aes(x= Survived, group=SibSp)) + 
                   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                   geom_text(aes( label = scales::percent(..prop..),
                                  y= ..prop.. ), stat= "count", vjust = -.5) +
                   labs(y = "Percent", fill="Survived?",title = "Survival vs number of siblings/spouses aboard",
                        x = "Number of Siblings/Spouses Aboard") +
                   facet_grid(~SibSp) +
                   scale_y_continuous(labels = scales::percent)+
                   guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
        
      }else{
        #Survival rate vs Number of Siblings/Spouses Aboard - ABSOLUTE
        ggplotly(ggplot(titanic_data, aes(x = Survived,group=SibSp,label = scales::percent(prop.table(stat(count))))) +
                   geom_bar(position = "dodge",aes(fill=  factor(..x..))) +
                   geom_text(stat = 'count',
                             vjust = -.5) + 
                   facet_grid(~SibSp) +
                   labs(y = "Number of Passengers",
                        x = "Number of Siblings/Spouses Aboard",
                        title = "Survival vs number of siblings/spouses aboard")+
                   guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
      }
    }else if( input$feature == colnames(titanic_data)[6] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs Number of Parents/Children Aboard - RELETIVE
        ggplotly(ggplot(titanic_data, aes(x= Survived, group=Parch)) + 
                   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                   geom_text(aes( label = scales::percent(..prop..),
                                  y= ..prop.. ), stat= "count", vjust = -.5) +
                   labs(y = "Percent",title = "Survival vs number of parents/children aboard",
                        x = "Number of Parents/Children Aboard") +
                   facet_grid(~Parch) +
                   scale_y_continuous(labels = scales::percent)+
                   guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
      }else{
        #Survival rate vs Number of Parents/Children Aboard  - ABSOLUTE           
        ggplotly(ggplot(titanic_data, aes(x = Survived,group=Parch,label = scales::percent(prop.table(stat(count))))) +
                   geom_bar(position = "dodge",aes(fill=  factor(..x..))) +
                   geom_text(stat = 'count',
                             vjust = -.5) + 
                   facet_grid(~Parch) +
                   labs(y = "Number of Passengers",
                        x = "Number of Parents/Children Aboard",
                        title = "Survival vs number of parents/children aboard")+
                   guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
     
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
        
        ggplotly(ggplot(titanic_data, aes(x = Survived, y = Fare)) +
                   geom_boxplot() +
                   labs(y = "Fare",
                        x = "Survived",
                        title = "Survival vs Fare"),tooltip = "y")
      }
    }else if( input$feature == colnames(titanic_data)[8] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs cabin - RELETIVE
        ggplotly(ggplot(titanic_data, aes(x= Survived, group=Cabin)) + 
                   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                   geom_text(aes( label = scales::percent(..prop..),
                                  y= ..prop.. ), stat= "count", vjust = -.5) +
                   labs(y = "Percent",title = "Survival rate vs port of embarkation",
                        x = "Cabin") +
                   facet_grid(~Cabin) +
                   scale_y_continuous(labels = scales::percent)+
                   guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
        
      }else{
        #Survival rate vs cabin - ABSOLUTE
        ggplotly(ggplot(titanic_data, aes(x = Cabin, fill = Survived)) +
                   geom_bar(position = "dodge") +
                   facet_grid(~Cabin) +
                   labs(y = "Number of Passengers",
                        x = "Cabin",
                        title = "Survival vs cabin"),tooltip = "y")
      }
    }else if( input$feature == colnames(titanic_data)[9] ){
      if( input$relAbs ==  "relativ"){
        #Survival rate vs Embarked - RELETIVE
        ggplotly(ggplot(titanic_data, aes(x= Survived, group=Embarked)) + 
                   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                   geom_text(aes( label = scales::percent(..prop..),
                                  y= ..prop.. ), stat= "count", vjust = -.5) +
                   labs(y = "Percent",title = "Survival rate vs port of embarkation",
                        x = "Port of embarkation") +
                   facet_grid(~Embarked) +
                   scale_y_continuous(labels = scales::percent)+
                   guides(fill=guide_legend("1=Died\n2=Survived")),tooltip = "y")
      }else{
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
