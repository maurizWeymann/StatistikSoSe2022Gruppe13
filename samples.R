######################
##### UI Elements ####
######################

#   ---INPUT---

#               basic INPUT syntax:
# *Input(inputId = "xyz123", label = "Beschriftung", ...)

# checkboxInput()
# checkboxGroupInput()
# 
# selectInput()
# numericInput()
#
# radioButtons()
# 
# sliderInput(inputId = "xyz123", label = "Beschriftung", value = XY, min = 1, max = 100)
# 
# actionButton()
# 
# fileInput()
# 
# textInput()
# passwordInput()
# dateInput()
# dateRangeInput()
# 

#   ---OUPUT---
#               basic OUTPUT syntax:
# *Output("xyz123")

# plotOutput()
# 
# tableOutput() - table
# dataTableOutput() - interactive Table
# 
# textOutput()
# imageOutput()
# htmlOutput()
# 
# uiOutput() - a Shiny UI element
# verbatimTextOutput()

########################
##### BASE Elements ####
########################

# ui <- dashboardPage()
# server <- function(input, output, session) {}
    #   input$xyz
    #   output$xyz <- renderPlot({ })
    
# shinyApp(ui, server)

###########################
##### render Functions ####
###########################

# use in server part with *Output$renderFunction()
# 
# renderDataTable()
# renderImage()
# renderPlot()
# renderPrint()
# renderTable()
# renderText()
# renderUI()
# render

###########################

# sting concatination mit paste(a,b)
















