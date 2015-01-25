library(shiny)

# Define UI for airquality data application
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Predicting New-York Air Quality Parameters."),
    
    sidebarPanel(
        selectInput("outcome", "Select an Air-Quality parameter to Predict:", list("Ozone" = "Ozone", 
                    "Solar Radiation" = "Solar.R", "Wind" = "Wind", "Temperature" = "Temp")),
        uiOutput("predictor"),
        uiOutput("slider") 
     ),
    
    mainPanel(
    #Draw a tab panel to show User instructions and Predicted values        
        tabsetPanel(type = "tabs",                     
                    tabPanel("Instructions",p(textOutput("text1")), p(textOutput("text2")),p(textOutput("text3")),
                             p(textOutput("text4")),p(textOutput("text5"))), 
                    tabPanel("Predictions",h4(textOutput("guess")), plotOutput("ioPlot")))
                    
        
        
    )
))