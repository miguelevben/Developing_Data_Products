library(shiny)

# Define UI for airquality data application
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Air Quality Relationships & Predictions"),
    
    sidebarPanel(
        selectInput("outcome", "Outcome:", list("Ozone" = "Ozone", 
                                                "Solar Radiation" = "Solar.R",
                                                "Wind" = "Wind",
                                                "Temperature" = "Temp")),
        uiOutput("predictor"),
        uiOutput("slider"),
        h5(textOutput("guess"))
    ),
    
    mainPanel(
        plotOutput("ioPlot"),
        textOutput("docs")
    )
))