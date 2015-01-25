# Load libraries: shiny and datasets for internal AirPolution dataset.
library(shiny)
library(datasets)

#Load dataset
DF_data <- airquality

# Dropdowns -> list of columns  
Drop_columns <- list("Ozone" = "Ozone", "Solar Radiation" = "Solar.R", "Wind" = "Wind", "Temperature" = "Temp")

# shiny server
shinyServer(function(input, output, session) {
    output$predictor = renderUI({
        outc <- input$outcome
        selectInput("predict", "Predictor:", Drop_columns[-grep(outc,Drop_columns)], selected="Solar.R")
    })
    
    #setup slider: default to mean, set max and min to the observed max and min
    output$slider = renderUI({
        #set default for input$predict for initial load
        if (is.null(input$predict)){
            ipd <- "Solar.R"
        }
        else
        {
            ipd <- input$predict     
        }
        
        slLabel = paste("Adjust to predict ", names(Drop_columns[grep(input$outcome,Drop_columns)]), "using this value for ", names(Drop_columns[grep(ipd,Drop_columns)]), ":")
        slVal = round(mean(DF_data[,ipd], na.rm=TRUE),0)
        slMin = round(min(DF_data[,ipd], na.rm=TRUE),0)
        slMax = round(max(DF_data[,ipd], na.rm=TRUE),0)
        sliderInput('slide', slLabel, value = slVal, min = slMin, max = slMax, step=1,)
    })
    
    #create formula for plot
    formulaText <- reactive({
        #set default for input$predict for initial load
        if (is.null(input$predict)){
            ipd <- "Solar.R"
        }
        else
        {
            ipd <- input$predict     
        }
        paste(input$outcome, " ~ ", ipd)
    })
    
    #create text for prediction
    output$guess <- renderText({
        fit <- lm(as.formula(formulaText()), data=DF_data)
        slp <- fit$coeff[2]
        yint <- fit$coeff[1]
        predout <- round(yint + slp * input$slide,2)
        paste("Predicted outcome for ", names(Drop_columns[grep(input$outcome,Drop_columns)]), ": ", predout)
    })
    
    #create plot and fitted line
    output$ioPlot <- renderPlot({
        #set default for input$predict for initial load
        if (is.null(input$predict)){
            ipd <- "Solar.R"
        }
        else
        {
            ipd <- input$predict     
        }
        
        fit <- lm(as.formula(formulaText()), data=DF_data)
        plot(as.formula(formulaText()), 
             data = DF_data, xlab=names(Drop_columns[grep(ipd,Drop_columns)]), ylab=names(Drop_columns[grep(input$outcome,Drop_columns)]))
        abline(fit, col="red")
        title(main= paste(names(Drop_columns[grep(input$outcome,Drop_columns)]), " vs. ", names(Drop_columns[grep(ipd,Drop_columns)])))
    })
    
    #create documentation
    output$docs <- renderText({
        "This is a Shiny application used for analyzing the airquality dataset in the R datasets package.
        To use this app you must first select one of the four measurements from the dataset as an outcome.
        Next you will choose a predictor, also from the four measurements.  The app will automatically remove
        the measurement you have selected as an outcome so that you do not compare a measurement against itself.
        After both an outcome and predictor are selected a plot will be generated comparing the two measurements
        and a regression line will be drawn.  This plot will update automatically as you switch between predictors
        and outcomes.  Finally we will use the equation from our fitted line to allow you to make predictions of what
        your outcome should be for any particular predictor value.  This is done by using the slider at the bottom.
        The slider defaults to the mean value for a given predictor and its maximum and minimum values are the
        maximum and minimum observed values in the dataset.  For any predictor value selected using the slider
        the predicted outcome returned is that y value for that selected value on our fitted line."
        })
        
        
})