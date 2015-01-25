# Load libraries: shiny and datasets for internal AirPolution dataset.
library(shiny)
library(datasets)

#Load dataset
DF_data <- airquality

# Dropdowns -> list of columns  
Drop_columns <- list("Ozone" = "Ozone", "Solar Radiation" = "Solar.R", "Temperature" = "Temp","Wind" = "Wind")

# shiny server main function
shinyServer(function(input, output, session) {
    output$predictor = renderUI({
        outc <- input$outcome
        selectInput("predict", "Air-Quality Predictor:", Drop_columns[-grep(outc,Drop_columns)], selected="Solar.R")
    })
    
    #slider control to select a predictor value: default to mean, set max and min to the observed max and min
    output$slider = renderUI({
        
        #input_data variable, set default  
        if (is.null(input$predict)){
            input_predictor <- "Solar.R"
        }
        else
        {
            input_predictor <- input$predict     
        }
        
        slider_Label= paste("Select a value for ", names(Drop_columns[grep(input_predictor,Drop_columns)]), ":")
        slider_meanValue = round(mean(DF_data[,input_predictor], na.rm=TRUE),0)
        slider_minValue = round(min(DF_data[,input_predictor], na.rm=TRUE),0)
        slider_maxValue = round(max(DF_data[,input_predictor], na.rm=TRUE),0)
        sliderInput('slide', slider_Label, value = slider_meanValue, min = slider_minValue, max = slider_maxValue, 
                    step=1)
    })
    
    #create dinamically a lm (regression) formula from selected measurement (predictor and outcome)
    formulaText <- reactive({
        #set default for input$predict for initial load
        if (is.null(input$predict)){
            input_predictor <- "Solar.R"
        }
        else
        {
            input_predictor <- input$predict     
        }
        paste(input$outcome, " ~ ", input_predictor)
    })
    
    #create text for prediction
    output$guess <- renderText({
        fit <- lm(as.formula(formulaText()), data=DF_data)
        slp <- fit$coeff[2]
        yint <- fit$coeff[1]
        predout <- round(yint + slp * input$slide,2)
        paste("Value predicted for ", names(Drop_columns[grep(input$outcome,Drop_columns)]), ": ", predout)
    })
    
    #create plot and fitted line
    output$ioPlot <- renderPlot({
        #set default for input$predict for initial load
        if (is.null(input$predict)){
            input_predictor <- "Solar.R"
        }
        else
        {
            input_predictor <- input$predict     
        }
        
        fit <- lm(as.formula(formulaText()), data=DF_data)
        plot(as.formula(formulaText()), 
             data = DF_data, xlab=names(Drop_columns[grep(input_predictor,Drop_columns)]), ylab=names(Drop_columns[grep(input$outcome,Drop_columns)]))
        abline(fit, col="red")
        title(main= paste(names(Drop_columns[grep(input$outcome,Drop_columns)]), " vs. ", names(Drop_columns[grep(input_predictor,Drop_columns)])))
    })
    
    #create documentation
    output$text1 <- renderText({
        "With this application you can predict parameters from airquality dataset (included in R dataset package)."
        })
    output$text2 <- renderText({
        "- Select an Air-Quality measurement to make predictions about his value."  
        
    })   
    output$text3 <- renderText({
        "- Select an Air-Quality Predictor."  
        
    })   
    output$text4 <- renderText({
        " - Select a value for the Air-Quality Predictor from the slider. The max., min. and mean values showed 
        will be obtained from dataset values."  
        
    })   
    output$text5 <- renderText({
        "   
        The outcome predicted will be showed at right-panel into Predictions tab. The app will show a plot          
        drawing a regression line comparing the two measurements (air-quality measurement respect air-quality 
        predictor. This plot will be updated automatically when you switch between both air-quality measurements.
        When the user select a predictor value from slider ( x axis from plot),  the predicted value (y axis)   
        will be obtained from fitted line (regression line)."  
        
    })   
        
})