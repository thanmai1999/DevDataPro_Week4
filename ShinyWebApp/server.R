library(shiny)
library(datasets)

MPGdatadata <- mtcars
MPGdatadata$am <- factor(MPGdatadata$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
    
    FormText <- reactive({
        paste("mpg ~", input$variable)
    })
    
    FormTextPoint <- reactive({
        paste("mpg ~", "as.integer(", input$variable, ")")
    })
    
   FITTER <- reactive({
        lm(as.formula(FormTextPoint()), data=MPGdatadata)
    })
    
    output$caption <- renderText({
        FormText()
    })
    
    output$mpgBoxPlot <- renderPlot({
        boxplot(as.formula(FormText()), 
                data = MPGdatadata,
                outline = input$outliers)
    })
    
    output$fit <- renderPrint({
        summary(fit())
    })
    
    output$mpgPlot <- renderPlot({
        with(MPGdatadata, {
            plot(as.formula(FormTextPoint()))
            abline(fit(), col=2)
        })
    })
    
})