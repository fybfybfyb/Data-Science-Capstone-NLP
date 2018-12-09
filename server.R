library(shiny)
library(shinythemes)

print("Loading model2.R...")
print(getwd())
source("model2.R")
print("...done.")

shinyServer(function(input, output) {
        prediction <- reactive({
                text <- cleanInput(input$textInput)
                pred <- predictWords(text, isDebugMode = FALSE)
        })
                output$text1 <- renderText({
                        prediction()
                })
                # output$text2 <- renderText({
                #         prediction()[2]
                # })
                # output$text3 <- renderText({
                #         prediction()[3]
                # })
                # output$text4 <- renderText({
                #         prediction()[4]
                # })
                # output$text5 <- renderText({
                #         prediction()[5]
                # })
                # output$text6 <- renderText({
                #         prediction()[6]
                # })
})
