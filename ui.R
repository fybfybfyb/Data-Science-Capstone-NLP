library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
       theme = shinytheme("cerulean"),
        titlePanel("Please type some words to predict next words"),
        fluidRow(
                column(
                        width = 10,
                        h4("by Yibo Fan", style="color:orange"),
                        p("This application is mainly for predicting next words given several words"),
                        p("We use N-gram method combined with data table to do searching in our lookup table")
                )
                ),
        sidebarLayout(
                sidebarPanel(
                        h5("BACKGROUND: "),
                        p("We do all of the work in four steps:"),
                        p("First, we read the text and sample 5% of them into our dataSample"),
                        p("Second, we use quanteda to do tokens and use dfm to create n-grams(5--2 grams)"),
                        p("Third, considering the high efficiency of data table, we make 4 data tables from the n-grams above with 2 columns: word,freq(frequence).
                          Then we split the word in the table into single words"),
                        p("Finally, we consider the length of the given words and use the data tables to search next words based to the last few words")
                ),
                mainPanel(
                        h5("Notice:"),
                        p("Type the words in the bar.
                          Then click the predict button to generate the result"),
                        textInput("textInput", label = h3("Enter a sentence:", value = "", style="font-size:44px;"),  width = "50%"),
                        submitButton("PREDICT"),
                        h3(textOutput("text1"),style="color:red")
                        )
                )  
        )
)
