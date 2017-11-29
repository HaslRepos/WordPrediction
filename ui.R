#suppressPackageStartupMessages(c(library(shiny),library(shinythemes),library(stringr),library(tm)))

shinyUI(fluidPage(

     titlePanel("Word Prediction"),
  
     sidebarLayout(
          sidebarPanel(
               helpText("Enter your text in the language selected."),
               textInput("text", "Text:"),

               # Read result range (Possible values 1, 3 and 5)
               helpText("Define the required number of results by moving the slider."),
               sliderInput("resultRange", "Number of results:", min=1, max=5, value=1, step=2),

               helpText("Select the language."),
               radioButtons('lang', "Language:", choices=c("English" = "EN","German" = "DE"), 
                    selected = "EN"),
               
               checkboxGroupInput("addInfo", "Additional Information:", c("Frequency" = "prop"), selected = NULL)
          ),
    
          mainPanel(
               helpText("The Word Prediction app provides predictions of the next possible word 
                    the user might enter, based on the last one, two or three words already 
                    entered. The app supports englisch and german input. It is configurable by 
                    the text input, a range of words to be provided (1, 3 or 5) and the language.
                    Additional information (frequency of the predicted word) can be provided."),
      
               br(),

               tabsetPanel(type = "tabs", tabPanel("Prediction",  tableOutput("prediction")))
          )
     )
))