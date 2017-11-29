suppressPackageStartupMessages(c(library(shiny),library(shinythemes),library(stringr),library(tm)))

# Load English dictionary
dict2EN <- readRDS(file="./dictionary/dictionary2EN.RDS")
dict3EN <- readRDS(file="./dictionary/dictionary3EN.RDS")
dict4EN <- readRDS(file="./dictionary/dictionary4EN.RDS")
# Load German dictionary
dict2DE <- readRDS(file="./dictionary/dictionary2DE.RDS")
dict3DE <- readRDS(file="./dictionary/dictionary3DE.RDS")
dict4DE <- readRDS(file="./dictionary/dictionary4DE.RDS")


shinyServer(function(input, output) {
     output$prediction <- renderTable(predictNextWord(input$text, input$resultRange, input$lang, input$addInfo), colnames = FALSE, bordered = TRUE)
})


# Function to clean string entered by user
cleanString <- function(text){
     cleanString <- tolower(text)
     cleanString <- removeNumbers(cleanString)
     cleanString <- removePunctuation(cleanString)
     cleanString <- stripWhitespace(cleanString)
     cleanString <- unlist(strsplit(cleanString, " "))

     return(cleanString)
}


predictNextWord <- function(text, resultRange, lang, addInfo){
     cleanText <- cleanString(text)
     len <- length(cleanText)  # Number of words

     if (lang == "DE") {
          dict2 <- dict2DE  # German Bigrams
          dict3 <- dict3DE  # German Trigrams
          dict4 <- dict4DE  # German Quadgrams
     } else {
          dict2 <- dict2EN  # English Bigrams
          dict3 <- dict3EN  # English Trigrams
          dict4 <- dict4EN  # English Quadgrams
     }

     searchString3 <- NA
     searchString2 <- NA
     searchString1 <- NA
     prediction <- NA
  
     # Initialize search strings based on number of words entered
     if (len >= 3) {
          # Last three words entered
          searchString3 <- paste(cleanText[(length(cleanText)-2):length(cleanText)], collapse=" ")
     } else if (len == 2) {
          # Last two words entered
          searchString2 <- paste(cleanText[(length(cleanText)-1):length(cleanText)], collapse=" ")
     } else if (len == 1) {
          # Last word entered
          searchString1 <- cleanText[(length(cleanText)):length(cleanText)]
     }
     
     # Predict based on last three words
     if (!is.na(searchString3)) {
          prediction <- dict4[dict4$term == searchString3, c(2,4)]
          if (nrow(prediction) == 0) {
               # If string not found, create search string with last two words entered
               searchString2 <- paste(cleanText[(length(cleanText)-1):length(cleanText)], collapse=" ")
          }
     }
     # Predict based on last two words
     if (!is.na(searchString2)) {
          prediction <- dict3[dict3$term == searchString2, c(2,4)]
          if (nrow(prediction) == 0) {
               # If string not found, create search string with last word entered
               searchString1 <- cleanText[(length(cleanText)):length(cleanText)]
          }
     }
     # Predict based on last word
     if (!is.na(searchString1)) {
          prediction <- dict2[dict2$term == searchString1, c(2,4)]
     }

     # Return top n rows with n defined by user input (resultRange)
     # If required add frequency
     if (is.null(addInfo)) {
          head(as.data.frame(prediction[c(1)]), resultRange)
     } else {
          head(as.data.frame(prediction[c(1,2)]), resultRange)
     }
}
