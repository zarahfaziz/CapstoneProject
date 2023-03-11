
library(shiny)

# Defining and importing the word / phrase count data.
# Word counts = sample_wordcounts.
# 2-gram counts = twogram_counts.

predictionMatch <- function(userInput, ngrams) {
  
  # 2-gram and 1-gram word/phrase predictions.
  if (ngrams < 3) {
    userInput1 <- userInput[length(userInput)]
    dataTokens <- freq2ngram %>% filter(variable == userInput1)
    return(dataTokens$outcome[1:3])
  }
  return(NA)
}

cleanInput <- function(input) {
  if (input == "" | is.na(input)) {
    return("")
  }

  input <- tolower(input)
 

  #Prediction model

predictNextWord <- function(input, word = 0) {
  
  input <- cleanInput(input)
  
  if (input[1] == "") {
    output <- initialPrediction
  } else if (length(input) == 1) {
    output <- predictionMatch(input, ngrams = 2)
  } else if (length(input) == 2) {
    output <- predictionMatch(input, ngrams = 3)
  } else if (length(input) > 2) {
    output <- predictionMatch(input, ngrams = 4)
  }
  
  if (word == 0) {
    return(output)
  } else if (word == 1) {
    return(output[1])
  } else if (word == 2) {
    return(output[2])
  } else if (word == 3) {
    return(output[3])
  }
  
}


#Server file.

shinyServer(function(input, output) {

  output$userSentence <- renderText({input$userInput});
  
  observe({
    numPredictions <- input$numPredictions
    if (numPredictions == 1) {
      output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
      output$prediction2 <- NULL
      output$prediction3 <- NULL
    } else if (numPredictions == 2) {
      output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
      output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
      output$prediction3 <- NULL
    } else if (numPredictions == 3) {
      output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
      output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
      output$prediction3 <- reactive({predictNextWord(input$userInput, 3)})
    }
  })
  
})
