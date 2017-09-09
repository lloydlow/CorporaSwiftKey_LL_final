library(shiny)

function(input, output) {
  output$sentence <- renderText({
    input$sentence
  })
  
  predicted <- eventReactive(input$predictButton, {
    theNextWord(input$sentence, input$words_to_predict)
  })
  output$tableFinal <- renderTable({ predicted() })
}
