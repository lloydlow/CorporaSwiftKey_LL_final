library(shiny)
library(shinythemes)

# Define UI for dataset viewer application
fluidPage(
  theme = shinytheme("cerulean"),
  # Application title
  titlePanel("Next word prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("sentence", "Enter your sentence (e.g. This is amazing):", "This is amazing"),
      br(),
      selectInput("words_to_predict", "Choose number of words to predict:", 
                  choices = seq(1:10),selected = 5),
      br(),
      actionButton("predictButton", "Predict!")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction",
                 h3(textOutput("sentence", container = span)),
                 #h4('Words predicted and score:'),
                 tableOutput("tableFinal")
                 ),
        tabPanel("Help",
                 h4("How to use this App"),
                 p("This is a simple to use next-word prediction App based on n-gram language model and 
                   implemented stupid backoff scoring scheme at its backend. To use, simply type the sentence 
                   you wish to have the next word predicted in the top left textbox (e.g. This is amazing) and choose from the
                   pulldown menu how many words you like predicted (default is 5). When both of these fields
                   are set, then click on the Predict! button."),
                 h4("Results"),
                 p("The results are displayed as a table of word(s) predicted in the first column and its 
                   corresponding score in the second column. The score is really somewhat arbitrary in 
                   stupid backoff scoring scheme but this should not be confused with probability, nonetheless
                   higher score indicates stronger prediction outcome. In the case where there is no word match
                   found in the cleaned ngram tables, the top five most common unigram is returned. Try
                   typing \"Biology is the most common\" in the textbox to see an example."),
                 h4("Source code"),
                 a("https://github.com/lloydlow/CorporaSwiftKey_LL_final")
                 )
      )
    )
  )
)
