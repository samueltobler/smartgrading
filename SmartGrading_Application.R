## © 2023 Samuel Tobler

# Run this R Shiny application either from your R Studio or use the following link:
# https://stobler.shinyapps.io/smartgrading 

# Load necessary libraries
library(shiny)
library(shinythemes)
library(httr)

# Define UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  includeCSS("www/style.css"),
  tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #58B99D;
                                                  border-top: 1px solid #58B99D ;
                                                  border-bottom: 1px solid #58B99D ;}
                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #58B99D }'
  ))),br(),
  titlePanel("👾 GenAI Smart Grading"),br(),
  h2("Instructions", style = "padding-top: 0;margin-top: 0px"),  # Reduce top padding
  p("This application allows you to use generative AI models to evaluate provided (student) 
        answers to your questions. Additionally, you can give sample solutions and instruction guidelines 
        (i.e., how many points should be assigned for a specific answer) and indicate how many points can be achieved."),
  
  p("To get started, upload a .csv file containing student answers and specify the required parameters below. 
        Once all values are inputted, click on the grading button to utilize generative AI for evaluation."),
  p("Please note that it is essential to double-check the provided scores with the scores you would have assigned."),
  br(),
  h3("Data upload"),
  br(),
  fileInput("data", "Upload your CSV file with student answers",
            multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"), width = "100%"),
  checkboxInput("header", "Header", TRUE),
  
  # Input: Select separator ----
  radioButtons("sep", "Separator",
               choices = c(Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"),
               selected = ","),
  
  h5("Data inspection", style = "font-weight: bold;"),
  tableOutput("contents"), 
  br(),
  
  h3("Parameter input"),
  h5("Temperature", style = "font-weight: bold;"),
  sliderInput("temperature", label = NULL, min = 0, 
              max = 1, value = 0.7, step = 0.1, width = "100%"),
  h5("API key", style = "font-weight: bold;"),
  textInput("apiKey", label = NULL, value = "sk-XXXXXXXXXXXXXXXXXXXXXXXXXX", width = "100%"),
  h5("GPT model", style = "font-weight: bold;"),
  textInput("model", label = NULL, value = "gpt-4o", width = "100%"),
  
  h5("Question", style = "font-weight: bold; padding-bottom: 0; margin-bottom: 0"),
  textInput("question", "", value = "Enter here the posed question", "", width = "100%"),
  
  h5("Sample solution", style = "font-weight: bold; padding-bottom: 0; margin-bottom: 0"),
  textInput("solution", "", value = "Enter here the sample solution", "", width = "100%"),
  
  h5("Maximal number of achievable points", style = "font-weight: bold;"),
  numericInput("points", label = NULL, min = 0, value = 2, width = "100%"),
  
  h5("Instruction", style = "font-weight: bold; padding-bottom: 0; margin-bottom: 0"),
  textInput("instruction", "", value = "Enter here the grading instruction", "", width = "100%"),
  
  h5("File name (optional)", style = "font-weight: bold;"),
  textInput("name", label = NULL, value = "Question_", width = "100%"),  # Added text input for file part
  br(),
  
  
  
  h3("Performance evaluation"),br(),
  actionButton("calculateBtn", "Generate Scores",icon = icon("gear", lib = "font-awesome", style = "padding-right: 5px;")),
  
  br(),br(),
  tableOutput("resultOutput"),
  br(),
  h3("Dowload evaluations"),br(),
  downloadButton("downloadBtn", "Download Scores", icon = icon("download", lib = "font-awesome", style = "padding-right: 5px;")),
  
  br(),br(),br(),br(),
  
  
  HTML("<p style = 'color: #BABFC4;'>© 2023 Samuel Tobler
                 </p>"),br(),br(),
  #  ) #main
  #),
  
)

# Define server logic
server <- function(input, output, session) {
  
  output$contents <- renderTable({
    
    req(input$data)
    
    tryCatch(
      {
        df <- read.csv(input$data$datapath,
                       header = input$header,
                       sep = input$sep)
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    return(df[1:3,])
  }, colnames = F)
  
  makeAPICall <- function(answer) {
    response <- tryCatch({
      POST(
        url = "https://api.openai.com/v1/chat/completions", 
        add_headers(Authorization = paste("Bearer", input$apiKey)),
        content_type_json(),
        encode = "json",
        body = list(
          model = input$model,
          temperature = input$temperature,
          messages = list(
            list(
              "role" = "system",
              "content" = paste("You have to give points to a user-generated answer based on the following information: 
                            This is the question the user tried to answer:", input$question, 
                                "This is the correct answer: ", input$solution, 
                                "The exercise gives", input$points, "Points, and 
                            this is your instruction how to give points:", input$instruction, 
                                "Only return the number of points. If there is no answer, return 0 points. "
              )
            ),
            list(role = "user", content = answer)
          )
        )
      )
    }, error = function(e) {
      showNotification("Error processing the request. Please try again later.", type = "error")
      return(NULL)
    })
    return(response)
  }
  
  calculateResult <- eventReactive(input$calculateBtn, {
    dfx <- read.csv(input$data$datapath, header = input$header, sep = input$sep)
    score <- numeric(nrow(dfx))
    withProgress(message = 'Evaluating...', value = 0.01, {
      for(i in 1:nrow(dfx)) {
        response <- makeAPICall(dfx[i, 1]) # Assuming the student answers are in the first column
        if (!is.null(response)) {
          result <- httr::content(response)$choices[[1]]$message$content
          score[i] <- as.numeric(trimws(result))
        }
        incProgress(1/nrow(dfx))
        
      }
      
    })
    results <- data.frame(Answer = dfx[, 1], Score = as.numeric(score))
    return(results)
  })
  
  # Display calculation result on the page
  output$resultOutput <- renderTable({
    req(input$calculateBtn) # Show result only after the button is clicked
    calculateResult()
  })
  
  output$downloadBtn <- downloadHandler(
    
    filename = function() {
      paste(input$name, '_Evaluation_', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(calculateResult(), file)
    }
  )
  
  
}

# Run the application
shinyApp(ui, server)
