#######################################################################
# Project:  TRIAS geopolitics paper
# Task:     App model for collecting human codes of country statements
# Author:   @ChRauh (30.06.2025)
#######################################################################


library(shiny)
library(shinyWidgets)

# Shiny App
ui <- fluidPage(
  titlePanel(HTML("<br><em>Coding task:</em> Are countries presented in more <span style= \"color: #1a9850;\">friendly</span> or more <span style= \"color: #d73027;\">adversarial</span></em> terms?<br><br>")),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p(HTML("<b><i>You are asked to read the text in the frame to the right, provide your assessment of the text, and continue to the next example.</i></b>")),
      p("The texts are excerpts from the official communication of a political institution. Each mentions one or sometimes several countries from around the world."),
      p(HTML("We want to learn <em><b>whether the particular text frames a <u>particular country</u> in more <span style= \"color: #1a9850;\">friendly</span> or more <span style= \"color: #d73027;\">adversarial</span> terms</b></em>.")),
      # p(HTML("Does the statement say or imply that the communicating institution sees the <u>country</u> in dtendency more as a friend or rather as an adversary?")),
      p(HTML("Some texts might evaluate the country directly, others might mention it in passing or frame it rather indirectly only. Often, country framing will thus be a <b>matter of degree</b> and there are <b>no right or wrong answers</b> - only what you personally read from the individual text snippet matters here.")),
      # p(HTML("Please assess - just from the text alone - whether the communicating institution tends to present the country in question more as a friend or as an enemy.")),
      # p(HTML()),
      HTML("<br>"),
      verbatimTextOutput("status"),  # Dynamic progress display
      HTML("<br>"),
      p(HTML("Once you click on the continue button, your assessment is saved. <b>You can close and later reload the app whenever you need a break.</b>")),
      p("Many thanks for your help!"),
      p(HTML("I am happy to tell you more about the research context once the coding is done. If other questions occur, please do not hesitate to contact me via <a href=\"mailto:rauh@wzb.eu\">Email</a>."))
    ),
    
    mainPanel(
      HTML("<br>"),
      h3("The text and your personal assessment"),
      div(
        id = "text-frame",
        uiOutput("current_text")
      ),
      # h3("Your assessment of the text"),
      HTML("<br>"),
      
      # Classification radio buttons and next button placed together
      div(
        # Full question as a separate reactive textOutput
        htmlOutput("question_text"),
        
        # radioButtons("classification", label = NULL,
        #              choices = c("Clearly a friend", "Rather a friend",  "Rather an enemy", "Clearly an enemy"), 
        #              inline = TRUE,
        #              selected = character(0)), # Ensures no pre-selected option
        
        radioGroupButtons(
          inputId = "classification",
          label = NULL,
          choices = c("Very friendly", "Rather friendly", "Neutral", "Rather adversarial", "Very adversarial"),
          selected = character(0),
          direction = "horizontal",  # like `inline = TRUE`
          individual = TRUE,         # allows styling each button
          checkIcon = list(yes = icon("check")),
          status =  c("one", "two", "three", "four", "five") # 'Name' buttons - interacts strangely on some machines
        ),
        
        HTML("<br>"),
        actionButton("next_btn", "Continue", style = "margin-top: 10px; background-color: black; color: white;")
      ),
      
      # Add custom CSS for the text frame
      tags$style(HTML("
        #text-frame {
          border: 2px solid darkred;       /* Border around the text */
          border-radius: 5px;              /* Rounded corners */
          padding: 15px;                   /* Spacing inside the frame */
          background-color: white;         /* Light background color */
          margin-top: 20px;                /* Space above the frame */
          margin-bottom: 20px;             /* Space below the frame */
          font-size: 20px;                 /* Adjust text size */
          color: #333;                     /* Text color */
        }
        
        /* Style the question label */
        .shiny-input-container > label {
          font-size: 18px;        /* Larger font size for the question label */
          color: darkred;         /* Dark red color for the question label */
          font-weight: bold;      /* Bold text */
          margin-bottom: 10px;    /* Space below the question label */
          font-style: normal;     /* Ensure question label is not italic */
        }
        
        /* Style the radio button texts */
        .shiny-options-group {
          font-size: 18px;        /* Slightly smaller font size */
          font-style: italic;     /* Make radio button texts italic */
        }
        
        /* General button styling by data-value */
        .btn-one { background-color: #1a9850; color: white; font-weight: bold; font-size: 18px;}
        .btn-two { background-color: #91cf60; color: white; font-weight: bold; font-size: 18px;}
        .btn-three { background-color: #999999; color: white; font-weight: bold; font-size: 18px;}
        .btn-four { background-color: #fc8d59; color: white; font-weight: bold; font-size: 18px;}
        .btn-five { background-color: #d73027; color: white; font-weight: bold; font-size: 18px;}
        "))
    )
  )
)

server <- function(input, output, session) {
  
  # Load the text data from a local file only once at the start
  # coder <- 2
  # textfile <- paste0("coder", coder, "texts.csv")
  textfile <- "validation_sample_coder3.csv" 
  text_data <- read.csv(textfile, stringsAsFactors = FALSE)
  
  
  # Reactive value to keep track of the current text index
  rv <- reactiveValues(index = 1, data = text_data)  # Store the data in memory
  
  # Function to skip already classified rows
  skip_to_next_unclassified <- function() {
    while (rv$index <= nrow(rv$data) && !is.na(rv$data$label[rv$index]) && rv$data$label[rv$index] != "") {
      rv$index <- rv$index + 1
    }
  }
  
  # Skip already classified rows on app initialization
  observeEvent(rv$index, {
    skip_to_next_unclassified()
  }, ignoreInit = FALSE)
  
  
  # Display the current country in the question text
  current_country <- reactive(rv$data$country[rv$index])
  output$question_text <- renderUI({
    HTML(paste0(
      '<div style="font-size: 20px; color: black; font-weight: bold;">',
      "Does this statement present <em><u><b>",
      current_country(),
      "</b></u></em> in more friendly or more adversarial terms?<br><br>"
    ))
  })
  
  
  # Display the current text bit
  output$current_text <- renderUI({
    if (rv$index <= nrow(rv$data)) {
      # Display the current text
      HTML(rv$data$text[rv$index])
    } else {
      # Completion message
      HTML("<b>All classifications completed. Thank you very much!</b> <br>You can close the app now.")
    }
  })
  
  # Display progress dynamically
  output$status <- renderText({
    paste0("Your task progress: ", sum(!is.na(rv$data$label) & rv$data$label != ""), 
           "/", nrow(rv$data), " (", 
           round((sum(!is.na(rv$data$label) & rv$data$label != "") / nrow(rv$data)) * 100, 0), "%)")
  })
  
  # # Debugging input classification
  # output$class_debug <- renderText({
  #   paste("Input value:", input$classification)
  # })
  
  # Save classification and move to the next text when 'Next' is clicked
  observeEvent(input$next_btn, {
    if (rv$index <= nrow(rv$data)) {
      if (!is.null(input$classification) && input$classification != "") {
      # if (!is.null(input$classification) && nzchar(input$classification)) {
        # Save the classification directly in the in-memory data object
        rv$data$label[rv$index] <- input$classification
        
        # Write the updated data to the file immediately
        write.csv(rv$data, file = textfile, row.names = FALSE)
        
        # Reset the selection
        # updateRadioButtons(session, "classification", selected = character(0))
        updateRadioGroupButtons(session, "classification", selected = character(0))

        # Move to the next text
        rv$index <- rv$index + 1
        
        # Skip already classified rows
        skip_to_next_unclassified()
        
      } else {
        showNotification("Please choose your assessment before moving to the next text.", type = "error")
      }
    } else {
      showNotification("No more texts to classify. You're done! Thanks!", type = "warning")
    }
    
  })
  
}

shinyApp(ui, server)
