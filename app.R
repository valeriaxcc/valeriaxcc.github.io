library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Figure Skating Jumps Guessing Game"),
  
  # Text to display instructions
  mainPanel(
    h3("Can you guess how many rotations are in the randomly selected jump?"),
    textOutput("selectedJumpText"),  # Display the selected jump here
    actionButton("generateJump", "Generate Jump"),  # Button to generate a new jump
    numericInput("userGuess", "Enter the number of rotations:",
                 value = NULL, min = 1, max = 4, step = 1),
    actionButton("submitGuess", "Submit")
  ),
  
  # Output to display game result
  verbatimTextOutput("gameResult")
)

# Define server logic
server <- function(input, output) {
  
  # Reactive value to store the selected jump
  selected_jump <- reactiveVal(NULL)
  
  # Mapping between jumps and number of rotations
  jump_rotations <- list(
    "Single" = 1,
    "Double" = 2,
    "Triple" = 3,
    "Quad" = 4
  )
  
  # Function to generate a random jump
  generate_random_jump <- function() {
    sample(names(jump_rotations), 1)
  }
  
  # Function to play the figure skating jumps guessing game
  play_skating_jumps_game <- function(selected_jump, player_guess) {
    correct_rotations <- jump_rotations[[selected_jump]]
    
    if (is.null(correct_rotations)) {
      return("Error: Invalid jump selected. Please generate a new jump.")
    }
    
    if (player_guess == correct_rotations) {
      if (selected_jump == "Quad") {
        return("Wow! You guessed the Quad jump correctly. Amazing!")
      } else {
        return("Congratulations! You guessed correctly.")
      }
    } else {
      return(paste("Sorry, that's incorrect. The correct number of rotations for", selected_jump, "is", correct_rotations))
    }
  }
  
  # Event handler for "Generate Jump" button
  observeEvent(input$generateJump, {
    selected_jump_val <- generate_random_jump()  # Generate a new random jump
    selected_jump(selected_jump_val)  # Update the reactive value
  })
  
  # Event handler for "Submit" button
  observeEvent(input$submitGuess, {
    if (is.null(selected_jump()) || is.null(input$userGuess)) {
      return()  # Do nothing if jump or guess is not selected
    }
    
    result <- play_skating_jumps_game(selected_jump(), input$userGuess)
    
    output$gameResult <- renderPrint({
      result
    })
  })
  
  # Display the selected jump text
  output$selectedJumpText <- renderText({
    if (!is.null(selected_jump())) {
      paste("Selected Jump:", selected_jump())
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
