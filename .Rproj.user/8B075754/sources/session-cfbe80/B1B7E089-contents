library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(sliderInput("samplesize","Sample Size:",min = 100,max = 10000,value = 1000)),
    mainPanel(plotOutput("distPlot"))
  )
)
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$samplesize),col='darkorchid',xlab="Sample",main="Standard Normally Distributed Sample")},
    height=300
  )
}
shinyApp(ui = ui, server = server)