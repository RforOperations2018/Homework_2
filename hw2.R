library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)

data <- read.csv("Something")
clean.data <- data %>% filter()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

