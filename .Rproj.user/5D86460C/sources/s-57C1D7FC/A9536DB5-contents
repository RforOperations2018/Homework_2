#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(ggplot2)
require(plotly)
Titanic <- as.data.frame(Titanic)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Titanic Shiny App"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          checkboxGroupInput(inputId = "x",
                      label = "Which class of passenger on the Titanic was the largest?",
                     choices = c("1st", "2nd", "3rd", "Crew"),
                     selected = "1st"
                     ),
          sliderInput(inputId = "hours",
                      label = "Give a range that captures how many hours the Titanic took to sink",
                      value = c(2,5),
                      min = 0,
                      max = 10,
                      step = 0.25),
          radioButtons(inputId = "reasons",
                             label = "Why did the Titanic sink?",
                             choices = c("The captain wasn't paying attention",
                                         "Rose was distracting the captain",
                                         "Aliens",
                                         "An iceberg",
                                         "The Titanic didn't sink"))
          
          
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = "titanic", width = "75%"),
        plotOutput(outputId = "titanic2", width = "75%")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
     output$titanic <- renderPlot({ggplot(data = Titanic, aes(x = Class, y = Freq, fill = Class)) + 
         geom_bar(stat="identity", position=position_dodge()) + 
         ggtitle("Titanic Classes and Age, Broken Down by Sex")+
         scale_fill_manual(values= c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
         ylab("Total Passenger Count")})
     output$titanic2 <- renderPlot({ggplot(data = Titanic, aes(x = Age, y = Freq, fill = Age)) + 
         geom_bar(stat="identity", position=position_dodge()) +
                    ylab("Total Passenger Count")})
   }


# Run the application 
shinyApp(ui = ui, server = server)

