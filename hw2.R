library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(reshape2)
library(data.table)
library(DT)

#Data Loading and Cleaning
Allegheny.County.Jail.2018.raw <- read.csv("~/GitHub/Homework_2/7f5da957-01b5-4187-a842-3f215d30d7e8.csv")
clean.data <- subset(Allegheny.County.Jail.2018.raw, select = -X_id) %>% na.omit(clean.data)
levels(clean.data$Gender) <- c("Female", "Male")
levels(clean.data$Race) <- c("Not Reported", "Asian", "Black", "Hispanic", "Indian", "Unknown", "White", "Mixed-race")


# Define UI for application using a fluid page layout
ui <- fluidPage(
# Layout of Basic Input          
  titlePanel("Allegheny County Jail Bookings for September "),
      tabsetPanel(
         tabPanel("Basic Information",
                  fluidRow(
                    column(4,
                           wellPanel(selectInput("race", label = "Race", choices = c("Not Reported", "Asian", "Black",
                                                                                     "Hispanic", "Indian", "Unknown", 
                                                                                     "White", "Mixed-race"), 
                                                 selected = c("Black", "White", "Hispanic"),
                                                 multiple = TRUE,
                                                 selectize = TRUE)),
                           wellPanel(radioButtons("gender", label = "Options", choices = c("Male", "Female")))),
                    column(8, wellPanel( plotOutput("race.info"), plotOutput("gender.info"))))),
                  # fluidRow(
                  #   column(4,
                  #          wellPanel(selectInput("race", label = "Race", choices = c("Not Reported", "Asian", "Black",
                  #                                                                    "Hispanic", "Indian", "Unknown", 
                  #                                                                    "White", "Mixed-race"), 
                  #                                selected = c("Black", "White", "Hispanic"),
                  #                                multiple = TRUE,
                  #                                selectize = TRUE)))),
                  # column(8, offset =  4, wellPanel(plotOutput("race.info")))),
         # tabPanel("Race",
         #          fluidRow(
         #            column(4,
         #                    wellPanel(selectInput("race", label = "Race", choices = c("Not Reported", "Asian", "Black",
         #                                                                          "Hispanic", "Indian", "Unknown", 
         #                                                                          "White", "Mixed-race"), 
         #                                                                          selected = c("Black", "White", "Hispanic"),
         #                                                                            multiple = TRUE,
         #                                                                            selectize = TRUE)))),
         #          column(8, wellPanel(plotOutput("race.info")))),
         tabPanel("Downloads and Table",
                   fluidRow(column(4, wellPanel(downloadButton("new.download", label = "Download New File")),
                                      wellPanel(actionButton("click", "Click to See What Happens"))),
                            column(8, wellPanel(renderDT("table", "Basic Data Table of September Bookings")))))
      ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  #ddply(clean.data, ~ Race + Gender, summarize, mean = mean(Age.at.Booking))
  #filter.data <- reactive({subset(clean.data, subset = c())})
  #clean.data.2 <- melt(clean.data)
  df.filter <- reactive ({
    clean.data %>% filter(input$gender == Gender & input$race %in% Race)})
   
   
    # # if (length(input$race) > 0 ) {
    # clean.data <- subset(clean.data, race %in% input$race)}
    # return(clean.data)})

  output$race.info <- renderPlot({
    df2 <- df.filter()
    ggplot(df2, aes(x = Race, fill = Race)) + geom_bar()})

  output$gender.info <- renderPlot({
    df1 <- df.filter()
    ggplot(df1, aes(x = Gender, fill = Gender)) + 
      geom_bar()})
  
      
  
  output$new.download <- downloadHandler(
    filename = function(){ 
      paste("newdownload", Sys.date(), ".csv", sep = "" )},
    content = function(file) {
      write.csv(clean.data, file)
    })
  
  output$table <- renderDataTable({
     clean.data
    
    subset(starwars, select = c(name, height, mass, birth_year, homeworld, species, films))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

