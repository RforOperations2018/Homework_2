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
        #1st Panel for Gender and Race Information 
        tabPanel("Basic Information",
                  fluidRow(
                    column(4,
                           wellPanel(selectInput("race", label = "Race", choices = c("Not Reported", "Asian", "Black",
                                                                                     "Hispanic", "Indian", "Unknown", 
                                                                                     "White", "Mixed-race"), 
                                                 selected = c("Black", "White", "Hispanic"),
                                                 multiple = TRUE,
                                                 selectize = TRUE)),
                           wellPanel(radioButtons("gender", label = "Options", choices = c("Male", "Female"))),
                           wellPanel(actionButton("click", "Click to See What Happens"))),
                   column(8, wellPanel( plotlyOutput("race.info")), 
                             wellPanel(plotlyOutput("gender.info"))))),
         #2nd Panel for Download Button, Action Button, and Data Table
         tabPanel("Downloads and Table",
                   fluidRow(column(4, wellPanel(downloadButton("new.download", label = "Download New File"))),
                            column(8, wellPanel(dataTableOutput("table", width = "80%")))))
      ))


# Define server logic required to draw two bar graphs, a data table, and a download button
server <- function(input, output) {
  
  df.filter <- reactive ({
    clean.data %>% filter(input$gender == Gender & Race %in% input$race)})
   
   
   

  output$race.info <- renderPlotly({
    df2 <- df.filter()
    ggplotly(ggplot(df2, aes(x = Race, fill = Race)) + geom_bar() + ggtitle("Arrests by Race for September"))})

  output$gender.info <- renderPlotly({
    df1 <- df.filter()
    ggplotly(ggplot(df1, aes(x = Gender, fill = Gender)) + 
      geom_bar()+ ggtitle("Arrests by Sex for September"))})
  
      
  
  output$new.download <- downloadHandler(
    filename = function(){ 
      paste("newdownload", Sys.date(), ".csv", sep = "" )},
    content = function(file) {
      write.csv(df.filter(), file)
    })
  
  output$table <- renderDataTable({
    functioning.table <- df.filter()
    subset(functioning.table, select = c(Date, Gender, Race, Age.at.Booking, Current.Age))
  })
    observeEvent(input$click, {
      print(paste("Hey, you clicked a button!"))
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

