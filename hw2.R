library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(reshape2)
library(data.table)
library(DT)

#Data Loading and Cleaning
Allegheny.County.Jail.2018.raw <- read.csv("~/GitHub/Homework_2/downloadfile_hw2.csv")
clean.data <- subset(Allegheny.County.Jail.2018.raw, select = -X_id) %>% na.omit(clean.data)
levels(clean.data$Gender) <- c("Female", "Male")
levels(clean.data$Race) <- c("Not Reported", "Asian", "Black", "Hispanic", "Indian", "Unknown", "White", "Mixed-race")


# Define UI for application using a fluid page layout
ui <- fluidPage(
# Layout of Basic Input          
  titlePanel("Allegheny County Jail Bookings for September "),
      tabsetPanel(
        #1st Panel for Gender and Race Information and Refresh Button
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
         #2nd Panel for Download Button and Data Table
         tabPanel("Downloads and Table",
                   fluidRow(column(4, wellPanel(downloadButton("new.download", label = "Download New File"))),
                            column(8, wellPanel(dataTableOutput("table", width = "80%")))))
      ))


# Define server logic required to draw two bar graphs, a data table, and a download button
server <- function(input, output, session = session) {
# create reactive element  
df.filter <- reactive ({
clean.data %>% filter(input$gender == Gender & Race %in% input$race)})
 
#Create Interactive Race and Gender Plot  
   output$race.info <- renderPlotly({
    df2 <- df.filter()
    ggplotly(ggplot(df2, aes(x = Race, fill = Race)) + geom_bar() + ggtitle("Arrests by Race and Gender for September") 
               + ylab("Total"))})

#Create Interactive Gender Plot
  output$gender.info <- renderPlotly({
    df1 <- df.filter()
    ggplotly(ggplot(df1, aes(x = Gender, fill = Gender)) + 
      geom_bar()+ ggtitle("Arrests by Sex for September") + ylab("Total"))})
  
#Create Download Button that allows people to download info
  output$new.download <- downloadHandler(
    filename = function(){ 
      paste("new.download", Sys.Date(), ".csv", sep = "" )},
    content = function(file) {
      write.csv(df.filter(), file, row.names = FALSE)
    })
#Create table  
  output$table <- renderDataTable({
    functioning.table <- df.filter()
    subset(functioning.table, select = c(Date, Gender, Race, Age.at.Booking, Current.Age))
  })
#Create Refresh Button   
  observeEvent(input$click, {
    updateSelectInput(session, "race", selected = c("Black", "White", "Hispanic"))
    showNotification("You have successfully reset the filters for race", type = "message")
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

