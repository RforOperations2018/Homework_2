library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(reshape2)
library(data.table)
library(DT)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)

## Creation of
ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}
#https://data.wprdc.org/datastore/dump/7f5da957-01b5-4187-a842-3f215d30d7e8
# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}


gender <- sort(ckanUniques("7f5da957-01b5-4187-a842-3f215d30d7e8", "Gender")$Gender)
race_choices <- sort(ckanUniques("7f5da957-01b5-4187-a842-3f215d30d7e8", "Race")$Race)
gender <- recode(gender, F = 'Female',  M = 'Male' )
race_choices <- recode(race_choices, A = "Asian", B = "Black", H = "Hispanic", I = "Indian", U = "Unknown", W = "White", x = "Not reported")


# Define UI for application using a fluid page layout
ui <- fluidPage(
# Layout of Basic Input          
  titlePanel("Allegheny County Jail Bookings for September "),
      tabsetPanel(
        #1st Panel for Gender and Race Information and Refresh Button
        tabPanel("Basic Information",
                  fluidRow(
                    column(4,
                           wellPanel(selectInput("race", label = "Race", choices = race_choices, 
                                                 selected = c("Black", "White", "Hispanic"),
                                                 multiple = TRUE,
                                                 selectize = TRUE)),
                           wellPanel(radioButtons("gender", label = "Options", choices = gender)),
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

  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%227f5da957-01b5-4187-a842-3f215d30d7e8%22%20WHERE%20%27", 
            input$gender ,"%27%20=%20%22Gender%22%20AND%20%22Race%22%20IN%20%27", 
           input$Race, "%27%20")
  
  # Load and clean data
  clean.data <- ckanSQL(url) %>%
    mutate(Gender = ifelse(STATUS == 1, "Female", "Male"), Race = ifelse(STATUS == 1, "Asian, Black", "Hispanic"))
  
  return(dat311)
  #clean.data %>% filter(input$gender == Gender & Race %in% input$race)
  })

#Create api ask
##https://data.wprdc.org/api/3/action/datastore_search_sql
#Create Interactive Race and Gender Plot  
   output$race.info <- renderPlotly({
    df2 <- df.filter()
    # ggplots are easier to read when each plus starts a new line.
    ggplotly(ggplot(df2, aes(x = Race, fill = Race)) + 
               geom_bar() + 
               ggtitle("Arrests by Race and Gender for September") +
               ylab("Total"))})

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
    updateSelectInput(session, "race", selected = c("Black", "Hispanic", "White"))
    showNotification("You have successfully reset the filters for race", type = "message")
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

