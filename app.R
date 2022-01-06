####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)

rm(list=ls())
setwd(getwd())

#data
emory_df <- read.csv("rppr/Emory.csv")
nu_df <- read.csv("rppr/NU.csv")
temple_df <- read.csv("rppr/Temple.csv")
uci_df <- read.csv("rppr/UCI.csv")
uga_df <- read.csv("rppr/UGA.csv")
umbc_df <- read.csv("rppr/UMBC.csv")
yale_df <- read.csv("rppr/Yale.csv")


# Define UI for app that draws a histogram ----
ui <- shinyUI(fluidPage(

  titlePanel(h1('Research Performance Progress Report (RPPR)', align="center")),
  
  fluidRow(
    
    column(6,
           # sidebarLayout(position = "right",
           sidebarPanel(
             selectInput("dataset", "Select CAPR Site:", choices = c("Yale", "Emory", "NU", "Temple", "UCI", "UGA","UMBC")),
             br(),
             helpText(" Select the download format"),
             radioButtons("type", "Format type:",
                          choices = c("Excel (CSV)")),
             br(),
             helpText(" Click to download the RPPR:"),
             downloadButton('downloadData', 'Download')
             
             # ),
           ),
           
           column(6,
                  mainPanel(
                    
                    tableOutput('table')
                  )
           )
           
    ),
    tags$title("RPPR"),
  )
  
))

# Define server logic required to draw a histogram ----
server <- shinyServer(function(input, output) {
  
  ###### Reactive function to fetch the dataset observations based on the user's choice ---- > this will be used in the renderTable ###
  datasetInput <- reactive({
    # Fetch the appropriate data object, depending on the value of input$dataset - the dataset selected by the user
    # switch(expression, list of alternatives .. )  switch evaluates EXPR and accordingly chooses one of the further arguments 
    #     If EXPR evaluates to a character string then that string is matched (exactly)to the names of the elements in the alternatives.... 
    #     If there is a match then that element is evaluated
    switch(input$dataset,
           "Yale" = yale_df,
           "Emory" = emory_df,
           "NU" = nu_df,
           "Temple" = temple_df,
           "UCI" = uci_df,
           "UGA" = uga_df,
           "UMBC" = umbc_df)
  })
  
  ##### A reactive function for the file extension ---- > this reactive function will be used by download handler ######
  fileext <- reactive({
    switch(input$type,
           "Excel (CSV)" = "csv", "Text (TSV)" = "txt","Text (Space Separated)" = "txt", "Doc" = "doc")
    
  })
  
  ### Output of renderTable will be used in the mainPanel of ui.R to display the dataset observations in the table
  output$table <- renderTable({
    datasetInput()
    
  },rownames = TRUE)
  
  ############ Download handler for the download button ####################
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$dataset, fileext(), sep = ".") # example : iris.csv, iris.doc, iris.txt 
      
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$type, "Excel (CSV)" = ",", "Text (TSV)" = "\t","Text (Space Separated)" = " ", "Doc" = " ")
      
      # Write to a file specified by the 'file' argument
      write.csv(datasetInput(), file, sep = sep,
                row.names = TRUE)
    }
  )
})

# Create Shiny app ----
shinyApp(ui = ui, server = server)
