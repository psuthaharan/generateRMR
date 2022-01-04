library(shiny)

shinyUI(fluidPage(
  
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

  )
  
))