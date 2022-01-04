library(shiny)
shinyUI(fluidPage(
  titlePanel(h1('Research Performance Progress Report (RPPR)', align="center")),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select CAPR Site:", choices = c("Yale", "Emory", "trees")),
      br(),
      helpText(" Select the download format"),
      radioButtons("type", "Format type:",
                   choices = c("Excel (CSV)")),
      br(),
      helpText(" Click on the download button to download the dataset observations"),
      downloadButton('downloadData', 'Download')
      
    ),
    mainPanel(
      
      tableOutput('table')
    )
  )
))