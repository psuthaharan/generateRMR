library(shiny)
shinyUI(fluidPage(
  titlePanel(h1('Research Performance Progress Report (RPPR)', align="center")),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select CAPR Site:", choices = c("Yale", "mtcars", "trees")),
      br(),
      helpText(" Select the download format"),
      radioButtons("type", "Format type:",
                   choices = c("Excel (CSV)", "Text (TSV)","Text (Space Separated)", "Doc")),
      br(),
      helpText(" Click on the download button to download the dataset observations"),
      downloadButton('downloadData', 'Download')
      
    ),
    mainPanel(
      
      tableOutput('table')
    )
  )
))