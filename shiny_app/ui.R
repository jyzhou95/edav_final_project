# Create app
ui <- shinyUI(
  fluidPage(
    headerPanel("Breach Data Analysis"),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overview",
                           fluidRow(
                             h2("Project description"),
                             column(9, textOutput("projectDescription"))
                           ),
                           br(),
                           fluidRow(
                             h2("Data description")
                           ),
                           br(),
                           fluidRow(
                             h2("Data pre-processing")
                           )
                           ),
                  tabPanel("Data",
                           fluidRow(
                             column(9, dataTableOutput("rawData"))
                           )),
                  tabPanel("Breach occurences",
                             fluidRow(
                               column(9, plotlyOutput("industryBreach", width = 900, height = 600))
                             )
                           ),
                  
                  tabPanel("Breaches over time",
                           fluidRow(
                             column(9, checkboxInput(inputId = "quarterly", 
                                                     label = "Quarterly: ",
                                                     value = FALSE))
                           ),
                           fluidRow(
                             column(9, plotlyOutput("annualBreaches", width = 900, height = 600))
                           ),
                           
                           br(),
                           br(),
                           
                           fluidRow(
                             column(9, plotlyOutput("breachMap", width = 900, height = 600))
                           ),
                           
                           
                           fluidRow(
                             column(9, plotlyOutput("breachScatterPlot", width = 900, height = 600))
                           )
                  ),
                  tabPanel("Data Quality Exploration",
                           )
      )
    )
  )
)