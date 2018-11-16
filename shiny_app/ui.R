# Create app
ui <- shinyUI(
  fluidPage(
    headerPanel("Breach Data Analysis"),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overview"),
                  tabPanel("Breach occurences",
                           fluidRow(
                             column(9, plotOutput("industryBreach", width = 900, height = 600))
                           )),
                  tabPanel("Breaches over time",
                           fluidRow(
                             column(9, plotlyOutput("annualBreaches", width = 900, height = 600))
                           ),
                           fluidRow(
                             column(9, plotOutput("breachMap", width = 900, height = 600))
                           )
                  )
      )
    )
  )
)