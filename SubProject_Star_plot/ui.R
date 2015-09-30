library(shiny)
library(htmlwidgets)
library(rCharts)
shinyUI(fluidPage(
  titlePanel("Users Feedback Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("input_date", label = h3("Enter date range : "), start = Sys.Date()-10, end = Sys.Date()-6),
      submitButton(text = "Submit"),
      helpText(h3("Note:"),
               p("The GP's Star data is update at 18:00 every day."),
               p("The data are from", a("http://mp.ijinshan.com/")),
               br(),
               em("If you have any question or problem, please contact below:"),
               h6("Email : raymond.liao@ileopard.com"),
               h6("Wechat : yuyu liao"),
               h6("Email : frank.tseng@ileopard.com"),
               h6("Wechat : Frank Tseng"))
    ),
    ## plot
    mainPanel(
      #dataTableOutput(outputId = "dates")
      tabsetPanel(type = "tabs", 
                  tabPanel("Data Table", dataTableOutput("dates")), 
                  tabPanel("Plot", showOutput("plots", "highcharts")))
      )
  )
))