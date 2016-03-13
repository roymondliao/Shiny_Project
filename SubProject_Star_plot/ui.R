### SubProject  Star Plot - ui.R
library(shiny)
library(htmlwidgets)
library(rCharts)
shinyUI(fluidPage(
  titlePanel("Users Feedback Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("input_date", label = h3("Enter date range : "), start = Sys.Date()-7, end = Sys.Date()-1),
      submitButton(text = "Submit"),
      helpText(h3("Note:"),
               p("The GP's Star data is update at 18:00 every day."),
               p("The data are from", a("WEB URL")),
               br(),
               em("If you have any question or problem, please contact below:"),
               h6("Email : //EMAIL"),
               h6("Wechat : //NAME"),
               h6("Email : //EMAIL"),
               h6("Wechat : //NAME"))
    ),
    ## plot
    mainPanel(
      #dataTableOutput(outputId = "dates")
      tabsetPanel(type = "tabs", 
                  tabPanel("Data Table", dataTableOutput("dates")), 
                  tabPanel("Plot", showOutput("plots", "highcharts")))
                  #tabPanel("test1", textOutput("test")))
      )
  )
))