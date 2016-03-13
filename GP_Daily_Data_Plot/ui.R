## gp daily data plot - ui.R
library(shiny)
library(rCharts)
library(htmlwidgets)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("GP Feedback Stat"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("Date",start = Sys.Date()-7, end = Sys.Date()-1, label = h3("Date range"))
      ,submitButton(text ="Submit"),
      helpText(h4("Note:"),
               p("The GP's data is update at 18:00 every day. The data are from", a("http://mp.ijinshan.com/")),
               #p("The data are from", a("http://mp.ijinshan.com/")),
               em("If you have any question or problem, please contact below:"),
               h6("Email : raymond.liao@ileopard.com      Wechat: Yuyu Liao"),
               h6("Email : frank.tseng@ileopard.com       Wechat: Frank Tseng"))
      #,hr(),
      #fluidRow(column(4, verbatimTextOutput("value")))
      ,width=2),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('Pie Chart & Raw Data',
                 column(12,showOutput("chart10","highcharts")),column(3,showOutput("chart5","highcharts"),showOutput("chart8","highcharts")), column(3,showOutput("chart4","highcharts"),showOutput("chart9","highcharts")),column(3,showOutput("chart7","highcharts"),showOutput("chart6","highcharts"))),
        
        tabPanel('1_Applock',
                 column(7,showOutput("chart1","highcharts"),dataTableOutput("mytable")),column(5,plotOutput("plot"))),
        tabPanel('2_Scan',
                 column(7,showOutput("chart2","highcharts"),dataTableOutput("mytable2")),column(5,plotOutput("plot2"))),
        tabPanel('3_Private',
                 column(7,showOutput("chart3","highcharts"),dataTableOutput("mytable3")),column(5,plotOutput("plot3"))),
        tabPanel('4_Junk',
                 column(7,showOutput("chart11","highcharts"),dataTableOutput("mytable4")),column(5,plotOutput("plot4"))),
        tabPanel('9_Wifi',
                 column(7,showOutput("chart12","highcharts"),dataTableOutput("mytable5")),column(5,plotOutput("plot5"))),
        tabPanel('10_Callblock',
                 column(7,showOutput("chart13","highcharts"),dataTableOutput("mytable6")),column(5,plotOutput("plot6")))
      )
      ,width=10)
  )))
