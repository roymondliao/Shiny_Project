# operation daily report - ui.R
library(shiny)

shinyUI(fluidPage(
  headerPanel(title = "Operaction Daily Report"), 
  #sidebarLayout(
    sidebarPanel(
      fileInput("input_file", 'Upload file', accept = c(".xlsx", ".csv")),
      tags$hr(),
      dateRangeInput("input_date", label = h5("Enter date range : "), start = Sys.Date()-2, end = Sys.Date()-1),
      #dateRangeInput("input_date", label = h5("Enter date range : "),  start = as.Date("2015-10-07"), end = as.Date("2015-10-08")),
      #submitButton(text = "Submit"),
      tags$hr(),
      checkboxInput('header', label = "Header", value = TRUE),
      radioButtons("sep", label = 'Separator', choices  =  c(Comma = ",", Semicolon = ";", Tabe = "\t"), selected  = ","),
      radioButtons("quote", label = 'Quote', choices = c(None = "", "Double Quote" = "\"", "Single Quote" = "\'"), selected = "\""),
      tags$hr(),
      selectInput("dataset", "Choose a fucntion:", 
                  choices = c("applock", "callblock", "cms", "phone performance", "scan", "secret box", "suggest")),
      downloadButton('downloadData', 'Download'),
      helpText(h3("Note:"),
               em("If you have any question or problem, please contact below:"),
               h6("Email : raymond.liao@ileopard.com"),
               h6("Wechat : yuyu liao"),
               h6("Email : frank.tseng@ileopard.com"),
               h6("Wechat : Frank Tseng")),
      width = 3),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Applock", DT::dataTableOutput("applock", width = 1000)),
                  tabPanel("Scan", DT::dataTableOutput("scan", width = 1000)),
                  tabPanel("CMS", DT::dataTableOutput("cms", width = 1000)),
                  tabPanel("Callblock", DT::dataTableOutput("callblock", width = 1000)),
                  tabPanel("Phone Performance", DT::dataTableOutput("phone_performance", width = 1000)),
                  tabPanel("Secret box", DT::dataTableOutput("secret_box", width = 1000)),
                  tabPanel("Suggest", DT::dataTableOutput("suggest", width = 1000)),
                  tabPanel("Update Message", textOutput("update_info"))),
      width = 9),
    position = "left"
  )
)