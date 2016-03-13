## Operaction dashboard - ui.R
library(shiny)
library(shinydashboard, warn.conflicts = FALSE)
library(htmlwidgets)
library(rCharts)
library(shinythemes)
library(DT)
options(shiny.maxRequestSize=10*1024^2) 
## app.R 
header <- dashboardHeader(title = "Operation Dashoard",
                          dropdownMenuOutput("messageMenu"),
                          ## dropdown Menu for message
                          dropdownMenu(type = "notifications",
                                       notificationItem(status = "success",
                                                        #from = "Support", 
                                                        text = em(h4("If you have any question or problem"),
                                                                  h4("please contact below:"),
                                                                  h6("Email:raymond.liao@ileopard.com"),
                                                                  h6("Wechat: Yuyu Liao"),
                                                                  h6("Email:frank.tseng@ileopard.com"),
                                                                  h6("Wechat: Frank Tseng")),
                                                        icon = icon("support")
                                       ))
                          )
sidebar <- dashboardSidebar(## icon from http://fontawesome.io/icons/ just use icon name, 
  ## icon from http://getbootstrap.com/components/  use the last name after hyphen
  sidebarMenu(
    menuItem("Star Plot", tabName = "Star_Plot", icon = icon(name = "line-chart")),
    menuItem("GP Daily Plot", tabName = "gp_daily_plot", icon = icon(name = "pie-chart"), badgeLabel = "new", badgeColor = "green"),
    menuItem("OP Daily Report", tabName = "op_daily_report", icon = icon(name = "table")),
    menuItem("Mail Daily Report", tabName = "mail_daily_report", icon = icon("envelope-o")),
    menuItem("Reply File Upload", tabName = "auto_reply", icon = icon(name = "mail-reply-all")),
    menuItem("GP AutoReply Type", tabName = "gp_autoreply_type", icon = icon("list-ol")),
    menuItem("GP Platform", icon = icon("link"),  href = "http://mp.ijinshan.com/login/login"),
    tags$hr(), 
    selectizeInput("account_name", label = "Who you are?",
                              choices = c(Choose='', "Fox Liu" = "Fox.Liu", "Serene Zeng" = "Serene.Zeng", "Pandora Syu" = "Pandora.Syu", "Wing Ho" = "Wing.Ho"),
                              selected = NULL, options = list(placeholder = 'select name'), width = "250px"),
    passwordInput(inputId = "account_password", label = "Enter the password:", value = "", width = "250"),
    tags$head(
      tags$style(HTML('#submit_account{color:#FFFFFF; background-color:#000000; font-family: Georgia, "Times New Roman",
          Times, serif; margin: 13px; width:170px}'))
    ),
    actionButton(inputId = "submit_account", label = "Touch me", icon = icon(name = "thumbs-o-up", class = "fa-1x")),  
    #submitButton(text = "Touch me", icon = icon(name = "thumbs-o-up", class = "fa-1x"), width = validateCssUnit("200")),
    tags$hr(),
    tags$head(
      tags$style(HTML('#account{color:#000000; background-color:#FFFFFF; font-family: Georgia, "Times New Roman",
          Times, serif; margin: 13px; width:170px; font-size: 135%}'))
    ),
    verbatimTextOutput(outputId = "account")),
  width = 200
)
body <- dashboardBody(# Boxes need to be put in a row (or column)
  tabItems(
    #### Star plot page
    tabItem(tabName = "Star_Plot",
            fluidRow(
              box(title = "Date range", width = 4, height = "500px", status = "primary", solidHeader = TRUE,
                  br(),
                  dateRangeInput("input_date", label = NULL, start = Sys.Date()-7, end = Sys.Date()-1),
                  tags$head(
                    tags$style(HTML('#submit_Star_plot{color:#FFFFFF; background-color:#0066CC; width:125px}'))
                  ),
                  actionButton(inputId = "submit_Star_plot", label = "Sumbit"), 
                  #submitButton(text = "Submit", icon("refresh")),
                  br(),
                  helpText(h2(strong("Note:")),
                           p("The GP's Star data is update at 18:00 every day."),
                           p("The data are from", a("http://mp.ijinshan.com/")))
              ),
              box(title = "Star Plot", width = 8, height = "500px", status = "primary", solidHeader = TRUE, 
                  showOutput(outputId = "star_plots", "highcharts")),
              box(tile = "Data Table", width = 12, height = "450px", DT::dataTableOutput("star_data"))
            )),
    #### GP daily plot page
    tabItem(tabName = "gp_daily_plot",  
            fluidRow(
              box(title = "All function feedback percentage", status = "danger", solidHeader = TRUE, width = 8, height = 500,
                  showOutput("main_function_plot", "highcharts")),
              box(title = "Date range", status = "danger", solidHeader = TRUE, width = 4, height = 200,
                  dateRangeInput("input_date_gp", label = NULL, start = Sys.Date()-7, end = Sys.Date()-1, separator = "-"),
                  tags$head(
                    tags$style(HTML('#submit_gp_daily{color:#FFFFFF; background-color:#FF3333; width:125px}'))
                  ),
                  actionButton(inputId = "submit_gp_daily", label = "Submit")
                  #submitButton(text = "Submit", icon("refresh"))
                  )
            ),
            fluidRow(
              tabBox(title = NULL, side = "left", width = 12, height = 600, 
                     #ollapsible = TRUE, solidHeader = TRUE, status = "danger",
                     tabPanel(title = "Main Plots", 
                              column(4, showOutput("applock_plot", "highcharts")),
                              column(4, showOutput("scan_plot", "highcharts")),
                              column(4, showOutput("private_plot", "highcharts")),
                              column(4, showOutput("junk_plot", "highcharts")),
                              column(4, showOutput("callblock_plot", "highcharts"))),
                     tabPanel(title = 'Applock',
                              column(8, showOutput("applock_line_plot", "highcharts")),
                              column(4, plotOutput("apploc_wc"))),
                     tabPanel(title = 'Scan',
                              column(8, showOutput("scan_line_plot", "highcharts")),
                              column(4, plotOutput("scan_wc"))),
                     tabPanel(title = 'Private',
                              column(8, showOutput("private_line_plot", "highcharts")),
                              column(4, plotOutput("private_wc"))),
                     tabPanel(title = 'Junk',
                              column(8, showOutput("junk_line_plot", "highcharts")),
                              column(4, plotOutput("junk_wc"))),
                     tabPanel(title = 'Callblock',
                              column(8, showOutput("callblock_line_plot", "highcharts")),
                              column(4, plotOutput("callblock_wc")))
              )
            ),
            fluidRow(
              mainPanel(
                tabsetPanel(type = "tabs", 
                            tabPanel("Applock", DT::dataTableOutput("applock_dt", width = 1000)),
                            tabPanel("Scan", DT::dataTableOutput("scan_dt", width = 1000)),
                            tabPanel("Private", DT::dataTableOutput("private_dt", width = 1000)),
                            tabPanel("Junk", DT::dataTableOutput("junk_dt", width = 1000)),
                            tabPanel("Callblock", DT::dataTableOutput("callblock_dt", width = 1000))
                ))
            )
    ),
    #### GP and Mail Auto reply page
    tabItem(tabName = "auto_reply", 
            fluidRow(
              box(title = "GP Platform auto reply system", status = "info", solidHeader = TRUE, width = 4, height = 300,
                  #selectizeInput("select_op_name_gp", label = "Who you are?",
                  #               choices = c(Choose='', "Fox Liu" = "Fox.Liu", "Serene Zeng" = "Serene.Zeng", "Pandora Syu" = "Pandora.Syu", "Wing Ho" = "Wing.Ho"),
                  #               selected = NULL, options = list(placeholder = 'select name')),    		  
                  fileInput(inputId = "input_file", label = "Please upload GP-reply file", accept = c(".xlsx", ".csv"))),
                  #submitButton(text = "Submit")),
              box(title = "Upload GP-File response massage :", width = 8, height = 300,
                  verbatimTextOutput(outputId = "update_info_gp")),
              box(title = "Mail Platform auto reply system", status = "primary", solidHeader = TRUE, width = 4, height = "300px",
                  #selectizeInput("select_op_name_mail", label = "Who you are?",
                  #               choices = c(Choose='', "Fox Liu" = "Fox.Liu", "Serene Zeng" = "Serene.Zeng", "Pandora Syu" = "Pandora.Syu", "Wing Ho" = "Wing.Ho"),
                  #               selected = NULL, options = list(placeholder = 'select name')), 
                  fileInput(inputId = "input_file_mail", label = "Please upload Mail-reply file", accept = c(".xlsx", ".csv"))),
                  #submitButton(text = "Submit")),
              box(title = "Upload Mail-File response massage :", width = 8, height = "300px",
                  verbatimTextOutput(outputId = "update_info_mail"))
            )),
    #### OP daily report page
    tabItem(tabName = "op_daily_report",
            fluidRow(
              box(title = "Upload file", width = 3, height = "180px", status = "success", solidHeader = TRUE,
                  fileInput(inputId = "input_file_op", label = NULL, accept = c(".xlsx", ".csv"))
                  #tags$head(
                  #  tags$style(HTML('#Submit_op_daily_report_uploadfile{color:#FFFFFF; background-color:#000000; width:125px}'))
                  #),
                  #actionButton(inputId = "Submit_op_daily_report_uploadfile", label = "Submit")
                  ),
              box(title = "Upload File Message", verbatimTextOutput(outputId = "update_info_op"),
                  width = 3, height = "180px", status = "success", solidHeader = TRUE),
              box(title = "Date range", width = 3, height = "180px", status = "success", solidHeader = TRUE,
                  dateRangeInput("input_date_op", label = NULL, start = Sys.Date()-2, end = Sys.Date()-1, separator = "-"),
                  tags$head(
                    tags$style(HTML('#Submit_op_daily_report{color:#000000; background-color:#009900; width:125px}'))
                  ),
                  actionButton(inputId = "Submit_op_daily_report", label = "Submit")
                  #submitButton(text = "Submit", icon("refresh"))
                  ),
              box(title = "Choose the fucntion",
                  selectInput("dataset", label = NULL,
                              choices = c("applock", "callblock", "cms", "phone performance", "scan", "secret box", "suggest")),
                  downloadButton('downloadData', 'Download'),
                  width = 3, height = "180px", status = "success", solidHeader = TRUE)                  
            ),
            fluidRow(
              mainPanel(
                tabsetPanel(type = "tabs", 
                            tabPanel("Applock", DT::dataTableOutput("applock", width = validateCssUnit(1000))),
                            tabPanel("Scan", DT::dataTableOutput("scan", width = 1000)),
                            tabPanel("CMS", DT::dataTableOutput("cms", width = 1000)),
                            tabPanel("Callblock", DT::dataTableOutput("callblock", width = 1000)),
                            tabPanel("Private Browser", DT::dataTableOutput("private_browser", width = 1000)),
                            tabPanel("Phone Performance", DT::dataTableOutput("phone_performance", width = 1000)),
                            tabPanel("Secret box", DT::dataTableOutput("secret_box", width = 1000)),
                            tabPanel("Suggest", DT::dataTableOutput("suggest", width = 1000))),
                width = 12),
              position = "left"
            )),
    ### gp autoreply type
    tabItem(tabName = "gp_autoreply_type",
            fluidRow(
              box(title = "Enter Auto Reply Context:", width = 4, height = "250px", status = "info", solidHeader = TRUE,
                  textInput(inputId = "input_reply_type_name", label = "Type Name (Ex: enter 'typeP')", value = ""),
                  textInput(inputId = "input_reply_type_context", label = "Type Context (Ex: enter 'what ever you want')", value = "")),
              box(title = "Submit and Result", width = 4, height = "250px", status = "info", solidHeader = TRUE,
                  #selectizeInput("select_name_gp_type", label = "Who you are?",
                  #               choices = c(Choose='', "Fox Liu" = "Fox.Liu", "Serene Zeng" = "Serene.Zeng", "Pandora Syu" = "Pandora.Syu", "Wing Ho" = "Wing.Ho"),
                  #               selected = NULL, options = list(placeholder = 'Taiwan No.1')),
                  #passwordInput(inputId = "input_name_gp_type_password", label = "Enter your password:", value = "", width = validateCssUnit(400)),
                  tags$head(
                    tags$style(HTML('#Submit_gp_autoreply{color:#000000; background-color:#00CCFF; width:125px}'))
                  ),
                  actionButton(inputId = "Submit_gp_autoreply", label = "Submit"),
                  tags$hr(),
                  verbatimTextOutput(outputId = "edit_gp_type_data")
                  )
            ),
            fluidRow(
              box(
                #actionButton(inputId = "refresh_action", label = "Refresh Type Table", icon = icon(name = "refresh", class = "fa-1x")),
                title = "The Auto Reply List:", width = 12, hieght = "500", status = "info", solidHeader = TRUE,
                DT::dataTableOutput("output_reply_table_df"))
              #fluidRow(
              #  box(title = "testing", width = 6,
              #      verbatimTextOutput("output_edit_reply_type"))
              #)
            )),
    ### mail daily report
    tabItem(tabName = "mail_daily_report", 
            fluidRow(
              box(title = "Enter modify content", width = 4, height = "430px", status = "info", solidHeader = TRUE,
                  dateInput(inputId = "input_mail_date", labe = "Select Date:", value = Sys.Date()-1, min = "2015-12-14", format = "yyyy-mm-dd", width = validateCssUnit(400)),
                  textInput(inputId = "input_mail_index", label = "Enter Index:", value = NULL, width = validateCssUnit(400)),
                  textInput(inputId = "input_mail_owner", label = "Enter Owner:", value = NULL, width = validateCssUnit(400)),
                  textInput(inputId = "input_mail_track", label = "Enter Track:", value = NULL, width = validateCssUnit(400)),
                  textInput(inputId = "input_mail_status", label = "Enter Status:", value = NULL, width = validateCssUnit(400))),
              #box(title = NULL, width = 4, height = "430px", status = "info", solidHeader = FALSE,
              #    selectizeInput("select_name_mail", label = "Who you are?",
              #                             choices = c(Choose='', "Fox Liu" = "Fox.Liu", "Serene Zeng" = "Serene.Zeng", "Pandora Syu" = "Pandora.Syu", "Wing Ho" = "Wing.Ho"),
              #                             selected = NULL, options = list(placeholder = 'who is the most beautiful girl in the world?')),
              #    passwordInput(inputId = "input_name_mail_password", label = "Enter your password:", value = "", width = validateCssUnit(400))
                  #submitButton(text = "Touch me", icon = icon(name = "gavel", class = "fa-1x"), width = validateCssUnit("50%"))
              #    ),
              box(title = "Submit and Result", width = 4, height = "430px", status = "info", solidHeader = TRUE,
                  tags$head(
                    tags$style(HTML('#submit_mail_daily{color:#000000; background-color:#00CCFF; width:125px}'))
                  ),
                  actionButton(inputId = "submit_mail_daily", label = "Submit"),
                  tags$hr(),
                  verbatimTextOutput(outputId = "modifyMailPlatform_dt")),
              box(title = NULL, width = 12, height = "1000px", status = "info", solidHeader = FALSE,
                  DT::dataTableOutput("output_mail_table_df"))
            ))
  ))
ui <- dashboardPage(header = header, body = body, sidebar = sidebar, skin = "yellow")

#shinyApp(ui, server)

