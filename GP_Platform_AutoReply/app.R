## GP_Platform_AutoReply app.R
library(shiny)
library(rmongodb)
library(mailR)
library(xlsx)
library(readxl)
library(dplyr)
mongo_EC2 <- mongo.create(host = "", username = "", password = "", db = "")
# mongo.is.connected(mongo_EC2) # connect to mongo server check
mongodb_change <- mongo_EC2
options(shiny.maxRequestSize=10*1024^2) 
shinyApp(
  ui = fluidPage(
    titlePanel("GP Platform auto reply system"),
    sidebarPanel(
      fileInput(inputId = "input_file", label = "Please upload GP-reply file", accept = c(".xlsx", ".csv")),
      br(),
      br(),
      fileInput(inputId = "input_file_mail", label = "Please upload Mail-reply file", accept = c(".xlsx", ".csv"))
    ),
    mainPanel(
      h2("Upload GP-file message:", align = "left"),
      verbatimTextOutput(outputId = "update_info"),
      br(),
      h2("Upload Mail-file message:", align = "left"),
      verbatimTextOutput(outputId = "update_info_mail")
    )
  ),
  server = function(input, output){
  output$update_info <- renderPrint({
    inFile <- input$input_file
      if(!is.null(inFile)){
        ext <- tools::file_ext(inFile$name) # Filename Extension 
        file.rename(inFile$datapath, 
                    paste(inFile$datapath, ".xlsx", sep=""))
        sheet_names <- excel_sheets(paste(inFile$datapath, ".xlsx", sep=""))
        file_path <- paste(inFile$datapath, ".xlsx", sep="")
        tryCatch({ 
          for(n in 1:length(sheet_names)){
            # excel_data <- read_excel(paste(inFile$datapath, ext, sep="."), sheet_index, col_names = TRUE)   
            namespace <- paste(db = "GP_Platform_AutoReply", sprintf("GP-%s", Sys.Date()), sep = ".")
            input_data <- read.xlsx(file_path, sheetIndex = n, as.data.frame = TRUE, header = TRUE, stringsAsFactors = FALSE)
            reply_data <- input_data %>% select(id, language, reply)
            reply_data$id <- as.character(reply_data$id) 
            reply_data$reply <- as.character(reply_data$reply) 
            reply_data <- na.omit(reply_data)
            insert_result <- mongo.insert(mongodb_change, ns = namespace, b = mongo.bson.from.df(reply_data))
          }
          print("Import data down, We will reply to gp's users soon!!")
        },error = function(err){
          error <- paste("GP_Platform_AutoReply web error message:",err)
          sender <- "raymond.liao@ileopard.com" # Replace with a valid address
          recipients <- c("raymond.liao@ileopard.com") # Replace with one or more valid addresses
          send.mail(from = sender,to = recipients,subject = "GP_Platform_AutoReply web error from AWS",
                    body = error,
                    #attach.files = "D:/R_code/R_exe/MainFunction(Download_by_Total).Rout",
                    smtp = list(host.name = "aspmx.l.google.com", port = 25))
        })
      } 
    })
  output$update_info_mail <- renderPrint({
    inFile_mail <- input$input_file_mail
    if(!is.null(inFile_mail)){
      ext <- tools::file_ext(inFile_mail$name) # Filename Extension 
      file.rename(inFile_mail$datapath, paste(inFile_mail$datapath, ".xlsx", sep=""))
      sheet_names <- excel_sheets(paste(inFile_mail$datapath, ".xlsx", sep=""))
      #d_index <- which(sheet_names %in% "Index")
      #if(length(d_index) != 0){
      #  sheet_names <- sheet_names[-d_index]
      #  d <- 2
      #}else{
      #  d <- 1
      #}
      file_path <- paste(inFile_mail$datapath, ".xlsx", sep="")
      tryCatch({ 
        namespace <- paste(db = "GP_Platform_AutoReply_Mail", sprintf("Mail-%s", Sys.Date()), sep = ".")
        for(n in 1:length(sheet_names)){
          if(sheet_names[n] != "Index"){
            fuck_mail <- read.xlsx(file_path, sheetName = sheet_names[n], as.data.frame = TRUE, header = FALSE, stringsAsFactors = FALSE, colIndex = 1)
            fuck_mail <- data.frame(fuck_mail)
            colnames(fuck_mail) <- c("mail")
            if(length(grep("@", fuck_mail$mail)) !=0){
              if(fuck_mail[1, 1] == "Text"){
                fuck_mail <- fuck_mail[-1, ] 
                fuck_mail <- data.frame(mail = na.omit(fuck_mail))
              }else{
                fuck_mail <- data.frame(mail = na.omit(fuck_mail[, 1]))
              }
              fuck_mail$mail <- as.character(fuck_mail$mail)
              fuck_mail$category <- as.character(sheet_names[n])
              insert_result <- mongo.insert(mongodb_change, ns = namespace, b = mongo.bson.from.list(fuck_mail)) 
            }
          }
        }
        print("Import data down, We will reply to gp's users soon!!") 
      },error = function(err){
        error <- paste("GP_Platform_AutoReply(Mail) web error message:",err)
        sender <- "raymond.liao@ileopard.com" # Replace with a valid address
        recipients <- c("raymond.liao@ileopard.com") # Replace with one or more valid addresses
        send.mail(from = sender,to = recipients,subject = "GP_Platform_AutoReply(Mail) web error from AWS",
                  body = error,
                  #attach.files = "D:/R_code/R_exe/MainFunction(Download_by_Total).Rout",
                  smtp = list(host.name = "aspmx.l.google.com", port = 25))
      })
    } 
  })
  }
)

