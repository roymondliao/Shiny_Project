# operation daily report - server.R
library(shiny)
library(XLConnect)
library(readxl)
library(DT)
library(rmongodb)
library(xlsx)
library(dplyr)
library(mailR)
#mongodb_change <- mongo
db <- "Operation_daily_report"
options(shiny.maxRequestSize=10*1024^2) 
shinyServer(function(input, output){
  date_range <- reactive({
    return(input$input_date)
  })
  dt.options <- list(dom = 'C<"clear">lfrtip',
                     colVis = list(exclude = c(0, 1), activate = 'mouseover'),
                     server = TRUE,
                     scrollY = 1000,
                     scrollCollapse = TRUE, 
                     pageLength = 20, autoWidth = TRUE, lengthMenu = c(10, 20, 30, 50, 100), searchHighlight = TRUE)
  # applock data
  applock_data <- reactive({  
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "applock", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$applock <- DT::renderDataTable({return(applock_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # callblock data
  callblock_data <- reactive({  
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "callblock", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$callblock <- DT::renderDataTable({return(callblock_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # cms data
  cms_data <- reactive({  
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "cms", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$cms <- DT::renderDataTable({return(cms_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  #Private Browser data
  Private_Browser_data <- reactive({  
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "Private_Browser", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$Private_Browser <- DT::renderDataTable({return(Private_Browser_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  
  # phone_performance data
  phone_performance_data <- reactive({  
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "phone_performance", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$phone_performance <- DT::renderDataTable({return(phone_performance_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # scan data
  scan_data <- reactive({  
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "scan", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$scan <- DT::renderDataTable({return(scan_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # secret box data
  secret_box_data <- reactive({  
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "secret_box", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$secret_box <- DT::renderDataTable({return(secret_box_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # suggest
  suggest_data <- reactive({  
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "suggest", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb_suggest() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$suggest <- DT::renderDataTable({return(suggest_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  
  # update new data
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
          if(sheet_names[n] == "Applock"){
            updateDataToMongodb(file_path = file_path, category_names = "applock", n = n)
            
          }else if(sheet_names[n] == "查殺隱私"){
            updateDataToMongodb(file_path = file_path, category_names = "scan", n = n)
            
          }else if(sheet_names[n] == "CMS"){
            updateDataToMongodb(file_path = file_path, category_names = "cms", n = n)
            
          }else if(sheet_names[n] == "CallBlock"){
            updateDataToMongodb(file_path = file_path, category_names = "callblock", n = n)
            
          }else if(sheet_names[n] == "Private Browser"){
            updateDataToMongodb(file_path = file_path, category_names = "Private_Browser", n = n)  
            
          }else if(sheet_names[n] == "手機效能"){
            updateDataToMongodb(file_path = file_path, category_names = "phone_performance", n = n)
            
          }else if(sheet_names[n] == "秘密盒子"){
            updateDataToMongodb(file_path = file_path, category_names = "secret_box", n = n)
            
          }else if(sheet_names[n] == "建議"){
            updateDataToMongodb(file_path = file_path, category_names = "suggest", n = n)     
          }
        }
        print("Update Data Down!!")
      },error = function(err){
        error <- paste("Operation daily report web error message:",err)
        sender <- "//YOUR EMAIL" # Replace with a valid address
        recipients <- c("//THE EMAIL YOU WANT TO SEND", "//THE EMAIL YOU WANT TO SEND") # Replace with one or more valid addresses
        send.mail(from = sender,to = recipients,subject = "Operation daily report web error from AWS",
                  body = error,
                  #attach.files = "D:/R_code/R_exe/MainFunction(Download_by_Total).Rout",
                  smtp = list(host.name = "aspmx.l.google.com", port = 25))
      })
      
      #wb <-loadWorkbook(inFile$datapath) 
      #sheets <- getSheets(wb)
    }
  })
  # download data
  datasetInput <- reactive({
    switch(input$dataset,
           "applock" = applock_data()[input$applock_rows_all, ],
           "callblock" = callblcok_data()[input$callblcok_rows_all, ], 
           "cms" =  cms_data()[input$cms_rows_all, ],
           "Private_Browser" =  cms_data()[input$Private_Browser_rows_all, ],
           "phone performance" = phone_performance_data()[input$phone_performance_rows_all, ],
           "scan" =  scan_data()[input$scan_rows_all, ],
           "secret box" = secret_box_data()[input$secret_box_rows_all, ],
           "suggest" = suggest_data()[input$suggest_rows_all, ])
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(sprintf("Daily_Report_%s", Sys.Date()),".xlsx", sep = "") },
    content = function(file) {
      write.xlsx(datasetInput(), file, sheetName = "sheet1", row.names = FALSE)
    }
  )
  #outputOptions(output, "sheet_name", suspendWhenHidden = FALSE)
})
