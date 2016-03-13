## Operaction dashboard - server.R
options(warn = -1)
library(shiny)
library(shinydashboard, warn.conflicts = FALSE)
library(rmongodb)
library(mailR)
library(xlsx)
library(readxl)
library(dplyr)
library(tm)
library(wordcloud)
library(reshape2)
library(devtools)
library(rCharts)
library(DT)
library(data.table)
library(stringr)
mongo_EC2 <- mongo.create(host = "//IP ADDRESS", username = "//ACCOUNT", password = "//PASSWORD", db = "admin")
#mongo_EC2 <- mongo.create(host = "localhost:27017", username = "//ACCOUNT", password = "//PASSWORD", db = "admin")
#mongo.is.connected(mongo_EC2) # connect to mongo server check
mongodb_change <- mongo_EC2
#### account info
account_df <- data.frame(name = c("//USER_ACCOUNT"),
                         password = c("USER_PASSWORD"))
#### gp auto reply type
Logged = FALSE

#### server script
server <- function(input, output, session){
  ## account info
  accountIndex <- reactive({
    index <- which(input$account_name == account_df$name)
    return(index)
  })
  account_info <- eventReactive(input$submit_account,{
    if(input$account_name %in% account_df$name){
      if(input$account_password == account_df$password[accountIndex()]){
        account_name <- ifelse(length(input$account_name) != 0, input$account_name, "")
        account_password <- ifelse(length(input$account_password) !=0, input$account_password, "")
        updateSelectizeInput(session = session, inputId = "account_name", choices = c(Choose='', "XXXX" = "XXX.XX", "XXXX" = "XXX.XX", "ROYMOND LIAO" = "ROYMOND.LIAO"),
                             selected = NULL, options = list(placeholder = 'select name'))
        updateTextInput(session = session, inputId = "account_password", value = "")
        account_v <- c(account_name, account_password, TRUE)
        return(account_v)
      }else{ 
        account_v <- c("Wrong password, please enter again.", "", FALSE)
        return(account_v)
      }
    }
  })
  
  output$account <- renderPrint({
    try(
      if(account_info()[3] == TRUE){
          sprintf("User:%s, weclome!", account_info()[1])
      }else{
        account_info()[1]
      }
    )
  })
  ### Star_Plot
  date_range_star <- reactive({
    if(input$submit_Star_plot == 0){
      return(c(Sys.Date()-7, Sys.Date()-1))
    }else{
      input$input_date  
    }
  })
  get_data <- reactive({
    if(date_range_star()[2] > Sys.Date()){
      require_date <- seq.Date(from = date_range_star()[1], to = date_range_star()-1, by = "day")
    }else{
      # enter date range to filter collection data
      require_date <- seq.Date(from = date_range_star()[1], to = date_range_star()[2], by = "day")
    }
    # Initialization and get gp star data from mongodb "GPDailyStar"
    dailydata_week <- data.frame()
    for(d in 1:length(require_date)){
      namespace <- paste(db = "GP_Daily_Star", require_date[d], sep = ".")
      collection_record <- mongo.find.all(mongo = mongo_EC2, ns = namespace, mongo.bson.empty())
      for(dd in 1:mongo.count(mongo_EC2, namespace)){
        dailydata <- getDataFromMongodb_star(collection_record[[dd]]) 
        dailydata_week <- rbind(dailydata_week, dailydata)
      }
    }
    return(dailydata_week)
  })
  # output datetable or plot to web
  output$star_data <- DT::renderDataTable({
    datatable(get_data()[, -1], rownames = FALSE)
  })
  # OptionSetting : http://api.highcharts.com/highcharts 
  output$star_plots <- renderChart({
    input_df <- transform(get_data(), star_score = as.double(star_score))
    #h1 <- hPlot(x = "Date", y = "star_score", type = c("line"), data = input_df)
    h1 <- hPlot(star_score ~ Date, group = "number", data = input_df)
    h1$yAxis(title  = list(text = "Score"), min =  min(input_df$star_score)-0.1, max = max(input_df$star_score)+0.1, tickInterval = 0.1)
    h1$xAxis(title = list(text = "Date"), categories = input_df$Date, style = list(fontSize = "15px"))
    h1$title(text = "Google Play Star Score")
    h1$legend(enabled = F)
    h1$chart("height" = 425,"width" = 650)
    h1$plotOptions(line = list(lineWidth = 2, color = "#3300FF", 
                               dataLabels = list(enabled = TRUE, color = "#FF0000", style = list(fontSize = "18px"))))
    h1$addParams(dom = 'star_plots')
    return(h1)
  })
  #### gp_daily_plot
  date_range_gp <- reactive({
    if(input$submit_gp_daily == 0){
      return(c(Sys.Date()-7, Sys.Date()-1))
    }else{
      input$input_date_gp
    }
  })
  get_data_gp <- reactive({
    # enter date range to filter collection data
    db <- "GPDailyDataType"
    require_date <- seq.Date(from = date_range_gp()[1], to = date_range_gp()[2], by = "day")
    #require_date <- seq.Date(from = as.Date("2015-11-16"), to = as.Date("2015-11-23"), by = "day")
    dailydata_week <- data.frame()
    for(d in 1:length(require_date)){
      namespace <- paste(db, require_date[d], sep = ".")
      collection_record <- mongo.find.all(mongo = mongo_EC2, namespace, mongo.bson.empty())
      for(dd in 1:mongo.count(mongo = mongo_EC2, namespace)){
        dailydata <- getDataFromMongodb_gp_daily(collection_record[[dd]]) 
        dailydata_week <- rbind(dailydata_week, dailydata)
      }
    }
    dailydata_week <- mutate(dailydata_week, comment_date = substr(dailydata_week$comment_time, 1, 10))
    count_table <- dailydata_week %>% group_by(comment_date, GP_type_2, type) %>% summarise(count = n()) %>%
      filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    return(list(count_table, dailydata_week))
  })
  #### main pie plot and each function pie plot
  output$main_function_plot <- renderChart({
    count_table <- get_data_gp()[[1]] %>% group_by(GP_type_2) %>% summarise(count = sum(count))
    h1 <- hPlot(x = "GP_type_2", y = "count", type = "pie", data = count_table)
    h1$plotOptions(pie = list(dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "14px"))))
    #h1$title(text = "All function feedback percentage")
    h1$chart("height" = 400, "width" = 550)
    h1$addParams(dom = 'main_function_plot')
    return(h1)
  })
  output$applock_plot <- renderChart({
    count_table <- get_data_gp()[[1]] %>% group_by(GP_type_2, type) %>% summarise(count = n())
    get_data_table <- subset(count_table, count_table$GP_type_2 == '1_Applock')
    h1 <- hPlot(x = "type", y = "count", type = "pie", data = get_data_table)
    h1$plotOptions(pie = list(colors=c('#FF0000', '#50B432', '#3300FF'), dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "Applock")
    h1$chart("height" = 250, "width" = 300)
    h1$addParams(dom = 'applock_plot')
    return(h1)
  })
  output$scan_plot <- renderChart({
    count_table <- get_data_gp()[[1]] %>% group_by(GP_type_2, type) %>% summarise(count = n())
    get_data_table <- subset(count_table, count_table$GP_type_2 == '2_Scan')
    h1 <- hPlot(x = "type", y = "count", type = "pie", data = get_data_table)
    h1$plotOptions(pie = list(colors = c('#FF0000', '#50B432', '#3300FF'), dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "Scan")
    h1$chart("height" = 250, "width" = 300)
    h1$addParams(dom = 'scan_plot')
    return(h1)
  })
  output$private_plot <- renderChart({
    count_table <- get_data_gp()[[1]] %>% group_by(GP_type_2, type) %>% summarise(count = n())
    get_data_table <- subset(count_table, count_table$GP_type_2 == '3_Private')
    h1 <- hPlot(x = "type", y = "count", type = "pie", data = get_data_table)
    h1$plotOptions(pie = list(colors = c('#FF0000', '#50B432', '#3300FF'), dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "Private")
    h1$chart("height" = 250, "width" = 300)
    h1$addParams(dom = 'private_plot')
    return(h1)
  })
  output$junk_plot <- renderChart({
    count_table <- get_data_gp()[[1]] %>% group_by(GP_type_2, type) %>% summarise(count = n())
    get_data_table <- subset(count_table, count_table$GP_type_2 == '4_Junk')
    h1 <- hPlot(x = "type", y = "count", type = "pie", data = get_data_table)
    h1$plotOptions(pie = list(colors = c('#FF0000', '#50B432', '#3300FF'), dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "Junk")
    h1$chart("height" = 250, "width" = 300)
    h1$addParams(dom = 'junk_plot')
    return(h1)
  })
  output$wifi_plot <- renderChart({
    count_table <- get_data_gp()[[1]] %>% group_by(GP_type_2, type) %>% summarise(count = n())
    get_data_table <- subset(count_table, count_table$GP_type_2 == '9_Wifi')
    h1 <- hPlot(x = "type", y = "count", type = "pie", data = get_data_table)
    h1$plotOptions(pie = list(colors = c('#FF0000', '#50B432', '#3300FF'), dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "Wifi")
    h1$chart("height" = 250, "width" = 300)
    h1$addParams(dom = 'wifi_plot')
    return(h1)
  })
  output$callblock_plot <- renderChart({
    count_table <- get_data_gp()[[1]] %>% group_by(GP_type_2, type) %>% summarise(count = n())
    get_data_table <- subset(count_table, count_table$GP_type_2 == '10_callblock')
    h1 <- hPlot(x = "type", y = "count", type = "pie", data = get_data_table)
    h1$plotOptions(pie = list(colors = c('#FF0000', '#50B432', '#3300FF'), dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "Callblock")
    h1$chart("height" = 250, "width" = 300)
    h1$addParams(dom = 'callblock_plot')
    return(h1)
  })
  ####  every function line plots
  output$applock_line_plot <- renderChart({
    function_table <- get_data_gp()[[1]] %>% group_by(comment_date, GP_type_2, type) %>% summarise(Count = sum(count)) %>% filter(GP_type_2 == '1_Applock')
    h1 <- hPlot(Count ~ comment_date, type = "line", group = "type" , data = function_table)
    h1$tooltip(shared = T, crosshairs = T)
    h1$yAxis(title  = list(text = "Count"), min = 0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1.5, dataLabels = list(enabled = TRUE, style = list(fontSize = "15px"))))
    h1$title(text = "Category Type Trend Plot")
    h1$chart("height" = 425,"width" = 650)
    h1$addParams(dom = 'applock_line_plot')
    return(h1)
  })
  output$scan_line_plot <- renderChart({
    function_table <- get_data_gp()[[1]] %>% group_by(comment_date, GP_type_2, type) %>% summarise(Count = sum(count)) %>% filter(GP_type_2 == '2_Scan')
    h1 <- hPlot(Count ~ comment_date, type = "line", group = "type" , data = function_table)
    h1$tooltip(shared = T, crosshairs = T)
    h1$yAxis(title  = list(text = "Count"), min = 0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1.5, dataLabels = list(enabled = TRUE, style = list(fontSize = "15px"))))
    h1$title(text = "Category Type Trend Plot")
    h1$chart("height" = 450, "width" = 650)
    h1$addParams(dom = 'scan_line_plot')
    return(h1)
  })
  output$private_line_plot <- renderChart({
    function_table <- get_data_gp()[[1]] %>% group_by(comment_date, GP_type_2, type) %>% summarise(Count = sum(count)) %>% filter(GP_type_2 == '3_Private')
    h1 <- hPlot(Count ~ comment_date, type = "line", group = "type" , data = function_table)
    h1$tooltip(shared = T, crosshairs = T)
    h1$yAxis(title  = list(text = "Count"), min = 0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1.5, dataLabels = list(enabled = TRUE, style = list(fontSize = "15px"))))
    h1$title(text = "Category Type Trend Plot")
    h1$chart("height" = 450, "width" = 650)
    h1$addParams(dom = 'private_line_plot')
    return(h1)
  })
  output$junk_line_plot <- renderChart({
    function_table <- get_data_gp()[[1]] %>% group_by(comment_date, GP_type_2, type) %>% summarise(Count = sum(count)) %>% filter(GP_type_2 == '4_Junk')
    h1 <- hPlot(Count ~ comment_date, type = "line", group = "type" , data = function_table)
    h1$tooltip(shared = T, crosshairs = T)
    h1$yAxis(title  = list(text = "Count"), min = 0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1.5, dataLabels = list(enabled = TRUE, style = list(fontSize = "15px"))))
    h1$title(text = "Category Type Trend Plot")
    h1$chart("height" = 450, "width" = 650)
    h1$addParams(dom = 'junk_line_plot')
    return(h1)
  })
  output$callblock_line_plot <- renderChart({
    function_table <- get_data_gp()[[1]] %>% group_by(comment_date, GP_type_2, type) %>% summarise(Count = sum(count)) %>% filter(GP_type_2 == '10_callblock')
    h1 <- hPlot(Count ~ comment_date, type = "line", group = "type" , data = function_table)
    h1$tooltip(shared = T, crosshairs = T)
    h1$yAxis(title  = list(text = "Count"), min = 0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1.5, dataLabels = list(enabled = TRUE, style = list(fontSize = "15px"))))
    h1$title(text = "Category Type Trend Plot")
    h1$chart("height" = 450, "width" = 650)
    h1$addParams(dom = 'callblock_line_plot')
    return(h1)
  })
  
  #### every funciton Data Table
  dt.options <- list(dom = 'C<"clear">lfrtip',
                     colVis = list(exclude = c(0, 1), activate = 'mouseover'),
                     server = TRUE,
                     scrollY = 1000,
                     scrollCollapse = TRUE, 
                     pageLength = 20, autoWidth = TRUE, lengthMenu = c(10, 20, 30, 50, 100), searchHighlight = TRUE)
  output$applock_dt <- renderDataTable({
    data <- get_data_gp()[[2]] %>% filter(GP_type_2 == "1_Applock" & type == "cons") %>% select(name, star, language, software, phone, comment_time, comment, reply, reply_time, Tran_comment)
  }, options = dt.options)
  output$scan_dt <- renderDataTable({
    data <- get_data_gp()[[2]] %>% filter(GP_type_2 == "2_Scan" & type == "cons") %>% select(name, star, language, software, phone, comment_time, comment, reply, reply_time, Tran_comment)
  }, options = dt.options)
  output$private_dt <- renderDataTable({
    data <- get_data_gp()[[2]] %>% filter(GP_type_2 == "3_Private" & type == "cons") %>% select(name, star, language, software, phone, comment_time, comment, reply, reply_time, Tran_comment)
  }, options = dt.options)
  output$junk_dt <- renderDataTable({
    data <- get_data_gp()[[2]] %>% filter(GP_type_2 == "4_Junk" & type == "cons") %>% select(name, star, language, software, phone, comment_time, comment, reply, reply_time, Tran_comment)
  }, options = dt.options)
  output$callblock_dt <- renderDataTable({
    data <- get_data_gp()[[2]] %>% filter(GP_type_2 == "10_callblock" & type == "cons") %>% select(name, star, language, software, phone, comment_time, comment, reply, reply_time, Tran_comment)
  }, options = dt.options)
  #### every function wordcloud
  output$apploc_wc <- renderPlot({
    datatest <- get_data_gp()[[2]] %>% filter(GP_type_2 == "1_Applock" & type == "cons")
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VCorpus(VectorSource(review_text), readerControl = list(language = "en"))
    corpus <- tm_map(review_source, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm_m <- as.matrix(dtm)
    #frequency_range <- sort(dtm_m, decreasing = TRUE)
    #FreqWord <- findFreqTerms(dtm, lowfreq = 10, highfreq = frequency_range[1])
    #RColorBrewer::brewer.pal.info
    h1 <- wordcloud(colnames(dtm_m), dtm_m, random.order = FALSE, scale = c(6, 1), rot.per = 0, 
                    min.freq = 20, max.words = 100, colors = brewer.pal(12, "Paired"), strwidth(2))
    return(h1)
  })
  output$apploc_wc <- renderPlot({
    datatest <- get_data_gp()[[2]] %>% filter(GP_type_2 == "1_Applock" & type == "cons")
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VCorpus(VectorSource(review_text), readerControl = list(language = "en"))
    corpus <- tm_map(review_source, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm_m <- as.matrix(dtm)
    #frequency_range <- sort(dtm_m, decreasing = TRUE)
    #FreqWord <- findFreqTerms(dtm, lowfreq = 10, highfreq = frequency_range[1])
    #RColorBrewer::brewer.pal.info
    h1 <- wordcloud(colnames(dtm_m), dtm_m, random.order = FALSE, scale = c(6, 1), rot.per = 0, 
                    min.freq = 20, max.words = 100, colors = brewer.pal(12, "Paired"), strwidth(2))
    return(h1)
  })
  output$scan_wc <- renderPlot({
    datatest <- get_data_gp()[[2]] %>% filter(GP_type_2 == "2_Scan" & type == "cons")
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VCorpus(VectorSource(review_text), readerControl = list(language = "en"))
    corpus <- tm_map(review_source, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm_m <- as.matrix(dtm)
    #frequency_range <- sort(dtm_m, decreasing = TRUE)
    #FreqWord <- findFreqTerms(dtm, lowfreq = 10, highfreq = frequency_range[1])
    #RColorBrewer::brewer.pal.info
    h1 <- wordcloud(colnames(dtm_m), dtm_m, random.order = FALSE, scale = c(6, 1), rot.per = 0, 
                    min.freq = 20, max.words = 100, colors = brewer.pal(12, "Paired"), strwidth(2))
    return(h1)
  })
  output$private_wc <- renderPlot({
    datatest <- get_data_gp()[[2]] %>% filter(GP_type_2 == "3_Private" & type == "cons")
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VCorpus(VectorSource(review_text), readerControl = list(language = "en"))
    corpus <- tm_map(review_source, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm_m <- as.matrix(dtm)
    #frequency_range <- sort(dtm_m, decreasing = TRUE)
    #FreqWord <- findFreqTerms(dtm, lowfreq = 10, highfreq = frequency_range[1])
    #RColorBrewer::brewer.pal.info
    h1 <- wordcloud(colnames(dtm_m), dtm_m, random.order = FALSE, scale = c(6, 1), rot.per = 0, 
                    min.freq = 1, max.words = 100, colors = brewer.pal(12, "Paired"), strwidth(2))
    return(h1)
  })
  output$junk_wc <- renderPlot({
    datatest <- get_data_gp()[[2]] %>% filter(GP_type_2 == "4_Junk" & type == "cons")
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VCorpus(VectorSource(review_text), readerControl = list(language = "en"))
    corpus <- tm_map(review_source, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm_m <- as.matrix(dtm)
    #frequency_range <- sort(dtm_m, decreasing = TRUE)
    #FreqWord <- findFreqTerms(dtm, lowfreq = 10, highfreq = frequency_range[1])
    #RColorBrewer::brewer.pal.info
    h1 <- wordcloud(colnames(dtm_m), dtm_m, random.order = FALSE, scale = c(6, 1), rot.per = 0, 
                    min.freq = 10, max.words = 100, colors = brewer.pal(12, "Paired"), strwidth(2))
    return(h1)
  })
  output$callblock_wc <- renderPlot({
    datatest <- get_data_gp()[[2]] %>% filter(GP_type_2 == "10_callblock" & type == "cons")
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VCorpus(VectorSource(review_text), readerControl = list(language = "en"))
    corpus <- tm_map(review_source, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm_m <- as.matrix(dtm)
    #frequency_range <- sort(dtm_m, decreasing = TRUE)
    #FreqWord <- findFreqTerms(dtm, lowfreq = 10, highfreq = frequency_range[1])
    #RColorBrewer::brewer.pal.info
    h1 <- wordcloud(colnames(dtm_m), dtm_m, random.order = FALSE, scale = c(6, 1), rot.per = 0, 
                    min.freq = 10, max.words = 100, colors = brewer.pal(12, "Paired"))
    return(h1)
  })
  #### op_daily_report
  date_range_op <- reactive({
    if(input$Submit_op_daily_report == 0){
      return(c(Sys.Date()-2, Sys.Date()-1))
    }else{
      input$input_date_op
    }
  })
  dt.options <- list(dom = 'C<"clear">lfrtip',
                     colVis = list(exclude = c(0, 1), activate = 'mouseover'),
                     server = TRUE,
                     scrollX = 200,
                     scrollY = 1000,
                     scrollCollapse = TRUE, 
                     pageLength = 20, autoWidth = TRUE, lengthMenu = c(10, 20, 30, 50, 100), searchHighlight = TRUE)
  # applock data
  applock_data <- reactive({  
    db <- "Operation_daily_report"
    require_date <- seq.Date(from = date_range_op()[1], to = date_range_op()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "applock", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$applock <- DT::renderDataTable({return(applock_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # callblock data
  callblock_data <- reactive({  
    db <- "Operation_daily_report"
    require_date <- seq.Date(from = date_range_op()[1], to = date_range_op()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "callblock", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$callblock <- DT::renderDataTable({return(callblock_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # cms data
  cms_data <- reactive({  
    db <- "Operation_daily_report"
    require_date <- seq.Date(from = date_range_op()[1], to = date_range_op()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "cms", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$cms <- DT::renderDataTable({return(cms_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  #Private Browser data
  Private_Browser_data <- reactive({  
    db <- "Operation_daily_report"
    require_date <- seq.Date(from = date_range_op()[1], to = date_range_op()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "Private_Browser", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$private_browser <- DT::renderDataTable({return(Private_Browser_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  
  # phone_performance data
  phone_performance_data <- reactive({  
    db <- "Operation_daily_report"
    require_date <- seq.Date(from = date_range_op()[1], to = date_range_op()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "phone_performance", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$phone_performance <- DT::renderDataTable({return(phone_performance_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # scan data
  scan_data <- reactive({  
    db <- "Operation_daily_report"
    require_date <- seq.Date(from = date_range_op()[1], to = date_range_op()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "scan", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$scan <- DT::renderDataTable({return(scan_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # secret box data
  secret_box_data <- reactive({  
    db <- "Operation_daily_report"
    require_date <- seq.Date(from = date_range_op()[1], to = date_range_op()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "secret_box", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$secret_box <- DT::renderDataTable({return(secret_box_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  # suggest
  suggest_data <- reactive({  
    db <- "Operation_daily_report"
    require_date <- seq.Date(from = date_range_op()[1], to = date_range_op()[2], by = "day")
    output_data <- mongo.find.all(mongo = mongodb_change, ns = paste(db, "suggest", sep = "."), query = mongo.bson.empty()) %>% .[[1]] %>% 
      getDataFromMongodb_suggest() %>% .[.$Date %in% require_date, -c(dim(.)[2])]
    return(output_data)
  })
  output$suggest <- DT::renderDataTable({return(suggest_data())}, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = dt.options) 
  
  # update new data
  output$update_info_op <- renderPrint({
    if(account_info()[3] == TRUE){
      inFile <- input$input_file_op
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
      }
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
  
  ##### GP & Mail Auto Reply
  ## gp platform autoreply 
  output$update_info_gp <- renderPrint({
    #operation_staff_names <- c("Fox.Liu", "Serene.Zeng", "Pandora.Syu", "Wing.Ho")
    #if(input$select_op_name_gp %in% operation_staff_names){
    if(account_info()[3] == TRUE){
      inFile <- input$input_file
      if(!is.null(inFile)){
        ext <- tools::file_ext(inFile$name) # Filename Extension 
        file.rename(inFile$datapath, 
                    paste(inFile$datapath, ".xlsx", sep=""))
        sheet_names <- excel_sheets(paste(inFile$datapath, ".xlsx", sep=""))
        file_path <- paste(inFile$datapath, ".xlsx", sep="")
        file_names <- str_split(file_path, "/")[[1]][length(str_split(file_path, "/")[[1]])]
        tryCatch({ 
          for(n in 1:length(sheet_names)){
            # excel_data <- read_excel(paste(inFile$datapath, ext, sep="."), sheet_index, col_names = TRUE)   
            namespace <- paste(db = "GP_Platform_AutoReply", sprintf("GP-%s", Sys.Date()), sep = ".")
            input_data <- read.xlsx(file_path, sheetIndex = n, as.data.frame = TRUE, header = TRUE, stringsAsFactors = FALSE)
            reply_data <- input_data %>% select(id, language, reply, Tran_comment, GP_type_2, SS_category_type)
            reply_data$id <- as.character(reply_data$id) 
            reply_data$reply <- as.character(reply_data$reply) 
            reply_data <- na.omit(reply_data)
            insert_result <- mongo.insert(mongodb_change, ns = namespace, b = mongo.bson.from.df(reply_data))
          }
          print("Import data down, We will reply to gp's users soon!!")
          print(sprintf("Upload Time : %s, Person:%s",  as.POSIXlt(Sys.time(), "Asia/Taipei"), account_info()[1]))
          sender <- "//YOUR EMAIL" # Replace with a valid address
          recipients <- c("//THE EMAIL YOU WANT TO SEND")
          send.mail(from = sender,to = recipients,subject = "Upload GP reply files record",
                    body = sprintf("Upload Time : %s, Person:%s",  as.POSIXlt(Sys.time(), "Asia/Taipei"), account_info()[1]),
                    #attach.files = "D:/R_code/R_exe/MainFunction(Download_by_Total).Rout",
                    smtp = list(host.name = "aspmx.l.google.com", port = 25))
        },error = function(err){
          error <- paste("GP_Platform_AutoReply web error message:",err)
          sender <- "//YOUR EMAIL" # Replace with a valid address
          recipients <- c("//THE EMAIL YOU WANT TO SEND") # Replace with one or more valid addresses
          send.mail(from = sender,to = recipients,subject = "GP_Platform_AutoReply web error from AWS",
                    body = error,
                    #attach.files = "D:/R_code/R_exe/MainFunction(Download_by_Total).Rout",
                    smtp = list(host.name = "aspmx.l.google.com", port = 25))
        })
      } 
    }
  })
  ## mail platform autoreply
  output$update_info_mail <- renderPrint({
    #operation_staff_names <- c("Fox.Liu", "Serene.Zeng", "Pandora.Syu", "Wing.Ho")
    if(account_info()[3] == TRUE){
      inFile_mail <- input$input_file_mail
      if(!is.null(inFile_mail)){
        ext <- tools::file_ext(inFile_mail$name) # Filename Extension 
        file.rename(inFile_mail$datapath, paste(inFile_mail$datapath, ".xlsx", sep=""))
        sheet_names <- excel_sheets(paste(inFile_mail$datapath, ".xlsx", sep=""))
        file_path <- paste(inFile_mail$datapath, ".xlsx", sep="")
        file_names <- str_split(file_path, "/")[[1]][length(str_split(file_path, "/")[[1]])]
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
          print("Import data down, We will reply to user's mail soon!!")
          print(sprintf("Upload Time : %s, Person:%s", as.POSIXlt(Sys.time(), "Asia/Taipei"), account_info()[1]))
          sender <- "//YOUR EMAIL" # Replace with a valid address
          recipients <- c("//THE EMAIL YOU WANT TO SEND")
          send.mail(from = sender,to = recipients,subject = "Upload Mail reply files record",
                    body = sprintf("Upload Time : %s, Person:%s",  as.POSIXlt(Sys.time(), "Asia/Taipei"), account_info()[1]),
                    #attach.files = "D:/R_code/R_exe/MainFunction(Download_by_Total).Rout",
                    smtp = list(host.name = "aspmx.l.google.com", port = 25))
        },error = function(err){
          error <- paste("GP_Pltform_AutoReply(Mail) web error message:",err)
          sender <- "//YOUR EMAIL" # Replace with a valid address
          recipients <- c("//THE EMAIL YOU WANT TO SEND", "//THE EMAIL YOU WANT TO SEND") # Replace with one or more valid addresses
          send.mail(from = sender,to = recipients,subject = "GP_Platform_AutoReply(Mail) web error from AWS",
                    body = error,
                    #attach.files = "D:/R_code/R_exe/MainFunction(Download_by_Total).Rout",
                    smtp = list(host.name = "aspmx.l.google.com", port = 25))
        })
      }
    }
  })
  
  #### gp_autoreply_type
  output$edit_gp_type_data <- eventReactive(input$Submit_gp_autoreply, {
    db <-  "GP_Platform_AutoReply_Type"
    namespace <- paste(db, "reply_multi_type", sep = ".")
    reply_df <- mongo.find.all(mongo = mongo_EC2, ns = namespace, query = mongo.bson.empty(), data.frame = TRUE) %>% .[, -1]
    colnames(reply_df) <- colnames(reply_df) %>% str_extract("[t].+")
    if(account_info()[3] == TRUE){
        new_reply_data <- data.frame(type_name = input$input_reply_type_context, stringsAsFactors = FALSE)
        colnames(new_reply_data) <- input$input_reply_type_name
        if(length(which(colnames(new_reply_data) %in% colnames(reply_df))) == 1){
          reply_df[, which(colnames(reply_df) %in% colnames(new_reply_data))] <- new_reply_data[1, ]
        }else{
          reply_df <- cbind(reply_df, new_reply_data)
        }
        updateTextInput(session = session, inputId = "input_reply_type_name", label = NULL, value = "")
        updateTextInput(session = session, inputId = "input_reply_type_context", label = NULL, value = "")
        result_mongo <- mongo.update(mongo = mongodb_change, ns = namespace, criteria = mongo.bson.empty(), objNew = mongo.bson.from.df(reply_df))
        if(result_mongo){
          print("Edit successfully!")
        }else{
          print("Sorry edit unsuccessfully, please try again")
        }
    }else{
        print("Sorry, you may don't login account!! Please login first.") 
    }
  })
  output$output_reply_table_df <- renderDataTable({
      db <-  "GP_Platform_AutoReply_Type"
      namespace <- paste(db, "reply_multi_type", sep = ".")
      reply_df <- mongo.find.all(mongo = mongo_EC2, ns = namespace, query = mongo.bson.empty(), data.frame = TRUE)
      reply_long_df <- melt(reply_df, variable.name = "type", value.name = "context", factorsAsStrings = FALSE, id = 1)
      reply_long_df$type <- reply_long_df$type %>% str_extract("[t].+")
      return(reply_long_df[, c("type", "context")]) 
  }, server = FALSE, extensions = "ColVis", rownames = FALSE)
  
  #### mail_daily_report
  mail.dt.options <- list(dom = 'C<"clear">lfrtip',
                     colVis = list(exclude = c(0, 1), activate = 'mouseover'),
                     server = TRUE,
                     scrollX = 100,
                     scrollY = 800,
                     scrollCollapse = TRUE, 
                     pageLength = 20, autoWidth = TRUE, lengthMenu = c(10, 20, 30, 50, 100), searchHighlight = TRUE)

  mail_platform_df <- reactive({
    db <- "Mail_Platform_DailyData"
    namespace <- paste(db, input$input_mail_date, sep = ".")
    mail_df <- mongo.find.all(mongo = mongo_EC2, ns = namespace, query = mongo.bson.empty())
    mail_dt <- getDataFromMongodb_mail(mail_df[[1]]) %>% data.table() %>% .[Type =="cons"] %>% 
      .[, .(Index, From, Title, Version, Tran_Content, Type, Catch_Date, Owner, Track, Status)] 
    return(mail_dt)
  })

  output$modifyMailPlatform_dt <- renderPrint({
    db <- "Mail_Platform_DailyData"
    namespace <- paste(db, input$input_mail_date, sep = ".")
    modify_df <- mongo.find.all(mongo = mongo_EC2, ns = namespace, query = mongo.bson.empty())
    modify_dt <- getDataFromMongodb_mail(modify_df[[1]]) %>% data.table(key = "Index") %>% .[Type =="cons"] 
    if(account_info()[3] == TRUE & input$submit_mail_daily == 1){
     modify_id <- input$input_mail_index
     modify_dt[.(as.integer(modify_id))]$Owner  <- input$input_mail_owner
     modify_dt[.(as.integer(modify_id))]$Track  <- input$input_mail_track
     modify_dt[.(as.integer(modify_id))]$Status  <- input$input_mail_status
     result_mongo <- mongo.update(mongo = mongodb_change, ns = namespace, criteria = mongo.bson.empty(), objNew = mongo.bson.from.list(modify_dt))
     updateTextInput(session = session, inputId = "input_mail_index",value = "")
     updateTextInput(session = session, inputId = "input_mail_owner",value = "")
     updateTextInput(session = session, inputId = "input_mail_track",value = "")
     updateTextInput(session = session, inputId = "input_mail_status",value = "")
     updateTextInput(session = session, inputId = "input_name_mail_password",value = "")
     print(sprintf("You are fucking to edit the raw data, wow!! It's awesome. Status : %s", result_mongo))
    }else{
     print("You don't do anything!! Borning, you shuck.")
    }
  })
  output$output_mail_table_df <- DT::renderDataTable({
    mail_platform_df()
  }, server = FALSE, extensions = "ColVis", rownames = FALSE, filter = "top", options = mail.dt.options)
}

