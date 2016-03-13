library(shiny)
library(rmongodb)
library(dplyr)  
library(ggplot2)
library(reshape)
if (!require("devtools")) install.packages('devtools')
if (!require("rCharts")) install_github('rCharts', 'ramnathv')
if (!require("DT")) install.packages('DT')

mongo <- mongo.create(host = "//IP ADDRESS", username = "//ACCOUNT", password = "//PASSWORD", db = "admin") # connection to local server????
mongo.is.connected(mongo) # connect to mongo server check
db <- "GPDailyDataType"

### show how many document in database and create a data table 
get.gp.daily.data <- function(data.list){
  GP_DATA <- data.frame()
  GP_DATA <- (data.frame(number = 1:length(data.list$name),
                         name = as.character(data.list$name),
                         id = as.character(data.list$id),
                         star = as.character(data.list$star),
                         language = as.character(data.list$language),
                         software = as.character(data.list$software),
                         phone = iconv(as.character(data.list$phone), from = "UTF-8", to = "UTF-8"),
                         comment_time = as.character(data.list$comment_time),
                         comment = as.character(data.list$comment),
                         reply = as.character(data.list$reply),
                         reply_time = as.character(data.list$reply_time),
                         Tran_comment = as.character(data.list$Tran_comment),
                         star_no = as.character(data.list$star_no),
                         type = as.character(data.list$type),
                         GP_type = as.character(data.list$GP_type),
                         GP_type_2 = as.character(data.list$GP_type_2)))
  
  return(GP_DATA)
}




shinyServer(function(input, output) {
  
  date_range <- reactive({
    input$Date
  })
  
  get_data <- reactive({
    # enter date range to filter collection data
    dailydata_week <- data.frame()
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    #require_date <- seq.Date(from = as.Date("2015-08-07"), to = as.Date("2015-08-13"), by = "day")
    for(d in 1:length(require_date)){
      namespace <- paste(db, require_date[d], sep = ".")
      collection_record <- mongo.find.all(mongo, namespace, mongo.bson.empty())
      for(dd in 1:mongo.count(mongo, namespace)){
        dailydata <- get.gp.daily.data(collection_record[[dd]]) 
        dailydata_week <- rbind(dailydata_week, dailydata)
      }
    }
    dailydata_week <- mutate(dailydata_week, comment_date = substr(dailydata_week$comment_time, 1, 10))
    
  })
  
  
  
  output$chart1 <- renderChart({
    count_table <- get_data() %>% group_by(comment_date, GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    
    
    get_data<-subset(count_table,count_table$GP_type_2=='1_Applock')
    h1 <- hPlot(count ~ comment_date,type="line",group="type" , data = get_data)
    h1$tooltip(shared= T, crosshairs=T)
    h1$yAxis(title  = list(text = "Count"),min=0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1, 
                               dataLabels = list(enabled = TRUE, style = list(fontSize = "10px"))))
    h1$title(text = "1_Applock pros_cons_spam trend chart")
    h1$addParams(dom = 'chart1')
    return(h1)
  })
  output$chart2 <- renderChart({
    count_table <- get_data() %>% group_by(comment_date, GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    
    
    get_data<-subset(count_table,count_table$GP_type_2=='2_Scan')
    h1 <- hPlot(count ~ comment_date,type="line",group="type" , data = get_data)
    h1$tooltip(shared= T, crosshairs=T)
    h1$yAxis(title  = list(text = "Count"),min=0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1, 
                               dataLabels = list(enabled = TRUE, style = list(fontSize = "10px"))))
    h1$title(text = "2_Scan pros_cons_spam trend chart")
    h1$addParams(dom = 'chart2')
    return(h1)
    
  })
  
  output$chart3 <- renderChart({
    count_table <- get_data() %>% group_by(comment_date, GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    
    
    get_data<-subset(count_table,count_table$GP_type_2=='3_Private')
    h1 <- hPlot(count ~ comment_date,type="line",group="type" , data = get_data)
    h1$tooltip(shared= T, crosshairs=T)
    h1$yAxis(title  = list(text = "Count"),min=0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1, 
                               dataLabels = list(enabled = TRUE, style = list(fontSize = "10px"))))
    h1$title(text = "3_Private pros_cons_spam trend chart")
    h1$addParams(dom = 'chart3')
    return(h1)
  })
  output$chart11 <- renderChart({
    count_table <- get_data() %>% group_by(comment_date, GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    
    
    get_data<-subset(count_table,count_table$GP_type_2=='4_Junk')
    h1 <- hPlot(count ~ comment_date,type="line",group="type" , data = get_data)
    h1$tooltip(shared= T, crosshairs=T)
    h1$yAxis(title  = list(text = "Count"),min=0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1, 
                               dataLabels = list(enabled = TRUE, style = list(fontSize = "10px"))))
    h1$title(text = "4_Junk pros_cons_spam trend chart")
    h1$addParams(dom = 'chart11')
    return(h1)
  })
  output$chart12 <- renderChart({
    count_table <- get_data() %>% group_by(comment_date, GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    
    
    get_data<-subset(count_table,count_table$GP_type_2=='9_Wifi')
    h1 <- hPlot(count ~ comment_date,type="line",group="type" , data = get_data)
    h1$tooltip(shared= T, crosshairs=T)
    h1$yAxis(title  = list(text = "Count"),min=0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1, 
                               dataLabels = list(enabled = TRUE, style = list(fontSize = "10px"))))
    h1$title(text = "9_Wifi pros_cons_spam trend chart")
    h1$addParams(dom = 'chart12')
    return(h1)
  })
  output$chart13 <- renderChart({
    count_table <- get_data() %>% group_by(comment_date, GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    
    
    get_data<-subset(count_table,count_table$GP_type_2=='10_callblock')
    h1 <- hPlot(count ~ comment_date,type="line",group="type" , data = get_data)
    h1$tooltip(shared= T, crosshairs=T)
    h1$yAxis(title  = list(text = "Count"),min=0)
    h1$colors('#FF0000', '#50B432', '#3300FF')
    #h1$xAxis(angle=45)
    h1$plotOptions(line = list(lineWidth = 1, 
                               dataLabels = list(enabled = TRUE, style = list(fontSize = "10px"))))
    h1$title(text = "10_Callblock pros_cons_spam trend chart")
    h1$addParams(dom = 'chart13')
    return(h1)
  })
  output$chart4 <- renderChart({
    count_table <- get_data() %>% group_by(GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    get_data<-subset(count_table,count_table$GP_type_2=='2_Scan')
    h1 <- hPlot(x="type",y="count",type="pie", data = get_data)
    h1$plotOptions(pie = list(colors=c('#FF0000', '#50B432', '#3300FF'),dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "2_Scan")
    h1$chart("height"= 290,"width"= 290)
    h1$addParams(dom = 'chart4')
    return(h1)
    
  })
  output$chart5 <- renderChart({
    count_table <- get_data() %>% group_by(GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    get_data<-subset(count_table,count_table$GP_type_2=='1_Applock')
    h1 <- hPlot(x="type",y="count",type="pie", data = get_data)
    h1$plotOptions(pie = list(colors=c('#FF0000', '#50B432', '#3300FF'),dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "1_Applock")
    h1$chart("height"= 290,"width"= 290)
    h1$addParams(dom = 'chart5')
    return(h1)
    
  })
  output$chart6 <- renderChart({
    count_table <- get_data() %>% group_by(GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    get_data<-subset(count_table,count_table$GP_type_2=='10_callblock')
    h1 <- hPlot(x="type",y="count",type="pie", data = get_data)
    h1$plotOptions(pie = list(colors=c('#FF0000', '#50B432', '#3300FF'),dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "10_callblock")
    h1$chart("height"= 290,"width"= 290)
    h1$addParams(dom = 'chart6')
    return(h1)
    
  })
  output$chart7 <- renderChart({
    count_table <- get_data() %>% group_by(GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    get_data<-subset(count_table,count_table$GP_type_2=='3_Private')
    h1 <- hPlot(x="type",y="count",type="pie", data = get_data)
    h1$plotOptions(pie = list(colors=c('#FF0000', '#50B432', '#3300FF'),dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "3_Private")
    h1$chart("height"= 290,"width"= 290)
    h1$addParams(dom = 'chart7')
    return(h1)
    
  })
  output$chart8 <- renderChart({
    count_table <- get_data() %>% group_by(GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    get_data<-subset(count_table,count_table$GP_type_2=='4_Junk')
    h1 <- hPlot(x="type",y="count",type="pie", data = get_data)
    h1$plotOptions(pie = list(colors=c('#FF0000', '#50B432', '#3300FF'),dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "4_Junk")
    h1$chart("height"= 290,"width"= 290)
    h1$addParams(dom = 'chart8')
    return(h1)
    
  })
  output$chart9 <- renderChart({
    count_table <- get_data() %>% group_by(GP_type_2, type) %>% summarise(count = n())
    count_table <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    get_data<-subset(count_table,count_table$GP_type_2=='9_Wifi')
    h1 <- hPlot(x="type",y="count",type="pie", data = get_data)
    h1$plotOptions(pie = list(colors=c('#FF0000', '#50B432', '#3300FF'),dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "10px"))))
    h1$title(text = "9_Wifi")
    h1$chart("height"= 290,"width"= 290)
    h1$addParams(dom = 'chart9')
    return(h1)
    
  })
  
  output$chart10 <- renderChart({
    count_table <- get_data() %>% group_by(GP_type_2) %>% summarise(count = n())
    count_table3 <- count_table %>% filter(GP_type_2 != "6_Phone",  GP_type_2 != "7_Family", GP_type_2 != "8_Backup", GP_type_2 != "5_CMS")
    h1 <- hPlot(x="GP_type_2",y="count",type="pie", data = count_table3)
    h1$plotOptions(pie = list(dataLabels = list(enabled = TRUE,format= '<b>{point.name}</b>: {point.percentage:.1f} %', style = list(fontSize = "14px"))))
    h1$title(text = "All function feedback percentage")
    h1$chart("height"= 500,"width"= 500)
    h1$addParams(dom = 'chart10')
    return(h1)
    
  })
  output$mytable = renderDataTable({
    data<-subset(get_data(),get_data()$GP_type_2=='1_Applock'& type=='cons')
  })
  output$mytable2 = renderDataTable({
    data<-subset(get_data(),get_data()$GP_type_2=='2_Scan'& type=='cons')
  })
  output$mytable3 = renderDataTable({
    data<-subset(get_data(),get_data()$GP_type_2=='3_Private'& type=='cons')
  })
  output$mytable4 = renderDataTable({
    data<-subset(get_data(),get_data()$GP_type_2=='4_Junk'& type=='cons')
  })
  output$mytable5 = renderDataTable({
    data<-subset(get_data(),get_data()$GP_type_2=='9_Wifi'& type=='cons')
  })
  output$mytable6 = renderDataTable({
    data<-subset(get_data(),get_data()$GP_type_2=='10_callblock'& type=='cons')
  })
  
  output$plot <- renderPlot({
    
    datatest<-subset(get_data(),get_data()$GP_type_2=='1_Applock'& type=='cons')
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VectorSource(review_text)
    corpus <- Corpus(review_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    head(frequency)
    words <- names(frequency)
    
    h1<-wordcloud(words[1:20], frequency[1:20], random.order = F,scale=c(6,.5),rot.per=0, colors = brewer.pal(9,"Blues"))
    return(h1)
  })
  output$plot2 <- renderPlot({
    
    datatest<-subset(get_data(),get_data()$GP_type_2=='2_Scan'& type=='cons')
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VectorSource(review_text)
    corpus <- Corpus(review_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    head(frequency)
    words <- names(frequency)
    h1<-wordcloud(words[1:20], frequency[1:20], random.order = F,scale=c(6,.5),rot.per=0, colors = brewer.pal(9,"Blues"))
    
    return(h1)
  })
  output$plot3 <- renderPlot({
    
    datatest<-subset(get_data(),get_data()$GP_type_2=='3_Private'& type=='cons')
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VectorSource(review_text)
    corpus <- Corpus(review_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    head(frequency)
    words <- names(frequency)
    h1<-wordcloud(words[1:20], frequency[1:20], random.order = F,scale=c(6,.5),rot.per=0, colors = brewer.pal(9,"Blues"))
    return(h1)
  })
  output$plot4 <- renderPlot({
    
    datatest<-subset(get_data(),get_data()$GP_type_2=='4_Junk'& type=='cons')
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VectorSource(review_text)
    corpus <- Corpus(review_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    head(frequency)
    words <- names(frequency)
    h1<-wordcloud(words[1:20], frequency[1:20], random.order = F,scale=c(6,.5),rot.per=0, colors = brewer.pal(9,"Blues"))
    return(h1)
  })
  output$plot5 <- renderPlot({
    
    datatest<-subset(get_data(),get_data()$GP_type_2=='9_Wifi'& type=='cons')
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VectorSource(review_text)
    corpus <- Corpus(review_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    head(frequency)
    words <- names(frequency)
    h1<-wordcloud(words[1:20], frequency[1:20], random.order = F,scale=c(6,.5),rot.per=0, colors = brewer.pal(9,"Blues"))
    return(h1)
  })
  output$plot6 <- renderPlot({
    
    datatest<-subset(get_data(),get_data()$GP_type_2=='10_callblock'& type=='cons')
    review_text <- paste(datatest$Tran_comment, collapse=" ")
    review_source <- VectorSource(review_text)
    corpus <- Corpus(review_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    head(frequency)
    words <- names(frequency)
    h1<-wordcloud(words[1:20], frequency[1:20], random.order = F,scale=c(6,.5),rot.per=0, colors = brewer.pal(9,"Blues"))
    return(h1)
  })
})