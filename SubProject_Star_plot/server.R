options(warn = -1)
library(shiny)
if (!require("devtools")) install.packages('devtools')
if (!require("rCharts")) install_github('rCharts', 'ramnathv')
if (!require("DT")) install.packages('DT')

shinyServer(function(input, output){
  # input date range from web
  date_range <- reactive({
      input$input_date
    })
  
  get_data <- reactive({
    # enter date range to filter collection data
    require_date <- seq.Date(from = date_range()[1], to = date_range()[2], by = "day")
    # Initialization and get gp star data from mongodb "GPDailyStar"
    dailydata_week <- data.frame()
    for(d in 1:length(require_date)){
      namespace <- paste(db = "GPDailyStar", require_date[d], sep = ".")
      collection_record <- mongo.find.all(mongo = mongo_EC2, ns = namespace, mongo.bson.empty())
      for(dd in 1:mongo.count(mongo_EC2, namespace)){
        dailydata <- getDataFromMongodb(collection_record[[dd]]) 
        dailydata_week <- rbind(dailydata_week, dailydata)
      }
    }
    dailydata_week
  })
  # output datetable or plot to web
  output$dates <- DT::renderDataTable({
    datatable(get_data()[, -1], rownames = FALSE)
  })
  # OptionSetting : http://api.highcharts.com/highcharts 
  output$plots <- renderChart({
    input_df <- transform(get_data(), star_score = as.double(star_score))
    #h1 <- hPlot(x = "Date", y = "star_score", type = c("line"), data = input_df)
    h1 <- hPlot(star_score ~ Date, group = "number", data = input_df)
    h1$yAxis(title  = list(text = "Score"), min =  min(input_df$star_score)-0.1, max = max(input_df$star_score)+0.1, tickInterval = 0.1)
    h1$xAxis(title = list(text = "Date"), categories = input_df$Date, style = list(fontSize = "15px"))
    h1$title(text = "Google Play Star Score")
    h1$legend(enabled = T)
    h1$plotOptions(line = list(lineWidth = 2, color = "#3300FF", 
                               dataLabels = list(enabled = TRUE, color = "#FF0000", style = list(fontSize = "18px"))))
    h1$addParams(dom = 'plots')
    return(h1)
  })
})