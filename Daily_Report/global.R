### operation daily report - global.R
if (!require(stringr)) install.packages("stringr")
if (!require(mailR)) install.packages("mailR")
if (!require(rmongodb)) install.packages("rmongodb")

### definition mongo server and connection
#mongo_EC2 <- mongo.create(host = "//IP address", username = "//account", password = "//password", db = "admin")
db = "Operation_daily_report"
mongo_EC2 <- mongo.create(host = "localhost:27017", username = "//account", password = "//password", db = "admin")
#mongo.is.connected(mongo_EC2) # connect to mongo server check
#mongo <- mongo.create()
#mongo.is.connected(mongo)
mongodb_change <- mongo_EC2
### show how many document in database and create a data table - type one
getDataFromMongodb <- function(data.list){
  mongodb_data <- data.frame()
  mongodb_data <-  data.frame(Index = data.list$Index,
                              Date = as.Date.character(data.list$Date),
                              Subject = data.list$Subject,
                              User = data.list$User,
                              Version = data.list$Version,
                              Type = data.list$Type,
                              Android = data.list$Android,
                              Comment = data.list$Comment,
                              MCC = data.list$MCC,
                              Update_time = as.character(data.list$Update_time),
                              Category = data.list$Category,
                              stringsAsFactors = FALSE)
  return(mongodb_data)
}
# type two
getDataFromMongodb_suggest <- function(data.list){
  mongodb_data <- data.frame()
  mongodb_data <-  data.frame(Index = data.list$Index,
                              Function_type = data.list$Function_type,
                              Date = as.Date.character(data.list$Date),
                              Subject = data.list$Subject,
                              User = data.list$User,
                              Version = data.list$Version,
                              Type = data.list$Type,
                              Android = data.list$Android,
                              Comment = data.list$Comment,
                              MCC = data.list$MCC,
                              Update_time = as.character(data.list$Update_time),
                              Category = data.list$Category,
                              stringsAsFactors = FALSE)
  return(mongodb_data)
}
sheetName_defult <- c("Date", "Subject", "User", "Version", "Type", "Android", "Comment", "MCC")
sheetName_defult_suggest <- c("Function_type", "Date", "Subject", "User", "Version", "Type", "Android", "Comment", "MCC")
# update daily date to mongodb 
updateDataToMongodb <- function(file_path, category_names, n){
  namespace <- paste(db = "Operation_daily_report", category_names, sep = ".")
  input_data <- read.xlsx(file_path, sheetIndex = n, as.data.frame = TRUE, header = TRUE, stringsAsFactors = FALSE)
  if(length(which(is.na(input_data) != TRUE))!=0){
    if(category_names != "suggest"){
      if(length(which(colnames(input_data) %in% sheetName_defult == FALSE) !=0)) input_data <- input_data[, -which(colnames(input_data) %in% sheetName_defult == FALSE)]
      input_data <- input_data[which(is.na(input_data$Date) == FALSE), ]
      input_data[is.na(input_data)] <- "NoInfo"
      input_data$Date <- as.character(as.Date(input_data$Date))
      input_data$MCC <- as.character(input_data$MCC)
      input_data$Category <- category_names
      input_data$Update_time <- as.character(Sys.time())
      original_data_from_db <- mongo.find.all(mongo = mongodb_change, ns = namespace, query = mongo.bson.empty())
      original_data <- getDataFromMongodb(original_data_from_db[[1]]) 
      #original_data$Update_time <- Sys.time()
    }else{
      if(length(which(colnames(input_data) %in% sheetName_defult_suggest == FALSE)) !=0) input_data <- input_data[, -which(colnames(input_data) %in% sheetName_defult_suggest == FALSE)]
      input_data <- input_data[which(is.na(input_data$Date) == FALSE), ]
      input_data[is.na(input_data)] <- "NoInfo"
      input_data$Date <- as.character(as.Date(input_data$Date))
      input_data$MCC <- as.character(input_data$MCC)
      input_data$Category <- category_names
      input_data$Update_time <- as.character(Sys.time())
      original_data_from_db <- mongo.find.all(mongo = mongodb_change, ns = namespace, query = mongo.bson.empty())
      original_data <- getDataFromMongodb_suggest(original_data_from_db[[1]]) 
      #original_data$Update_time <- Sys.time()
    }
    input_data$Index <- (original_data$Index[dim(original_data)[1]]+1):(original_data$Index[dim(original_data)[1]]+dim(input_data)[1])
    update_data <- rbind(original_data, input_data) %>% .[rownames(unique(.[, c("Date", "Subject", "User")])), ]
    update_data$Date <- as.character(update_data$Date)
    update_data$Update_time <- as.character(update_data$Update_time)
    result_mongo <- mongo.update(mongo = mongodb_change, ns = namespace, criteria = mongo.bson.empty(), objNew = mongo.bson.from.list(update_data))
    #mongo.insert(mongo = mongodb_change, ns = namespace, b = mongo.bson.from.list(input_data)) 
  }
}

