### Operation dashboard  - global.R
library(rmongodb)
library(mailR)
library(stringr)
library(xlsx)
library(readxl)
### definition mongo server and connection
mongo_EC2 <- mongo.create(host = "//IP ADDRESS", username = "//ACCOUNT", password = "//PASSWORD", db = "admin")
#mongo_EC2 <- mongo.create(host = "localhost:27017", username = "//ACCOUNT", password = "//PASSWORD", db = "admin")
mongo.is.connected(mongo_EC2) # connect to mongo server check
mongodb_change <- mongo_EC2
### change data structure from mongodb to R engine - star plot 
getDataFromMongodb_star <- function(data.list){
  mongodb_data <- data.frame()
  mongodb_data <-  data.frame(number = 1:length(data.list$Date),
                              Date = data.list$Date,
                              star_1 = data.list$star_1,
                              star_2 = data.list$star_2,
                              star_3 = data.list$star_3,
                              star_4 = data.list$star_4,
                              star_5 = data.list$star_5,
                              star_score = data.list$star_score,
                              stringsAsFactors = FALSE)
  return(mongodb_data)
}
### change data structure from mongodb to R engine - gp daily data plot 
getDataFromMongodb_gp_daily <- function(data.list){
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

### change data structure from mongodb to R engine - op daily report
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
#  change data structure from mongodb to R engine - op daily report suggest
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
## update daily date to mongodb - op daily report suggest
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
## change data structure from mongodb to R engine - mail daily report 
getDataFromMongodb_mail <- function(data.list){
  mongodb_data <- data.frame()
  mongodb_data <-  data.frame(Index = 1:length(data.list$Index),
                              Name = as.character(data.list$Name),
                              From = as.character(data.list$From),
                              Date = as.character(data.list$Date),
                              Title = as.character(data.list$Title),
                              Content = as.character(data.list$Content),
                              Tran_Content = as.character(data.list$Tran_Content),
                              Mail_type_2 = as.character(data.list$Mail_type_2),
                              Version = as.character(data.list$Version),
                              Type = as.character(data.list$Type),
                              Catch_Date = as.character(data.list$Catch_Date),
                              Owner = as.character(data.list$Owner),
                              Track = as.character(data.list$Track),
                              Status = as.character(data.list$Status),
                              stringsAsFactors = FALSE)
  return(mongodb_data)
}