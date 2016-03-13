## GP_Platform_AutoReply global.R
library(rmongodb)
library(readxl)
library(xlsx)
library(stringr)
library(dplyr)

mongo_EC2 <- mongo.create(host = "//IP address", username = "//ACCOUNT", password = "//PASSWORD", db = "admin")
db <- "GP_Platform_AutoReply_Mail" #TestDataBase
# mongo_EC2 <- mongo.create(host = "localhost:27017", username = "//ACCOUNT", password = "//PASSWORD", db = "admin")
# mongo.is.connected(mongo_EC2) # connect to mongo server check
mongodb_change <- mongo_EC2
# create database
# mongo.drop(mongo = mongo_EC2, ns = namespace) # drop collections
# mongo.drop.database(mongo = mongo_EC2, db) # drop collections
# mongo.remove(mongo, ns, criteria = mongo.bson.empty())
# mongo.get.database.collections(mongo_EC2, db)
file_path <- "~/Downloads/Mail_1118_scan.xlsx" 
sheet_names <- excel_sheets(file_path)
namespace <- paste(db, sprintf("Mail-%s", Sys.Date()), sep = ".")

for(n in 1:length(sheet_names)){
  fuck_mail <- read.xlsx(file_path, sheetName = sheet_names[n], as.data.frame = TRUE, header = FALSE, stringsAsFactors = FALSE)
  colnames(fuck_mail) <- c("mail")
  if(fuck_mail[1, 1] == "Text"){
    fuck_mail <- fuck_mail[-1, ] 
    fuck_mail <- data.frame(mail = na.omit(fuck_mail[, 1]))
  }else{
    fuck_mail <- data.frame(mail = na.omit(fuck_mail[, 1]))
  }
  fuck_mail$mail <- as.character(fuck_mail$mail)
  fuck_mail$category <- as.character(sheet_names[n])
  insert_result <- mongo.insert(mongodb_change, ns = namespace, b = mongo.bson.from.list(fuck_mail))
}




