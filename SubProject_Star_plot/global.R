if (!require(stringr)) install.packages("stringr")
if (!require(mailR)) install.packages("mailR")
if (!require(mailR)) install.packages("rmongodb")

### definition mongo server and connection
mongo_EC2 <- mongo.create(host = "54.148.221.90:27017", username = "yuyu", password = "roymond20*", db = "admin")
#mongo_EC2 <- mongo.create(host = "localhost:27017", username = "yuyu", password = "roymond20*", db = "admin")
mongo.is.connected(mongo_EC2) # connect to mongo server check

### show how many document in database and create a data table 
getDataFromMongodb <- function(data.list){
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
