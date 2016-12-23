#################################################
# This is the code for EECS6893-BigDataAnalytics#
# Author: Haowei Wu                             #
#################################################

# ****Contents*****
# combine the .csv files collected by Python code
# process data with SparkR and R
# write up functions for SHINY APP


# collect data from download .csv files

whole1 <- data.frame()
for (i in c(1:11)){
  path <- sprintf("~/Big_data_analytics_project/bda_proj_%d_%d",50*(i-1),50*i)
  setwd(path)
  file.list <- dir()
  idx <- grep("weather_data.csv", file.list)
  file.list <- file.list[idx]
  for (j in 1:length(file.list)){
    this.file <- read.csv(file = file.list[j])
    if (nrow(this.file)==0){break}
    for (k in c(4:14)){
      this.file[,k] <- gsub("-|T", NA, this.file[,k])
    }
    station <- strsplit(file.list[j], split = "_")[[1]][1]
    city <- strsplit(file.list[j], split = "_")[[1]][2]
    state <- strsplit(file.list[j], split = "_")[[1]][3]
    this.file <- data.frame(this.file, station, city, state)
    whole1 <- rbind.data.frame(whole1, this.file)
    
  }
  print(paste0(i/11*100, "%"))
}


whole2 <- data.frame()
for (i in c(23:32)){
  path <- sprintf("~/Big_data_analytics_project/bda_proj_%d_%d",50*(i-1),50*i)
  setwd(path)
  file.list <- dir()
  idx <- grep("weather_data.csv", file.list)
  file.list <- file.list[idx]
  for (j in 1:length(file.list)){
    this.file <- read.csv(file = file.list[j])
    if (nrow(this.file)==0){break}
    for (k in c(4:14)){
      this.file[,k] <- gsub("-|T", NA, this.file[,k])
    }
    station <- strsplit(file.list[j], split = "_")[[1]][1]
    city <- strsplit(file.list[j], split = "_")[[1]][2]
    state <- strsplit(file.list[j], split = "_")[[1]][3]
    this.file <- data.frame(this.file, station, city, state)
    whole2 <- rbind.data.frame(whole2, this.file)
    
  }
  print(paste0(i/32*100, "%"))
}

whole <- rbind.data.frame(whole1, whole2)
setwd("~/Big_data_analytics_project")
write.csv(whole, file = "whole.csv", row.names = FALSE)

#store <- whole

# drop factor levels
for (i in c(4:14)){
  whole[,i] <- as.numeric(whole[,i])
}
weather <- na.omit(whole)

# Previously, we want to use Spark for PCA and Clustering
# However, the MLlib of SparkR only support GLM
# Here we do PCA and Clustering using R
# we also tested K-means Clustering using pyspark
# Do Clustering for large scale dataset is challenging in R
# Clara algorithm is used to get clustering result


# try low memory clustering
library(cluster)
# import dataset and clean NAs
whole <- read.csv(file = "whole.csv")
test <- whole[complete.cases(whole),]

# scale the data and do pca to reduce the number of varirable
my_pca <- test
my_pca[,c(4:14)] <- scale(my_pca[,c(4:14)])
pca_model <- princomp(my_pca[,c(4:14)])
pca_model$sdev
sdev_sum <- vector()

# 6 PCS are enough for clustering
for (i in 1:11){
  sdev_sum[i] <- sum(pca_model$sdev[1:i])
}
plot(c(1:11),sdev_sum/sum(pca_model$sdev), type = 'b', main = "Principle Component Selection", xlab = "Number of PCs" ,ylab = "variance explained", ylim = c(0,1))
abline(v=6, col = 'red')

# 6 PCs are fine

my_pca$pc1 <- as.matrix(my_pca[,c(4:14)]) %*% pca_model$loadings[,1]
my_pca$pc2 <- as.matrix(my_pca[,c(4:14)]) %*% pca_model$loadings[,2]
my_pca$pc3 <- as.matrix(my_pca[,c(4:14)]) %*% pca_model$loadings[,3]
my_pca$pc4 <- as.matrix(my_pca[,c(4:14)]) %*% pca_model$loadings[,4]
my_pca$pc5 <- as.matrix(my_pca[,c(4:14)]) %*% pca_model$loadings[,5]
my_pca$pc6 <- as.matrix(my_pca[,c(4:14)]) %*% pca_model$loadings[,6]

############################################################
# export file for PySpark
spark_pca <- my_pca[,c("pc1","pc2","pc3","pc4","pc5","pc6")]
rownames(spark_pca) <- c(0:(nrow(spark_pca)-1))
spark_pca$pc1 <- paste0("1:",spark_pca$pc1)
spark_pca$pc2 <- paste0("2:",spark_pca$pc2)
spark_pca$pc3 <- paste0("3:",spark_pca$pc3)
spark_pca$pc4 <- paste0("4:",spark_pca$pc4)
spark_pca$pc5 <- paste0("5:",spark_pca$pc5)
spark_pca$pc6 <- paste0("6:",spark_pca$pc6)
write.table(spark_pca, file = "spark_pca.txt", sep = " ", row.names = TRUE, col.names = FALSE, quote=FALSE)
############################################################

# clustering
ptm <- proc.time()
clara_model = clara(my_pca[,c("pc1","pc2","pc3","pc4","pc5","pc6")], k=10, metric = "euclidean", pamLike = TRUE, samples = 50, sampsize = 10000)
proc.time() - ptm

my_pca$clustering <- clara_model$clustering

write.csv(my_pca, file = "my_pca.csv", row.names = FALSE)



# create psuedo user information
# userID, state, city, date, temperature, humidity, wind, rate
# 1000 users, 10~20 ratings per user, 1~3 times in traveling

set.seed(10086)

# ID maker funciton
idmaker <- function(x){
  max.val = x*100
  count <- nchar(as.character(max.val))
  size <- paste("%0",count,"d",sep = "")
  lets <- toupper(sample(letters,x,replace = T))
  nums <- sprintf(size,sample(1:max.val))[1:x]
  ids <- paste(lets,nums,sep = "")
  return(ids)
}

# create userID
users <- idmaker(1000)
# randomly assing living city
pseudo_data <- my_pca[,c("Month","Day","Year","station","city","state","weather_type")]
city.list <- as.character(unique(pseudo_data$city))

pseudo_record <- data.frame()
for (i in 1:length(users)){
  record.length <- sample(c(10:20),1)
  travel.length <- sample(c(1:3),1)
  living.length <- record.length - travel.length
  
  living.city <- sample(city.list,1)
  travel.city <- sample(city.list,3)
  
  living_set <- pseudo_data[which(pseudo_data$city==living.city),]
  travel_set <- pseudo_data[which(pseudo_data$city %in% travel.city),]
  
  if (living.length>nrow(living_set)){living.length <- nrow(living_set)}
  living.record <- data.frame(living_set[sample(c(1:nrow(living_set)),living.length),], rating = sample(c(1:5),living.length, replace = TRUE), status = "living")
  travel.record <- data.frame(travel_set[sample(c(1:nrow(travel_set)),travel.length),], rating = sample(c(1,5),travel.length, replace = TRUE), status = "traveling")
  
  this.record <- rbind.data.frame(living.record,travel.record)
  this.record$userID <- users[i]
  pseudo_record <- rbind.data.frame(pseudo_record, this.record)
  print(paste0(i/length(users)*100,"%"))
}


# Recommendation: use slope-one algorithm to fill user's rating matrix
rate_matrix <- data.frame()
# calculate average rate
for (i in 1:length(users)){
  this.user <- pseudo_record[which(pseudo_record$userID==users[i]),]
  rate <- vector()
  for (j in 1:10){
    if (!is.nan(mean(this.user$rating[which(this.user$weather_type==j)]))){
      this.rate <- data.frame(userID = users[i], weather_type = j, rate = mean(this.user$rating[which(this.user$weather_type==j)]))
      rate_matrix <- rbind.data.frame(rate_matrix, this.rate)
    }
  }
}

# Slope-One Algorithm
user.list <- unique(rate_matrix$userID)
diff.matrix <- matrix(rep(NA,100), nrow=10)
for (i in 1:9){
  for (j in (i+1):10){
    # search user with both i and j
    this.diff <- 0
    n_user <- 0
    for (k in 1:length(user.list)){
      this.user <- rate_matrix[which(rate_matrix$userID==user.list[k]),]
      this.rate <- unique(this.user$weather_type)
      if (i %in% this.rate & j %in% this.rate){
        this.diff <- this.diff + (this.user$rate[which(this.user$weather_type==i)] - this.user$rate[which(this.user$weather_type==j)])
        n_user <- n_user + 1
      }
    }
    if (n_user>0){diff.matrix[i,j]<-this.diff/n_user}
  }
}

# flag: user rated/predicted
rate_matrix$is_self <- 1
whole_fill <- data.frame()
for (i in 1:length(user.list)){
  this.user <- rate_matrix[which(rate_matrix$userID==user.list[i]),]
  rate.self <- unique(this.user$weather_type)
  rate.recommend <- setdiff(c(1:10), rate.self)
  if (length(rate.recommend)>0){
    for (j in 1:length(rate.recommend)){
      this.type <- rate.recommend[j]
      n <- 0
      recommend <- 0
      for (k in rate.self[which(rate.self<this.type)]){
        if (!is.na(diff.matrix[k,this.type])){
          recommend <- recommend + this.user$rate[which(this.user$weather_type==k)] + diff.matrix[k,this.type]
          n <- n + 1
        }
      }
      this.fill <- data.frame(userID = user.list[i], weather_type = this.type, rate = recommend/n, is_self = 0)
      whole_fill <- rbind.data.frame(whole_fill, this.fill)
    }

  }
}

full_rate <- rbind.data.frame(rate_matrix, whole_fill)

user_data <- arrange(full_rate, userID, weather_type)
#


# save data for SHINY APP
setwd("~/Big_data_analytics_project")
# weather
weather <- pseudo_data
weather$month_day <- weather$Month * 100 + weather$Day
save(weather, file = "weather.RData")
# user data
save(user_data, file = "user_data.RData")
# pseudo record
save(pseudo_record, file = "pseudo_record.RData")




######################################################


##### SHINY APP query functions #####

# input: state, date.begin, date.end
library("lubridate")
weather <- pseudo_data
weather$month_day <- weather$Month * 100 + weather$Day
library(plyr)

weather_query <- function(userID, go_state, date.begin, date.end){
  
  weather_state <- weather[which(weather$state %in% go_state),]
  date_begin <- month(strptime(date.begin,format = "%Y-%m-%d")) * 100 + day(strptime(date.begin,format = "%Y-%m-%d"))
  date_end <- month(strptime(date.end,format = "%Y-%m-%d")) * 100 + day(strptime(date.end,format = "%Y-%m-%d"))
  if (date_begin <= date_end){
    weather_date <- weather_state[which(weather_state$month_day>=date_begin & weather_state$month_day<=date_end),]
  }else{
    weather_date <- weather_state[which((weather_state$month_day>=date_begin & weather_state$month_day<=1231)|(weather_state$month_day>=101 & weather_state$month_day<=date_end)),]
  }
  this.user <- user_data[which(user_data$userID==userID),]
  weather_date <- merge(weather_date, this.user[,c("weather_type","rate")], by = "weather_type")
  my_count <- aggregate(weather_date$rate, list(weather_date$station), mean)
  names(my_count) <- c("station", "score")
  my_count <- my_count[order(my_count$score, decreasing = TRUE),]
  rownames(my_count) <- NULL
  return(my_count)
}


this.query <- weather_query("J05033","AK", "2009-10-01", "2010-03-01")

#get user preference
user_query <- function(userID){
  this.user <- user_data[which(user_data$userID==userID),]
  rownames(this.user) <- NULL
  return(this.user)
}

this.user <- user_query("J05033")
barplot(this.user$rate, col = (this.user$is_self+2), ylim = c(0,5))


weatehr_type_count <- function(station, date.begin, date.end){
  this.city <- weather[which(weather$station==station),]
  date_begin <- month(strptime(date.begin,format = "%Y-%m-%d")) * 100 + day(strptime(date.begin,format = "%Y-%m-%d"))
  date_end <- month(strptime(date.end,format = "%Y-%m-%d")) * 100 + day(strptime(date.end,format = "%Y-%m-%d"))
  if (date_begin <= date_end){
    weather_date <- weather_state[which(weather_state$month_day>=date_begin & weather_state$month_day<=date_end),]
  }else{
    weather_date <- weather_state[which((weather_state$month_day>=date_begin & weather_state$month_day<=1231)|(weather_state$month_day>=101 & weather_state$month_day<=date_end)),]
  }
  my_count <- ddply(weather_date, .(weather_type),nrow)
  create_table <- data.frame(weather_type = c(1:10))
  create_table <- merge(create_table, my_count, by = "weather_type", all.x = TRUE)
  names(create_table)[2] <- "Occurence"
  create_table$Occurence <- create_table$Occurence / sum(create_table$Occurence, na.rm = TRUE)
  return(create_table)
}

best_city <- as.character(this.query$station[1])
best <- weatehr_type_count(best_city,"2009-10-01", "2010-03-01")
barplot(best$Occurence)

bad_city <- as.character(this.query$station[20])
bad <- weatehr_type_count(bad_city,"2009-10-01", "2010-03-01")
barplot(best$Occurence)


# user analysis

user_analysis <- function(userID){
  this.user <- user_data[which(user_data$userID==userID),]
  # preference plot this barplot

  p<-ggplot(data=this.user, aes(x=weather_type, y=rate)) +
    ggtitle(sprintf("%s's Weather Taste", userID)) +
    geom_bar(stat="identity", fill=c("#999999", "#56B4E9")[this.user$is_self+1]) +
    geom_text(aes(label=round(rate,1)), vjust=1.6, color="white", size=3.5) +
    ylim(0,5) + 
    theme_minimal()
  p
  
  
  # records  <- show this table
  this.record <- pseudo_record[which(pseudo_record$userID==userID),]
  this.record <- arrange(this.record, Year, Month, Day)
  
}






############################################################################


# Running SparkR in RStudio
# set the system environment viariables
Sys.setenv(SPARK_HOME = "/usr/local/Cellar/spark-2.0.1-bin-hadoop2.7")
# set the library path
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
# load the Sparkr library
install.packages("SparkR")

library(sparklyr)
sc <- spark_connect(master = "local")
library(dplyr)


weather$month_day <- weather$Month *100 + weather$Day
my_state <- c("NY")
date.begin = "2016-12-25"
date.end = "2017-01-15"

weather_query <- function(my_state, date.begin, date.end){
  # pass dataframe to spark
  weather_tbl <- copy_to(sc, my_weather, overwrite = TRUE)
  # filter the state
  if (length(my_state)==1){
    weather_state <- weather_tbl %>% filter(state == my_state)
  }else{
    weather_state <- weather_tbl %>% filter(state %in% my_state)
  }
  # deternmine the date_begin and date_end
  date_begin <- month(strptime(date.begin,format = "%Y-%m-%d")) * 100 + day(strptime(date.begin,format = "%Y-%m-%d"))
  date_end <- month(strptime(date.end,format = "%Y-%m-%d")) * 100 + day(strptime(date.end,format = "%Y-%m-%d"))
  if (date_begin <= date_end){
    weather_date <- weather_state %>% filter(month_day>=date_begin & month_day<=date_end)
  }else{
    weather_date <- weather_state %>% filter((month_day>=date_begin & month_day<=1231)| (month_day<=date_end & month_day>=101))
  }
  return(weather_date)
  
}

############################################################################


# PCA in spark
weather_tbl <- copy_to(sc, weather, overwrite = TRUE)

pca_model <- tbl(sc, 'whole') %>%
  select(Mean_Temperature,Max_Temperature,Min_Temperature,Dew_Point,Average_Humidity,
         Maximum_Humidity,Minimum_Humidity,Wind_Speed,Max_Wind_Speed,Max_Gust_Speed) %>%
  ml_pca()

print(pca_model)
pca_model$explained.variance

weather_tbl$pc1 <- weather_tbl[4:14] %*% pca_model$components[,c(1:2)]


library(DBI)
weather_view <- dbGetQuery(sc, "SELECT * FROM weather LIMIT 10")
weather_view

dbGetQuery(sc, "ADD pc1 FLOAT")




