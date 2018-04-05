library(data.table)
library(missForest)
library(randomForest)
library(dplyr)
library(ggplot2)
library(caret)
library(doParallel)

df.features <- fread("dengue_features_train.csv", na.strings = c("","NA","?", " ", NA, "NaN", "Inf", "<NA>"))
df.labels <- fread("dengue_labels_train.csv", na.strings = c("","NA","?", " ", NA, "NaN", "Inf", "<NA>"))
df.test.features <- fread("dengue_features_test.csv", na.strings = c("","NA","?", " ", NA, "NaN", "Inf", "<NA>"))
df.submit <- fread("submission_format.csv", na.strings = c("","NA","?", " ", NA, "NaN", "Inf", "<NA>"))

df.data <- inner_join(df.features,df.labels)
df.test <- inner_join(df.test.features,df.submit)

rm(df.features, df.labels, df.test.features)

#ggplot(df.data, aes(x = precipitation_amt_mm, y = reanalysis_sat_precip_amt_mm)) + geom_smooth()

preprocessData <- function(df){
  df$week_start_date <- NULL
  df$precipitation_amt_mm <- NULL
  df$year <- NULL
  
  df$reanalysis_air_temp_k <- df$reanalysis_air_temp_k - 273.15
  df$reanalysis_avg_temp_k <- df$reanalysis_avg_temp_k - 273.15
  df$reanalysis_dew_point_temp_k <- df$reanalysis_dew_point_temp_k - 273.15
  df$reanalysis_max_air_temp_k <- df$reanalysis_max_air_temp_k - 273.15
  df$reanalysis_min_air_temp_k <- df$reanalysis_min_air_temp_k - 273.15
  
  df$city <- as.factor(as.character(df$city))
  df$weekofyear <- as.factor(as.character(df$weekofyear))
  
  return(df)
}

sumVegetations <- function(df){
  df$ndvi <- df$ndvi_ne + df$ndvi_nw + df$ndvi_se + df$ndvi_sw
  df$ndvi_ne <- NULL
  df$ndvi_nw <- NULL 
  df$ndvi_se <- NULL
  df$ndvi_sw <- NULL
  return(df)
}


df.data <- preprocessData(df.data)
df.test <- preprocessData(df.test)

sapply(df.data, function(x){sum(is.na(x))})

hist(df.data$total_cases)

summary(df.data)
summary(df.test)

df <- rbind(df.data,df.test)
registerDoParallel(cores = 4)
m <- missForest(df, ntree = 100, parallelize = "forests")
df <- m$ximp
rm(df, m)
df.data <- df[c(1:1456),]
df.test <- df[-c(1:1456),]

sapply(df.data, function(x){sum(is.na(x))})

df.data <- sumVegetations(df.data)
df.test <- sumVegetations(df.test)
str(df.data)
#################
index <- createDataPartition(df.data$total_cases,p=0.75,list = FALSE)
df.data.train <- df.data[index,]
df.data.test <- df.data[-index,]

#table(train$yn)
#table(test$yn)

rf.train <- randomForest(total_cases~.,df.data.train,ntree=300,do.trace=TRUE)
pred <- predict(rf,df.data.test)

varImpPlot(rf)
df.data.test$pred <- pred
ggplot(df.data.test, aes(x = total_cases, y = pred)) + geom_bar()
roc.curve(df.data.test$total_cases,pred)
MAE(df.data.test$total_cases,pred)
####################################
rf <- randomForest(total_cases~.,df.data,ntree=300,do.trace=TRUE)
pred.final <- predict(rf,df.test)

df.submit$total_cases <- round(pred.final)

write.csv(df.submit,"fin.csv",row.names = FALSE)
#26.3413