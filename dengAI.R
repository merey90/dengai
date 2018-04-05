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
## Set functions
preprocessData <- function(df){
  df$week_start_date <- NULL
  df$precipitation_amt_mm <- NULL
  #df$year <- NULL
  
  df$reanalysis_air_temp_k <- df$reanalysis_air_temp_k - 273.15
  df$reanalysis_avg_temp_k <- df$reanalysis_avg_temp_k - 273.15
  df$reanalysis_dew_point_temp_k <- df$reanalysis_dew_point_temp_k - 273.15
  df$reanalysis_max_air_temp_k <- df$reanalysis_max_air_temp_k - 273.15
  df$reanalysis_min_air_temp_k <- df$reanalysis_min_air_temp_k - 273.15
  
  df$city <- as.factor(as.character(df$city))
  #df$weekofyear <- as.factor(as.character(df$weekofyear))
  
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

## Predict missing data
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
df.data <- df[c(1:1456),]
df.test <- df[-c(1:1456),]
rm(df, m)

sapply(df.data, function(x){sum(is.na(x))})

df.data <- sumVegetations(df.data)
df.test <- sumVegetations(df.test)
str(df.data)

## Test model
df.sj <- df.data[df.data$city == 'sj',]
df.iq <- df.data[df.data$city == 'iq',]
df.sj$city <- NULL
df.iq$city <- NULL
summary(df.sj)
index <- createDataPartition(df.sj$total_cases,p=0.75,list = FALSE)
df.data.train <- df.sj[index,]
df.data.test <- df.sj[-index,]

rf.train <- randomForest(total_cases~.,df.data.train,ntree=300,do.trace=TRUE)
pred.train <- predict(rf.train,df.data.test)

varImpPlot(rf.train)
df.data.test$pred <- pred.train
df.data.test$pred.round <- round(pred.train)
ggplot(df.data.test, aes(x = total_cases, y = pred)) + geom_point()
ggplot(df.data.train, aes(x = total_cases, y = weekofyear)) + geom_point()
MAE(df.data.test$total_cases,df.data.test$pred.round)

## Submit Data
rf.sj <- randomForest(total_cases~.,df.sj,ntree=300,do.trace=TRUE)
rf.iq <- randomForest(total_cases~.,df.iq,ntree=300,do.trace=TRUE)

pred.sj <- predict(rf.sj, df.test[df.test$city == 'sj',])
pred.iq <- predict(rf.iq, df.test[df.test$city == 'iq',])
str(df.submit)
df.submit[df.submit$city == 'sj',]$total_cases <- as.integer(round(pred.sj))
df.submit[df.submit$city == 'iq',]$total_cases <- as.integer(round(pred.iq))

write.csv(df.submit,"fin3.csv",row.names = FALSE)

#25.9736
