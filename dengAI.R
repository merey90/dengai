library(data.table)
#library(missForest)
library(randomForest)
library(dplyr)
library(ggplot2)
#library(doParallel)

df.features <- fread("dengue_features_train.csv", na.strings = c("","NA","?", " ", NA, "NaN", "Inf", "<NA>"))
df.labels <- fread("dengue_labels_train.csv", na.strings = c("","NA","?", " ", NA, "NaN", "Inf", "<NA>"))
df.test.features <- fread("dengue_features_test.csv", na.strings = c("","NA","?", " ", NA, "NaN", "Inf", "<NA>"))
df.submit <- fread("submission_format.csv", na.strings = c("","NA","?", " ", NA, "NaN", "Inf", "<NA>"))
# 273.15
rm(df.features, df.labels, df.test.features)

df.data <- inner_join(df.features,df.labels)
df.test <- inner_join(df.test.features,df.submit)
#MAE()
sapply(df.data, function(x){sum(is.na(x))})
sapply(df.test, function(x){sum(is.na(x))})

hist(df.data$total_cases)

ggplot(df.data, aes(x = precipitation_amt_mm, y = reanalysis_sat_precip_amt_mm)) + geom_smooth()
summary(train)
summary(test)

df.data$week_start_date <- NULL
df.test$week_start_date <- NULL

train$city <- as.factor(train$city)
test$city <- as.factor(test$city)
df <- rbind(train,test)
str(train)
registerDoParallel(cores = 4)
m <- missForest(df, ntree = 100, parallelize = "forests")
m_train <- missForest(train, ntree = 1, parallelize = "forests")
m_test <- missForest(train, ntree = 1, parallelize = "forests")

df <- m$ximp

train <- df[c(1:1456),]
test <- df[-c(1:1456),]

table(test$total_cases)

#################

library(caret)
index <- createDataPartition(train$total_cases,p=0.8,list = FALSE)
df_train <- train[index,]
df_test <- train[-index,]

#table(train$yn)
#table(test$yn)

rf <- randomForest(total_cases~.,df_train,ntree=300,do.trace=TRUE)
pred <- predict(rf,df_test)

varImpPlot(rf)

library(ROSE)
roc.curve(df_test$total_cases,pred)

####################################


pred_test <- predict(rf,test)

df0 <- df4
df0$total_cases <- round(pred_test)

write.csv(df0,"~/resources/rstudio/fin.csv",row.names = FALSE)