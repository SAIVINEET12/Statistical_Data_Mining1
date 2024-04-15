#library(pls)
#library(e1071)
#library(Metrics)
#library(hydroGOF)
#library(kernlab)
#library(caret)
#library(class)
#library(randomForest)
#library("neuralnet")

data <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/END/eas508_exam2.csv",header = TRUE)
data <- data[,!colnames(data) %in% c('Feature2','Feature3','Feature14','Feature19','Feature5','Feature6','Feature4','Feature12')]
data <- as.data.frame(scale(data))
set.seed(123)
target <- data[,1]
splitIndex=sample(nrow(data),nrow(data)*0.80)
train<-data[splitIndex,]
test<-data[-splitIndex,]
descriptors_train<-train[,-1]
descriptors_test<-test[,-1]
prop_train<-train[,1]
prop_test<-test[,1]

#MLR
model_lm<-lm(train$Property1~.,train)
pred_lm_train<-predict(model_lm,descriptors_train)
pred_lm_test<-predict(model_lm,descriptors_test)
#cf_train <- confusionMatrix(as.factor(pred_lm_train),as.factor(train$Property1))
#cf_test <- confusionMatrix(as.factor(pred_lm_test),as.factor(prop_test))
rmse_lm<-rmse(test$Property1,pred_lm_test)
r2_lm_train <- R2(train$Property1,pred_lm_train,form="traditional")
r2_lm_test <- R2(test$Property1,pred_lm_test,form="traditional")
r2_lm_train
r2_lm_test

###RandomForest
model2 <- randomForest(train$Property1 ~ ., data = train, ntree = 500, mtry = 6, importance = TRUE)
predTrain <- predict(model2, train, type = "class")
predTest <- predict(model2, test, type = "class")
r2_rf_train <- R2(train$Property1, predTrain, form = "traditional")
r2_rf_test <- R2(test$Property1, predTest, form = "traditional")
r2_rf_train
r2_rf_test
#cf_test_rf=confusionMatrix(as.factor(predTest), as.factor(test$Property1))


#NN
trainBd <- train
testBd <- test
medianVal <- median(data$Property1)
trainBd$Property1 <- ifelse(trainBd$Property1 > medianVal, 1, 0)
testBd$Property1 <- ifelse(testBd$Property1 > medianVal, 1, 0)
nn_dats <- neuralnet(Property1~., data = trainBd,hidden=3,err.fct = "ce",linear.output = FALSE)

pred <- predict(nn_dats,newdata = trainBd)
pred<-round(pred)
train_error <- length(which(trainBd$Property1 != pred))/length(pred)
train_error
r2_nn_train <- R2(trainBd$Property1,pred,form="traditional")
r2_nn_train
cf_train_nn=confusionMatrix(factor(pred), factor(trainBd$Property1))
cf_train_nn
### Test error
pred <- predict(nn_dats,newdata = testBd)
pred <- round(pred)
test_error <- length(which(testBd != pred))/length(pred)
test_error
r2_nn_test <- R2(testBd$Property1,pred,form="traditional")
r2_nn_test
cf_test_nn=confusionMatrix(factor(pred), factor(testBd$Property1))
cf_test_nn

