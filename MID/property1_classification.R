hw <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/MID/eas508_exam1.csv", header=TRUE)
miss <- hw[1:10,]
miss_desc <- miss[,-c(1:2)]
miss_desc
data <- hw[-c(1:10),]
target1 <- miss[,1]
target1
target2 <- miss[,2]
target2
desc <- data[,-c(1:2)]
desc

# Using property 1 as categorical for predictions
Property1 <- ifelse(data$Property1 > 0.7,'Yes','No')
Property1
HW3 <- data.frame(data, Property1)
HW3 <- HW3[,-c(1:2)]

# Setting seed
set.seed(101)
train = sample(1:nrow(HW3),264)
data_test <- HW3[-train,]
data_train <- HW3[train,]

# Bagging
cvcontrol <- trainControl(method = 'repeatedcv', number=10, allowParallel= TRUE)
train.bagg <- train(as.factor(Property1.1)~.,data=data_train,method='treebag',trControl=cvcontrol,importance= TRUE, nbagg = 100)
train.bagg
plot(varImp(train.bagg))
varImp(train.bagg)

pred_test_bag <- predict(train.bagg, data_test)
pred_test_bag
table_mat <- table(data_test$Property1.1,pred_test_bag)
accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
accuracy_Test
pred_missing <- predict(train.bagg, miss_desc)
pred_missing


# Random Forest
library(randomForest)
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.rf <- train(as.factor(Property1.1)~., data= data_train, method='rf',trControl= cvcontrol,importance=TRUE)
train.rf
varImp(train.rf)
plot(varImp(train.rf))

pred_test_rf <- predict(train.rf, data_test)
pred_test_rf
table_mat <- table(data_test$Property1.1,pred_test_rf)
accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
accuracy_Test
pred_missing <- predict(train.rf, miss_desc)
pred_missing


# Boosting
library(gbm)
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.gbm <- train(as.factor(Property1.1)~., data= data_train,method = 'gbm', verbose= F, trControl = cvcontrol)
train.gbm
plot(varImp(train.gbm))
varImp(train.gbm)


pred_test_boost <- predict(train.gbm, data_test)
pred_test_boost
table_mat <- table(data_test$Property1.1,pred_test_boost)
table_mat
accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
accuracy_Test
pred_missing <- predict(train.gbm, miss_desc)
pred_missing

# Logistic Regression

data_train$Property1.1 <- ifelse(data_train$Property1.1=='Yes' , 1 , 0)
data_test$Property1.1 <- ifelse(data_test$Property1.1=='Yes',1, 0)
glm.fit <- glm(Property1.1 ~ ., data = data_train, family = binomial)
glm.fit
glm.probs <- predict(glm.fit,type = "response")
glm.probs
glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")
glm.probs_test <- predict(glm.fit, data = data_test, type='response')[1:66]
glm.pred_test <- ifelse(glm.probs_test > 0.5, "Yes", "No")
data_train$Property1.1 <- ifelse(data_train$Property1.1==1 , 'Yes' , 'No')
data_test$Property1.1 <- ifelse(data_test$Property1.1==1,'Yes', 'No')
table_mat <- table(data_train$Property1.1,glm.pred)
table_mat
accuracy_Train <- sum(diag(table_mat))/sum(table_mat)
accuracy_Train

table_mat_test <- table(data_test$Property1.1,glm.pred_test)
table_mat_test
accuracy_Test <- sum(diag(table_mat_test))/sum(table_mat_test)
accuracy_Test
pred_missing <- predict(glm.fit, miss_desc, type= 'response')[1:10]
glm.pred_miss <- ifelse(pred_missing > 0.5, "Yes", "No")
glm.pred_miss
