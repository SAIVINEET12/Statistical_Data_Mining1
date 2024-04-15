# Random Forest
HW3 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/HW2.csv", header=TRUE)
#HW3 <- as.data.frame(scale(HW3))

# Using property 1 as categorical for predictions
Property1 <- ifelse(HW3$Property > 7.835, 'Yes','No')
summary(Property1)
HW3 <- data.frame(HW3, Property1)
HW3 <- HW3[,-1]

# Setting Seed
set.seed(101)
train = sample(1:nrow(HW3),75)
data_test <- HW3[-train,]
data_train <- HW3[train,]

# Random Forest
library(randomForest)
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.rf <- train(as.factor(Property1)~., data= data_train, method='rf',trControl= cvcontrol,importance=TRUE)
train.rf
varImp(train.rf)
plot(varImp(train.rf))

pred_test_rf <- predict(train.rf, data=data_test)
pred_test_rf
table_mat <- table(data_test$Property1,pred_test_rf)
accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
accuracy_Test
