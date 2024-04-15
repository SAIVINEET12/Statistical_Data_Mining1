# Bagging
HW3 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/HW2.csv", header=TRUE)
#HW3 <- as.data.frame(scale(HW3))

# Using property 1 as categorical for predictions
Property1 <- ifelse(HW3$Property > 7.835,'Yes','No')
summary(Property1)
HW3 <- data.frame(HW3, Property1)
HW3 <- HW3[,-1]

# Setting seed
set.seed(101)
train = sample(1:nrow(HW3),75)
data_test <- HW3[-train,]
data_train <- HW3[train,]

# Bagging
cvcontrol <- trainControl(method = 'repeatedcv', number=10, allowParallel= TRUE)
train.bagg <- train(as.factor(Property1)~.,data=data_train,method='treebag',trControl=cvcontrol,importance= TRUE, nbagg = 100)
train.bagg
plot(varImp(train.bagg))
varImp(train.bagg)

pred_test_bag <- predict(train.bagg, data_test)
pred_test_bag
table_mat <- table(data_test$Property1,pred_test_bag)
accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
accuracy_Test