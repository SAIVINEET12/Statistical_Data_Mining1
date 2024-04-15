hw <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/MID/eas508_exam1.csv", header=TRUE)
#hw <- as.data.frame(scale(hw))
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

# Splitting training and test data set
set.seed(5)
HW2_sort <- sort(sample(nrow(data), nrow(data)*0.8))
hw_train <- data[HW2_sort,]
hw_test <- data[-HW2_sort,]

# Setup predictors and target
hw_train_target1 <- hw_train[,1]
hw_train_target2 <- hw_train[,2]
hw_train_desc <- hw_train[,-c(1:2)]

hw_test_target1 <- hw_test[,1]
hw_test_target2 <- hw_test[,2]
hw_test_desc <- hw_test[,-c(1:2)]

# Gauss PR
mdl <- gausspr(hw_train_desc,hw_train_target1)
pred_train <- predict(mdl, hw_train_desc)
pred_test <- predict(mdl , hw_test_desc)
print(mdl)
plot(hw_test_target1, pred_test, main='Gaussian Process Regression - Property1')
abline(0,1)
pred_missing <- predict(mdl, miss_desc)
pred_missing

# RMSE Values for training and testing data
hw_train_target1 <- as.matrix(hw_train_target1)
rmse_train <- rmse(pred_train,hw_train_target1)
rmse_train

hw_test_target1 <- as.matrix(hw_test_target1)
rmse_test <- rmse(pred_test, hw_test_target1)
rmse_test

# R2 values for training and testing data
sst_train <- sum((hw_train_target1 - mean(hw_train_target1))^2)
ssr_train <- sum((hw_train_target1 - pred_train)^2)
R2_train <- 1 - ssr_train/sst_train
R2_train

sst_test <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr_test <- sum((hw_test_target1 - pred_test)^2)
R2_test <- 1 - ssr_test/sst_test
R2_test


# SVR Model
hw_train_desc <- as.matrix(hw_train_desc)
hw_train_target1 <- as.matrix(hw_train_target1)
optmodelsvm <- tune(svm , hw_train_target1~hw_train_desc , ranges = list(epsilon = seq(0,1,0.1),cost = 1:100))
best <- optmodelsvm$best.model
pred_y_train <- predict(best , hw_train_desc)
pred_y_test <- predict(best , hw_test_desc)[1:66]
plot(hw_test_target1, pred_y_test, main='Support Vector Regression - Property1')
abline(0,1)
pred_missing <- predict(best, miss_desc)
pred_missing

# RMSE Values for training and testing data
pred_y_train <- as.matrix(pred_y_train)
rmse1_train <- rmse(pred_y_train , hw_train_target1)
rmse1_train
pred_y_test <- as.matrix(pred_y_test)
rmse1_test <- rmse(pred_y_test , hw_test_target1)
rmse1_test

# R2 for training and testing data
sst1_train <- sum((hw_train_target1 - mean(hw_train_target1))^2)
ssr1_train <- sum((hw_train_target1 - pred_y_train)^2)
R2_train1 <- 1 - ssr_train/sst_train
R2_train1

sst1_test <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr1_test <- sum((hw_test_target1 - pred_y_test)^2)
R2_test1 <- 1 - ssr1_test/sst1_test
R2_test1

# Random Forest Regression
model <- randomForest(hw_train_target1~., data= hw_train_desc, importance= TRUE, na.action = na.omit , mtry = 6)
print(model)
plot(model)
test_pred <- predict(model, newdata = hw_test_desc)
pred_missing <- predict(model, miss_desc)
pred_missing
plot(hw_test_target1, test_pred, main = 'Random Forest Regression - Property1')
abline(0,1)
sst <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr <- sum((test_pred - hw_test_target1)^2)
rsq <- 1 - ssr/sst
rsq
