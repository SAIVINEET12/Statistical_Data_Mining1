HW2 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/HW2.csv", header=TRUE)
HW2 <- as.data.frame(scale(HW2))

# Splitting training and test data set
HW2_sort <- sort(sample(nrow(HW2), nrow(HW2)*0.8))
hw_train <- HW2[HW2_sort,]
hw_test <- HW2[-HW2_sort,]

# Setup predictors and target
hw_train_target <- hw_train[,1]
hw_train_desc <- hw_train[,-1]

hw_test_target <- hw_test[,1]
hw_test_desc <- hw_test[,-1]

# Gauss PR
mdl <- gausspr(hw_train_desc,hw_train_target)
pred_train <- predict(mdl, hw_train_desc)
pred_test <- predict(mdl , hw_test_desc)

# RMSE Values for training and testing data
hw_train_target <- as.matrix(hw_train_target)
rmse_train <- rmse(pred_train,hw_train_target)
rmse_train

hw_test_target <- as.matrix(hw_test_target)
rmse_test <- rmse(pred_test, hw_test_target)
rmse_test

# R2 values for training and testing data
sst_train <- sum((hw_train_target - mean(hw_train_target))^2)
ssr_train <- sum((hw_train_target - pred_train)^2)
R2_train <- 1 - ssr_train/sst_train
R2_train

sst_test <- sum((hw_test_target - mean(hw_test_target))^2)
ssr_test <- sum((hw_test_target - pred_test)^2)
R2_test <- 1 - ssr_test/sst_test
R2_test


# SVR Model
hw_train_desc <- as.matrix(hw_train_desc)
hw_train_target <- as.matrix(hw_train_target)
optmodelsvm <- tune(svm , hw_train_target~hw_train_desc , ranges = list(epsilon = seq(0,1,0.1),cost = 1:100))
best <- optmodelsvm$best.model
pred_y_train <- predict(best , hw_train_desc)
pred_y_test <- predict(best , hw_test_desc)[1:20]

# RMSE Values for training and testing data
pred_y_train <- as.matrix(pred_y_train)
rmse1_train <- rmse(pred_y_train , hw_train_target)
rmse1_train
pred_y_test <- as.matrix(pred_y_test)
rmse1_test <- rmse(pred_y_test , hw_test_target)
rmse1_test

# R2 for training and testing data
sst1_train <- sum((hw_train_target - mean(hw_train_target))^2)
ssr1_train <- sum((hw_train_target - pred_y_train)^2)
R2_train1 <- 1 - ssr_train/sst_train
R2_train1

sst1_test <- sum((hw_test_target - mean(hw_test_target))^2)
ssr1_test <- sum((hw_test_target - pred_y_test)^2)
R2_test1 <- 1 - ssr1_test/sst1_test
R2_test1
