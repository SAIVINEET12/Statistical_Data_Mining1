HW2 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/HW2.csv", header=TRUE)
HW2 <- as.data.frame(scale(HW2))

# Splitting training and test dataset
HW2_sort <- sort(sample(nrow(HW2), nrow(HW2)*0.70))
hw_train <- HW2[HW2_sort,]
hw_test <- HW2[-HW2_sort,]

# Setup predictors and target
hw_train_target <- hw_train[,1]
hw_train_desc <- hw_train[,-1]
hw_train_desc <- as.matrix(hw_train_desc)

hw_test_target <- hw_test[,1]
hw_test_desc <- hw_test[,-1]

# Multiple Regression
linearmodel = lm(hw_train_target~hw_train_desc)
y_train <- predict(linearmodel, data = hw_train_desc)
y_test <- predict(linearmodel, newdata = hw_test_desc)[1:30]

# RMSE for Linear Regression
y_train <- as.matrix(y_train)
rmse_train <- rmse(y_train , as.matrix(hw_train_target))
rmse_train
y_test <- as.matrix(y_test)
rmse_test <- rmse(y_test , (hw_test_target))
rmse_test

# R2 for Linear Regression
sst <- sum((hw_train_target - mean(hw_train_target))^2)
ssr <- sum((y_train - hw_train_target)^2)
R2_train <- 1 - ssr/sst
R2_train

sst_test <- sum((hw_test_target - mean(hw_test_target))^2)
ssr_test <- sum((y_test - hw_test_target)^2)
R2_test <- 1 - ssr_test/sst_test
R2_test
par(mfrow = c(2,2))
plot(linearmodel)

