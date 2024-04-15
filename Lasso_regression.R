# Lasso Regression alpha = 1
HW2 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/HW2.csv", header=TRUE)
HW2 <- scale(HW2)


# Splitting training and test dataset
HW2_sort <- sort(sample(nrow(HW2), nrow(HW2)*0.8))
hw_train <- HW2[HW2_sort,]
hw_test <- HW2[-HW2_sort,]

# Setup predictors and target
hw_train_target <- hw_train[,1]
hw_train_desc <- hw_train[,-1]
hw_train_desc <- as.matrix(hw_train_desc)

hw_test_target <- hw_test[,1]
hw_test_desc <- hw_test[,-1]
hw_test_desc <- as.matrix(hw_test_desc)

# Using the Lasso Regression
cv_lasso <- cv.glmnet(hw_train_desc, hw_train_target, alpha = 1)
best_lambda <- cv_lasso$lambda.min
plot(cv_ridge)
best_lambda
log(best_lambda)
best_lasso <- glmnet(hw_train_desc, hw_train_target, alpha = 1, lambda = best_lambda)
coef(best_ridge)

# R2 value for Lasso
# For Training set
y_train <- predict(best_lasso, s = best_lambda, newx = hw_train_desc)
sst <- sum((hw_train_target - mean(hw_train_target))^2)
ssr <- sum((hw_train_target - y_train)^2)
rsq <- 1 - ssr/sst
rsq
rmse_train <- rmse(hw_train_target,y_train)
rmse_train

# For Testing set
y_test <- predict(best_lasso, s= best_lambda, newx = hw_test_desc)
sst_test <- sum((hw_test_target - mean(hw_test_target))^2)
ssr_test <- sum((hw_test_target - y_test)^2)
R2_test <- 1 - ssr_test/sst_test
R2_test
rmse_test <- rmse(hw_test_target, y_test)
rmse_test

plot(hw_test_target,y_test, xlab= 'actual value', ylab='predicted value',main='Lasso Regression')
abline(0,1)
par(mfrow = c(1,1))
