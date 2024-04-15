library(kernlab)
library(hydroGOF)
library(e1071)
library(caret)
library(dplyr)
library(ggplot2)
library(glmnet)
library(randomForest)

data <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/END/eas508_exam2.csv",header = TRUE)
#data <- as.data.frame(scale(data))
# Splitting training and test data set
#set.seed(5)
HW2_sort <- sort(sample(nrow(data), nrow(data)*0.80))
hw_train <- data[HW2_sort,]
hw_test <- data[-HW2_sort,]

# Setup predictors and target
hw_train_target1 <- hw_train[,1]
hw_train_desc <- hw_train[,-1]
hw_train_desc <- scale(hw_train_desc)
hw_test_target1 <- hw_test[,1]
hw_test_desc <- hw_test[,-1]
hw_test_desc <- scale(hw_test_desc)

# Gauss PR
mdl <- gausspr(hw_train_desc,hw_train_target1)
pred_train <- predict(mdl, hw_train_desc)
pred_test <- predict(mdl , hw_test_desc)
print(mdl)
plot(hw_test_target1, pred_test, main='Gaussian Process Regression - Property1')
abline(0,1)

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



# Random Forest Regression
model <- randomForest(hw_train_target1~., data= hw_train_desc, importance= TRUE, na.action = na.omit , mtry = 6)
print(model)
plot(model)
varImp(model)
varImpPlot(model)

test_pred <- predict(model, newdata = hw_test_desc)
train_pred <- predict(model, hw_train_desc)
plot(hw_test_target1, test_pred, main = 'Random Forest Regression - Property1')
abline(0,1)
sst_train <- sum((hw_train_target1 - mean(hw_train_target1))^2)
ssr_train <- sum((hw_train_target1 - train_pred)^2)
rsq_train <- 1 - ssr_train/sst_train
rsq_train
sst <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr <- sum((test_pred - hw_test_target1)^2)
rsq <- 1 - ssr/sst
rsq
# RMSE Values for training and testing data
rf_train_pred <- as.matrix(train_pred)
rmse1_trainlm <- rmse(rf_train_pred , hw_train_target1)
rmse1_trainlm
rf_test_pred <- as.matrix(test_pred)
rmse1_testlm <- rmse(rf_test_pred , hw_test_target1)
rmse1_testlm



# Linear Regression
data <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/END/eas508_exam2.csv",header = TRUE)
#data <- as.data.frame(scale(data))
# Splitting training and test data set
#set.seed(5)
HW2_sort <- sort(sample(nrow(data), nrow(data)*0.8))
hw_train <- data[HW2_sort,]
hw_test <- data[-HW2_sort,]

# Setup predictors and target
hw_train_target1 <- hw_train[,1]
hw_train_desc <- hw_train[,-1]
#hw_train_desc <- scale(hw_train_desc)
hw_test_target1 <- hw_test[,1]
hw_test_desc <- hw_test[,-1]
#hw_test_desc <- scale(hw_test_desc)
lm_mdl <- lm(hw_train_target1~., data = hw_train_desc)
print(lm_mdl)
lm_test_pred <- predict(lm_mdl, new_data = hw_test_desc)[1:60]
lm_train_pred <- predict(lm_mdl, new_data = hw_train_desc)

# RMSE Values for training and testing data
#lm_train_pred <- as.matrix(lm_train_pred)
rmse1_trainlm <- rmse(lm_train_pred , hw_train_target1)
rmse1_trainlm
#lm_test_pred <- as.matrix(lm_test_pred)
rmse1_testlm <- rmse(lm_test_pred , hw_test_target1)
rmse1_testlm

# R2 for training and testing data
sstlm_train <- sum((hw_train_target1 - mean(hw_train_target1))^2)
ssrlm_train <- sum((hw_train_target1 - lm_train_pred)^2)
R2_trainlm <- 1 - ssrlm_train/sstlm_train
R2_trainlm

sst1_testlm <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr1_testlm <- sum((hw_test_target1 - lm_test_pred)^2)
R2_testlm <- 1 - ssr1_testlm/sst1_testlm
R2_testlm

# Using the Lasso Regression
data <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/END/eas508_exam2.csv",header = TRUE)
#data <- as.data.frame(scale(data))
# Splitting training and test data set
#set.seed(5)
HW2_sort <- sort(sample(nrow(data), nrow(data)*0.8))
hw_train <- data[HW2_sort,]
hw_test <- data[-HW2_sort,]

# Setup predictors and target
hw_train_target1 <- hw_train[,1]
hw_train_desc <- hw_train[,-1]
#hw_train_desc <- scale(hw_train_desc)
hw_test_target1 <- hw_test[,1]
hw_test_desc <- hw_test[,-1]
#hw_test_desc <- scale(hw_test_desc)
cv_lasso <- cv.glmnet(as.matrix(hw_train_desc), as.matrix(hw_train_target1), alpha = 1)
best_lambda <- cv_lasso$lambda.min
plot(cv_lasso)
best_lambda
log(best_lambda)
best_lasso <- glmnet(as.matrix(hw_train_desc), as.matrix(hw_train_target1), alpha = 1, lambda = best_lambda)
coef(best_lasso)

# R2 value for Lasso
# For Training set
y_train <- predict(best_lasso, s = best_lambda, newx = as.matrix(hw_train_desc))
y_train
sst <- sum((hw_train_target1 - mean(hw_train_target1))^2)
ssr <- sum((hw_train_target1 - y_train)^2)
rsq <- 1 - ssr/sst
rsq
rmse_train <- rmse(as.matrix(hw_train_target1),y_train)
rmse_train

# For Testing set
y_test <- predict(best_lasso, s= best_lambda, newx = as.matrix(hw_test_desc))
sst_test <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr_test <- sum((hw_test_target1 - y_test)^2)
R2_test <- 1 - ssr_test/sst_test
R2_test
rmse_test <- rmse(as.matrix(hw_test_target1), y_test)
rmse_test

plot(hw_test_target1,y_test, xlab= 'actual value', ylab='predicted value',main='Lasso Regression')
abline(0,1)
par(mfrow = c(1,1))

# Using the Ridge Regression
cv_ridge <- cv.glmnet(as.matrix(hw_train_desc), as.matrix(hw_train_target1), alpha = 0)
best_lambda <- cv_ridge$lambda.min
plot(cv_ridge)
best_lambda
log(best_lambda)
best_ridge <- glmnet(as.matrix(hw_train_desc), as.matrix(hw_train_target1), alpha = 0, lambda = best_lambda)
coef(best_ridge)

# R2 value for Ridge
# For Training set
y_train <- predict(best_ridge, s = best_lambda, newx = as.matrix(hw_train_desc))
sst <- sum((hw_train_target1 - mean(hw_train_target1))^2)
ssr <- sum((hw_train_target1 - y_train)^2)
rsq <- 1 - ssr/sst
rsq
rmse_train <- rmse(as.matrix(hw_train_target1),y_train)
rmse_train


# For Testing set
y_test <- predict(best_ridge, s= best_lambda, newx = as.matrix(hw_test_desc))
sst_test <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr_test <- sum((hw_test_target1 - y_test)^2)
R2_test <- 1 - ssr_test/sst_test
R2_test
rmse_test <- rmse(as.matrix(hw_test_target1),y_test )
rmse_test

plot(hw_test_target1,y_test, xlab= 'actual value', ylab='predicted value',main='Ridge Regression')
abline(0,1)
par(mfrow = c(1,1))

#Principal Component Analysis
data <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/END/eas508_exam2.csv",header = TRUE)
data <- as.data.frame(scale(data))
# Splitting training and test data set
#set.seed(5)
HW2_sort <- sort(sample(nrow(data), nrow(data)*0.8))
hw_train <- data[HW2_sort,]
hw_test <- data[-HW2_sort,]

# Setup predictors and target
hw_train_target1 <- hw_train[,1]
hw_train_desc <- hw_train[,-1]
hw_train_desc <- scale(hw_train_desc)
hw_test_target1 <- hw_test[,1]
hw_test_desc <- hw_test[,-1]
hw_test_desc <- scale(hw_test_desc)
pca1 <- prcomp(data)
pca1
scores <- pca1$x
loads <- pca1$rotation
pca1$sdev
plot(pca1$sdev)
sum(pca1$sdev)
sum(pca1$sdev[1:13])/sum(pca1$sdev)
loads_prop <- loads[1,1:13]
loads_desc <- loads[-1, 1:13]
weights <- loads_prop * loads_desc
vip <- weights[,1] + weights[,2] + weights[,3] + weights[,4] + weights[,5] + weights[,6] + weights[,7] + weights[,8] + weights[,9] + weights[,10] + weights[,11] + weights[,12]
barplot(sort(vip))
sort(vip)
# We choose features 1,2,4,9,10,11,12,15,16,20
pcr = prcomp(hw_train_desc)
View(pcr)
pcr
plot(pcr$sdev)
#using the 4 pcas for regression
scores_pcr <- pcr$x[,1:13]
pcrmodel <- lm(hw_train_target1 ~ scores_pcr)
pcrmodel
summary(pcrmodel)
# R2 value for Ridge
# For Training set
y_train <- predict(pcrmodel, data = hw_train_desc)
sst <- sum((hw_train_target1 - mean(hw_train_target1))^2)
ssr <- sum((hw_train_target1 - y_train)^2)
rsq <- 1 - ssr/sst
rsq
rmse_train <- rmse(hw_train_target1,y_train)
rmse_train


# For Testing set
y_test <- predict(pcrmodel, data =  hw_test_desc)[1:60]
sst_test <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr_test <- sum((hw_test_target1 - y_test)^2)
R2_test <- 1 - ssr_test/sst_test
R2_test
rmse_test <- rmse(hw_test_target1,y_test )
rmse_test

plot(hw_test_target1,y_test, xlab= 'actual value', ylab='predicted value',main='PCA')
abline(0,1)
par(mfrow = c(1,1))

# SVR Model
hw_train_desc <- as.matrix(hw_train_desc)
hw_train_target1 <- as.matrix(hw_train_target1)
optmodelsvm <- tune(svm , hw_train_target1~hw_train_desc , ranges = list(epsilon = seq(0,1,0.1),cost = 1:100))
best <- optmodelsvm$best.model
pred_y_train <- predict(best , hw_train_desc)
pred_y_test <- predict(best , hw_test_desc)[1:60]
plot(hw_test_target1, pred_y_test, main='Support Vector Regression - Property1')
abline(0,1)

# RMSE Values for training and testing data
pred_y_train <- as.matrix(pred_y_train)
rmse1_train <- rmse(pred_y_train ,as.matrix( hw_train_target1))
rmse1_train
pred_y_test <- as.matrix(pred_y_test)
rmse1_test <- rmse(pred_y_test , as.matrix(hw_test_target1))
rmse1_test

# R2 for training and testing data
sst1_train <- sum((hw_train_target1 - mean(hw_train_target1))^2)
ssr1_train <- sum((hw_train_target1 - pred_y_train)^2)
R2_train1 <- 1 - ssr1_train/sst1_train
R2_train1

sst1_test <- sum((hw_test_target1 - mean(hw_test_target1))^2)
ssr1_test <- sum((hw_test_target1 - pred_y_test)^2)
R2_test1 <- 1 - ssr1_test/sst1_test
R2_test1

corr <- cor(data)
ggcorrplot(corr)


