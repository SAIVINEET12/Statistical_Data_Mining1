homework1 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/homework1.csv", header=TRUE)
summary(homework1)
hw_std <- as.data.frame(scale(homework1))
targetprop <- hw_std[,1]
descriptors <- hw_std[,-1]
target_sort <- sort(sample(nrow(hw_std) , nrow(hw_std)*0.8))
trainset <- hw_std[target_sort,]
testset <- hw_std[-target_sort,]
train_target <- hw_std[target_sort,1]
test_target <- hw_std[-target_sort,1]
train_desc <- hw_std[target_sort,-1]
test_desc <- hw_std[-target_sort,-1]
model_linear <- lm(Property1~. , data = trainset)
summary(model_linear)
predtest <- predict(model_linear , newdata = testset)
plot(predtest , testset$Property1)


svm_train <- svm(Property1~., data = trainset)
summary(svm_train)
pred_svm <- predict(svm_train , newdata = testset)
plot(pred_svm , testset$Property1)
rmse(pred_svm , testset$Property1)
w = t(svm_train$coefs)
w
b <- t(svm_train$rho)
b
#plot(svm_train)


#TUNED SVM without pca on all features
#optmodelsvm <- tune(svm,train.x =  train_desc,train.y = train_target , ranges = list(epsilon = seq(0,1,0.1), cost = 1:100))
optmodelsvm <- tune(svm,trainset[,-1] , trainset[,1] , ranges = list(epsilon = seq(0,1,0.1), cost = 1:100))
summary(optmodelsvm)
print(optmodelsvm)
pred_prop_tuned <- predict(optmodelsvm$best.model)
rmsesvm_tuned <- rmse(pred_prop_tuned, trainset$Property1)
rmsesvm_tuned





