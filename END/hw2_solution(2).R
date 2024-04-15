library(kernlab)
library(hydroGOF)
library(e1071)
library(caret)
library(dplyr)
library(ggplot2)
library(glmnet)
library(randomForest)

#Import data file
hw2<-read.csv(file="hw2.csv")


#Linear Regression 
hw1_cor<-round(cor(hw2),digits=2)
hw1_cor
#Identify the most correlated feature and we will use that
#We find that it is Feature5
data_lr<-hw2[,c("Property","Feature5")]
#Divide into training and test data
set.seed(508)
hw2sort_lr<-sample(1:nrow(data_lr),nrow(data_lr)*0.8)
train_lr<-data_lr[hw2sort_lr,]
test_lr<-data_lr[-hw2sort_lr,]
#Build the model and predict the values
mdl_lr<-lm(Property~Feature5,data=train_lr)
pred_train_lr<-predict(mdl_lr,train_lr)
pred_test_lr<-predict(mdl_lr,test_lr)
#Get the RMSE values
rmse_lr_train<-rmse(pred_train_lr,train_lr$Property)
rmse_lr_test<-rmse(pred_test_lr,test_lr$Property)
rmse_lr_train
rmse_lr_test
#Get the R2 value for the training data
sst<-sum((train_lr$Property-mean(train_lr$Property))^2)
sse<-sum((pred_train_lr-train_lr$Property)^2)
rsq<-1-sse/sst
rsq


#Multiple Linear Regression 
set.seed(508)
hw2sort<-sample(1:nrow(hw2),nrow(hw2)*0.8)
train_mlr<-hw2[hw2sort,]
test_mlr<-hw2[-hw2sort,]
mdl_mlr<-lm(train_mlr$Property~.,data=train_mlr)
pred_train_mlr<-predict(mdl_mlr,data=train_mlr)
pred_test_mlr<-predict(mdl_mlr,newdata=test_mlr)
rmse_mlr_train<-rmse(pred_train_mlr,train_mlr$Property)
rmse_mlr_test<-rmse(pred_test_mlr,test_mlr$Property)
rmse_mlr_train
rmse_mlr_test
sst<-sum((train_mlr$Property-mean(train_mlr$Property))^2)
sse<-sum((pred_train_mlr-train_mlr$Property)^2)
rsq<-1-sse/sst
rsq


#Principal Component Regression
propPCR<-hw2$Property
featurePCR <-hw2[ , ! names(hw2) %in% c("Property")] 
pca<-prcomp(featurePCR)
plot(pca$sdev)
#Select number of PCs
scores<-pca$x[,1:4]
pcr_data<-cbind(propPCR,scores)
pcr_data_d<-as.data.frame(pcr_data)
#Have now defined new data with property and PCs. 
#Now repeat the MLR process
set.seed(508)
hw2sort_pcr<-sample(1:nrow(pcr_data_d),nrow(pcr_data_d)*0.8)
train_pcr<-as.data.frame(pcr_data[hw2sort_pcr,])
test_pcr<-as.data.frame(pcr_data[-hw2sort_pcr,])
mdl_pcr<-lm(train_pcr$propPCR~.,data=train_pcr)
pred_train_pcr<-predict(mdl_pcr,data=train_pcr)
pred_test_pcr<-predict(mdl_pcr,newdata=test_pcr)
rmse_pcr_train<-rmse(pred_train_pcr,train_pcr$propPCR)
rmse_pcr_test<-rmse(pred_test_pcr,test_pcr$propPCR)
rmse_pcr_train
rmse_pcr_test
sst<-sum((train_pcr$propPCR-mean(train_pcr$propPCR))^2)
sse<-sum((pred_train_pcr-train_pcr$propPCR)^2)
rsq<-1-sse/sst
rsq


#Ridge Regression
set.seed(508)
hw2sort<-sample(1:nrow(hw2),nrow(hw2)*0.8)
train_ridge<-hw2[hw2sort,]
test_ridge<-hw2[-hw2sort,]
descriptors_train_ridge<-train_ridge[,! names(train_ridge) %in% c("Property")]
descriptors_test_ridge<-test_ridge[,! names(test_ridge) %in% c("Property")]
descriptors_train_ridge<-as.matrix(descriptors_train_ridge)
descriptors_test_ridge<-as.matrix(descriptors_test_ridge)
mdl_ridge<-glmnet(descriptors_train_ridge,train_ridge$Property,alpha=0)
mdl_ridge_cv<-cv.glmnet(descriptors_train_ridge,train_ridge$Property,alpha=0)
best_lambda<-mdl_ridge_cv$lambda.min
mdl_ridge_best<-glmnet(descriptors_train_ridge,train_ridge$Property,alpha=0,lambda=best_lambda)
coef(mdl_ridge)
pred_train_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_train_ridge)
pred_test_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_test_ridge)
pred_train_ridge<-as.data.frame(pred_train_ridge)
pred_test_ridge<-as.data.frame(pred_test_ridge)
rmse_ridge_train<-rmse(pred_train_ridge,train_ridge$Property)
rmse_ridge_test<-rmse(pred_test_ridge,test_ridge$Property)
rmse_ridge_train
rmse_ridge_test
sst<-sum((train_ridge$Property-mean(train_ridge$Property))^2)
sse<-sum((pred_train_ridge-train_ridge$Property)^2)
rsq<-1-sse/sst
rsq

#Lasso Regression
#The process is exactly the same as Ridge, we just change alpha from 0 to 1
set.seed(508)
hw2sort<-sample(1:nrow(hw2),nrow(hw2)*0.8)
train_lasso<-hw2[hw2sort,]
test_lasso<-hw2[-hw2sort,]
descriptors_train_lasso<-train_lasso[,! names(train_lasso) %in% c("Property")]
descriptors_test_lasso<-test_lasso[,! names(test_lasso) %in% c("Property")]
descriptors_train_lasso<-as.matrix(descriptors_train_lasso)
descriptors_test_lasso<-as.matrix(descriptors_test_lasso)
mdl_lasso<-glmnet(descriptors_train_lasso,train_lasso$Property,alpha=1)
mdl_lasso_cv<-cv.glmnet(descriptors_train_lasso,train_lasso$Property,alpha=1)
best_lambda<-mdl_lasso_cv$lambda.min
mdl_lasso_best<-glmnet(descriptors_train_lasso,train_lasso$Property,alpha=1,lambda=best_lambda)
coef(mdl_lasso)
pred_train_lasso<-predict(mdl_lasso,s=best_lambda,newx=descriptors_train_lasso)
pred_test_lasso<-predict(mdl_lasso,s=best_lambda,newx=descriptors_test_lasso)
pred_train_lasso<-as.data.frame(pred_train_lasso)
pred_test_lasso<-as.data.frame(pred_test_lasso)
rmse_lasso_train<-rmse(pred_train_lasso,train_lasso$Property)
rmse_lasso_test<-rmse(pred_test_lasso,test_lasso$Property)
rmse_lasso_train
rmse_lasso_test
sst<-sum((train_lasso$Property-mean(train_lasso$Property))^2)
sse<-sum((pred_train_lasso-train_lasso$Property)^2)
rsq<-1-sse/sst
rsq


#Support vector regression
set.seed(508)
hw2sort<-sample(1:nrow(hw2),nrow(hw2)*0.8)
train_svr<-hw2[hw2sort,]
test_svr<-hw2[-hw2sort,]
train_svr_d<-data.frame(train_svr)
descriptors_train_svr<-train_svr[,! names(train_svr) %in% c("Property")]
descriptors_test_svr<-test_svr[,! names(test_svr) %in% c("Property")]
descriptors_train_svr<-as.matrix(descriptors_train_svr)
descriptors_test_svr<-as.data.frame(descriptors_test_svr)
prop_train_svr<-train_svr$Property
prop_test_svr<-test_svr$Property
mdl_svr<-tune(svm,prop_train_svr~descriptors_train_svr,ranges=list(epsilon=seq(0,1,0.1),cost=1:10))
BstModel<-mdl_svr$best.model
summary(BstModel)
#Update the regression model with the selections from BstModel (kernel, cost, gamma, epsilon)
svmfit <- svm(train_svr$Property ~., data = train_svr, method="eps-regression",kernel = 'radial', cost = 2, gamma=0.125,epsilon=.1,scale=FALSE)
pred_train_svr<-predict(svmfit, data=descriptors_train_svr)
pred_test_svr<-predict(svmfit,newdata=descriptors_test_svr)
rmse_SVR_train<-rmse(pred_train_svr,prop_train_svr)
rmse_SVR_test<-rmse(pred_test_svr,prop_test_svr)
rmse_SVR_train
rmse_SVR_test
sst<-sum((train_svr$Property-mean(train_svr$Property))^2)
sse<-sum((pred_train_svr-train_svr$Property)^2)
rsq<-1-sse/sst
rsq


#Genetic Process Regression
set.seed(508)
hw2sort<-sample(1:nrow(hw2),nrow(hw2)*0.8)
train_gpr<-hw2[hw2sort,]
test_gpr<-hw2[-hw2sort,]
descriptors_train_gpr<-train_svr[,! names(train_ridge) %in% c("Property")]
descriptors_test_gpr<-test_svr[,! names(test_ridge) %in% c("Property")]
mdl_gpr<-gausspr(descriptors_train_gpr,train_gpr$Property)
pred_train_gpr<-predict(mdl_gpr,descriptors_train_gpr)
pred_test_gpr<-predict(mdl_gpr,descriptors_test_gpr)
rmse_gpr_train<-rmse(pred_train_gpr,as.matrix(train_gpr$Property))
rmse_gpr_test<-rmse(pred_test_gpr,as.matrix(test_gpr$Property))
rmse_gpr_train
rmse_gpr_test
sst<-sum((train_gpr$Property-mean(train_gpr$Property))^2)
sse<-sum((pred_train_gpr-train_gpr$Property)^2)
rsq<-1-sse/sst
rsq


#Random Forest Regression
set.seed(508)
hw2sort<-sample(1:nrow(hw2),nrow(hw2)*0.8)
train_rf<-hw2[hw2sort,]
test_rf<-hw2[-hw2sort,]
model_rf<-randomForest(train_rf$Property~.,data=train_rf,mtry=3,importance=TRUE,na.action=na.omit)
pred_train_rf<-predict(model_rf,train_rf)
pred_test_rf<-predict(model_rf,newdata=test_rf)
rmse_rf_train<-rmse(pred_train_rf,train_rf$Property)
rmse_rf_test<-rmse(pred_test_rf,test_rf$Property)
rmse_rf_train
rmse_rf_test
sst<-sum((train_rf$Property-mean(train_rf$Property))^2)
sse<-sum((pred_train_rf-train_rf$Property)^2)
rsq<-1-sse/sst
rsq
plot(model)