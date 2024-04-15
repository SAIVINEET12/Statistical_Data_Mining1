library(kernlab)
library(hydroGOF)
library(e1071)
library(caret)
library(dplyr)
library(ggplot2)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)

#Import data file
hw2<-read.csv(file="hw2.csv")

#Categorize the Data
hist(hw2$Property)
#from dataset, we can pick a cutoff.  For this, we will define 7.8 as it is in the middle.  Obviously not particularly rigorous in our selection, but we'll go with it for the example.
High=ifelse(hw2$Property<=7.8,"No","Yes")
hw3_class=data.frame(hw2,High)
hw3_class=hw3_class[,-1]


#Build the tree, predicting property from hw 2 data
#Break into training and test data
set.seed(508)
hw3sort<-sample(1:nrow(hw3_class),nrow(hw3_class)*0.8)
train_dt<-hw3_class[hw3sort,]
test_dt<-hw3_class[-hw3sort,]
fit<-rpart(High~.,train_dt)
tree.pred_train=predict(fit,train_dt,type="class")
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(fit,test_dt,type="class")
with(test_dt,table(tree.pred_test,High))
accuracy_tune<-function(fit){
  predict_unseen<-predict(fit,test_dt,type='class')
  table_mat<-table(test_dt$High,predict_unseen)
  accuracy_Test<-sum(diag(table_mat))/sum(table_mat)
  accuracy_Test
}
#Allow us to change the parameters (minsplit,minbucket,maxdepth,cp) and see how the model changes
control<-rpart.control(minsplit=4,minbucket=2,maxdepth=3,cp=0)
tune_fit<-rpart(High~.,data=test_dt,method='class',control=control)
accuracy_tune(tune_fit)
#Identify the tuned parameters
tune_fit$control
#Pruning the tree as a function of cp (complexity parameter)
pfit<-prune(tune_fit,cp=.1)
plot(pfit, uniform=TRUE,main="Pruned Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
tree.pred_train=predict(pfit,train_dt,type="class")
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(pfit,test_dt,type="class")
with(test_dt,table(tree.pred_test,High))


#Bagging
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.bagg<-train(as.factor(High)~.,data=train_dt,method="treebag",trControl=cvcontrol,importance=TRUE,nbagg=50,minsplit=4,minbucket=2,maxdepth=3,cp=0)
train.bagg
plot(varImp(train.bagg))
tree.pred_train=predict(train.bagg,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.bagg,test_dt)
with(test_dt,table(tree.pred_test,High))


#Boosting
#Random Forest
train.rf <- train(as.factor(High) ~ ., data=train_dt,method="rf",trControl=cvcontrol,importance=TRUE)
train.rf
tree.pred_train=predict(train.rf,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.rf,test_dt)
with(test_dt,table(tree.pred_test,High))
#Random Forest Boosting
train.gbm <- train(as.factor(High) ~ ., data=train_dt,method="gbm",verbose=F,trControl=cvcontrol)
train.gbm
tree.pred_train=predict(train.gbm,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.gbm,test_dt)
with(test_dt,table(tree.pred_test,High))


#Logistic Regression
#Break into training and test data
High=ifelse(hw2$Property<=7.8,0,1)
hw3_class=data.frame(hw2,High)
hw3_class=hw3_class[,-1]
set.seed(508)
hw3sort<-sample(1:nrow(hw3_class),nrow(hw3_class)*0.8)
train_logit<-hw3_class[hw3sort,]
test_logit<-hw3_class[-hw3sort,]
#Develop Logistic Regression Model on the Training Data
glm.fit <- glm(train_logit$High ~ ., data = train_logit, family = binomial)
glm.probs_train <- predict(glm.fit,train_logit,type = "response")
glm.pred_train <- ifelse(glm.probs_train > 0.5, "1", "0")
#Assess prediction result as table as yes / no accuracy
(misclass <- table(glm.pred_train, truth = train_logit$High))
#Apply model and repeat on training data
glm.probs_test <- predict(glm.fit,test_logit,type = "response")
glm.pred_test <- ifelse(glm.probs_test > 0.5, "1", "0")
(misclass <- table(glm.pred_test, truth = test_logit$High))


#Support Vector Machine
set.seed(508)
hw3sort<-sample(1:nrow(hw3_class),nrow(hw3_class)*0.8)
train_svm<-hw3_class[hw3sort,]
test_svm<-hw3_class[-hw3sort,]
svmfit=svm(train_svm$High~.,data=train_svm,kernel="linear",epsilon=0.15,cost=1,scale=FALSE)
svm.probs_train <- predict(svmfit,train_svm,type = "response")
svm.pred_train <- ifelse(svm.probs_train > 0.5, "1", "0")
(misclass <- table(svm.pred_train, truth = train_svm$High))
svm.probs_test <- predict(svmfit,newdata=test_svm,type = "response")
svm.pred_test <- ifelse(svm.probs_test > 0.5, "1", "0")
(misclass <- table(svm.pred_test, truth = test_svm$High))
