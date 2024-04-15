library(ISLR)
data(package="ISLR")
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret)

hist(Carseats$Sales)
#from dataset on carseats, define sales as categorical : ie. high or low; we select 8 as it is in middle, but 8 will need to be changed with different data.
High=ifelse(Carseats$Sales<=8,"No","Yes")
Carseats_data=data.frame(Carseats,High)

#Remove predicted column from predictor columns
Carseats_data=Carseats_data[,-1]

#Build the tree, predicting carseat sales
fit<-rpart(High~.,Carseats_data)
rpart.plot(fit)
summary(fit)
plot(fit)
text(fit,pretty=1)

#Now we want to divide in traing and test data.  Have 400 values, so will split 250/150
set.seed(508)
train=sample(1:nrow(Carseats_data),250)
fit.carseats<-rpart(High~.,Carseats_data,subset=train)
plot(fit.carseats)
text(fit.carseats,pretty=0)

#Let's name the data differently to keep it easier to remember
data_test=Carseats_data[-train,]
data_train=Carseats_data[train,]

#Get a table for the test accuracy
tree.pred=predict(fit.carseats,data_test,type="class")
with(data_test,table(tree.pred,High))

accuracy_tune<-function(fit){
  predict_unseen<-predict(fit,data_test,type='class')
  table_mat<-table(data_test$High,predict_unseen)
  accuracy_Test<-sum(diag(table_mat))/sum(table_mat)
  accuracy_Test
}

#Allow us to change the parameters (minsplit,minbucket,maxdepth,cp) and see how the model changes
control<-rpart.control(minsplit=4,minbucket=2,maxdepth=3,cp=0)
tune_fit<-rpart(High~.,data=data_train,method='class')
accuracy_tune(tune_fit)

#Plotting the accuracy
plot(fit, uniform=TRUE,main="Classification Tree for High Carseat Sales")
text(fit, use.n=TRUE, all=TRUE, cex=1)

#Pruning the tree as a function of cp (complexity parameter)
pfit<-prune(fit,cp=.1)
plot(pfit, uniform=TRUE,main="Pruned Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#Bagging
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.bagg<-train(as.factor(High)~.,data=data_train,method="treebag",trControl=cvcontrol,importance=TRUE) #Can add term: nbagg=x to change the value of B in the bagging.  Default is 25.
train.bagg
plot(varImp(train.bagg))

#Random Forest
train.rf <- train(as.factor(High) ~ ., data=data_train,method="rf",trControl=cvcontrol,importance=TRUE)
train.rf

#Random Forest Boosting
train.gbm <- train(as.factor(High) ~ ., data=data_train,method="gbm",verbose=F,trControl=cvcontrol)
train.gbm

#Random Forest Regression, using sales values, instead of just category
#Would want to split into training and test data first
sales<-Carseats$Sales
descriptors<-Carseats[,2:10]
model<-randomForest(sales~.,data=descriptors,mtry=3,importance=TRUE,na.action=na.omit)
sales_pred<-predict(model,descriptors)
plot(sales,sales_pred)
print(model)
plot(model)