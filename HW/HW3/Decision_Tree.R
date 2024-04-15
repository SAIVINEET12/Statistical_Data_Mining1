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
fit.HW3 <- rpart(Property1~.,HW3, subset = train)
fit.HW3
plot(fit.HW3)
text(fit.HW3,pretty = 0)
rpart.plot(fit.HW3)
tree.pred <- predict(fit.HW3, HW3[-train,],type='class')
with(HW3[-train,],table(tree.pred,Property1))

# Adjustable Parameters
data_test <- HW3[-train,]
data_train <- HW3[train,]
accuracy_tune <- function(fit){
  predict_unseen <- predict(fit, data_test, type='class')
  table_mat <- table(data_test$Property1,predict_unseen)
  accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
  accuracy_Test
}
control <- rpart.control(minsplit = 5, minbucket = round(5/3),maxdepth = 3,cp=0)
tune_fit <- rpart(Property1~.,data = data_train,method = 'class')
accuracy_tune(tune_fit)

#Plotting the accuracy
plot(fit.HW3, uniform=TRUE,main="Classification Tree for HW3")
text(fit.HW3, use.n=TRUE, all=TRUE, cex=1)

#Pruning the tree as a function of cp (complexity parameter) to prevent over-fitting
pfit<-prune(fit.HW3,cp=.1)
plot(pfit, uniform=TRUE,main="Pruned Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


predict_seen <- predict(fit.HW3, data_train, type='class')
table_mat <- table(data_train$Property1,predict_seen)
accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
accuracy_Test
varImp(fit.HW3)
