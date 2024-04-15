# Random Forest Regression
HW3 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/HW2.csv", header=TRUE)
#HW3 <- as.data.frame(scale(HW3))

HW3 <- data.frame(HW3)

# Setting Seed
set.seed(101)
train = sample(1:nrow(HW3),75)
data_test <- HW3[-train,]
data_train <- HW3[train,]

# Random Forest Regression
model <- randomForest(Property~., data= data_train, importance= TRUE, na.action = na.omit , mtry = 6)
print(model)
plot(model)
test_pred <- predict(model, newdata = data_test)
plot(data_test[,1], test_pred)
abline(0,1)
data_test
sst <- sum((data_test[,1] - mean(data_test[,1]))^2)
ssr <- sum((test_pred - data_test[,1])^2)
rmse <- 1 - ssr/sst
rmse
