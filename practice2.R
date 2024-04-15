homework1 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/homework1.csv", header=TRUE)
summary(homework1)
is.na(homework1)
homework1 <- as.data.frame(scale(homework1))
hw_std <- homework1
target <- hw_std[,1]
desc <- hw_std[,-1]
pca1 <- prcomp(hw_std)
View(pca1)
scores <- pca1$x
loads <- pca1$rotation
pca1$sdev
plot(pca1$sdev)
sum(pca1$sdev)
sum(pca1$sdev[1:4])
# we choose 4 descriptors as their sum is over 90%.
loads_prop <- loads[1,1:4]
loads_desc <- loads[-1, 1:4]
weights <- loads_prop * loads_desc
vip <- weights[,1] + weights[,2] + weights[,3] + weights[,4]  
barplot(vip)

#using the features 2 , 3 , 14 , 15 for linear regression
linear_model <- lm(Property1 ~ Feature2+ Feature3 + Feature14 + Feature15 , data = hw_std)
summary(linear_model)
linear_model1 <- lm(Property1~., data = hw_std)
summary(linear_model1)

svm_model <- svm(Property1 ~ Feature2+ Feature3 + Feature14 + Feature15 , data = hw_std)
summary(model_svm)
pre_prop <- predict(svm_model , newdata = hw_std)
pre_prop
rmsesvm <- rmse(pre_prop, hw_std$Property1)
rmsesvm
plot(pre_prop,hw_std$Property1)

svm_cont <- svm(Property1~. , data = hw_std)
svm_cont
pre_prop_all <- predict(svm_cont , newdata = hw_std)
rmsev <- rmse(pre_prop_all, hw_std$Property1)
rmsev
