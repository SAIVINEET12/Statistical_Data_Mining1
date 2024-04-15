homework1 <- homework1[1:40,1:21]
usedata <- homework1
summary(homework1)
hw_std <- as.data.frame(scale(usedata))
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
#hence we choose features 2 , 3 , 14 , 15 for our principal component regression based on barplot


#we do pca on desc table as it does not contain the target property
pcr <- prcomp(desc)
View(pcr)
pcr
plot(pcr$sdev)
#using the 4 pcas for regression
scores_pcr <- pcr$x[,1:4]
pcrmodel <- lm(target ~ scores_pcr)
pcrmodel
summary(pcrmodel)


# trying svr on the same

model_svm <- svm(target ~ scores_pcr)
pred_prop <- predict(model_svm)
pred_prop
plot(pred_prop , target)
summary(model_svm)
w = t(model_svm$coefs)
w
b <- t(model_svm$rho)
b
optmodelsvm <- tune(svm, target~scores_pcr , ranges = list(epsilon = seq(0,1,0.1), cost = 1:100))
summary(optmodelsvm)
print(optmodelsvm)
rmsesvm <- rmse(pred_prop, target)
pred_prop_tuned <- predict(optmodelsvm$best.model)
rmsesvm_tuned <- rmse(pred_prop_tuned, target)
