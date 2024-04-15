# Using both GPR and SVR to do final analysis

baseball <- baseballstats <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/baseball stats.csv")
baseball <- baseball[1:30,]

#Divide into 80% train and 20% test, with the cut randomly selected
baseballsort <- sort(sample(nrow(baseball) , nrow(baseball)*0.8))
baseball_train <- baseball[baseballsort,]
baseball_test <- baseball[-baseballsort,]

#Set up training and test predictor and predicted
runspergame_train <- baseball_train[,2]
runspergame_test <- baseball_test[,2]
descriptor_train <- baseball_train[,14:16]
descriptor_test <- baseball_test[,14:16]

#GPR Model
mdl <- gausspr(descriptor_train , runspergame_train)
predicted_train <- predict(mdl, descriptor_train)
predicted_test <- predict(mdl , descriptor_test)

library(hydroGOF)
runspergame_train_m <- as.matrix(runspergame_train)
rmse_gpr_train <- rmse(predicted_train,runspergame_train_m)
runspergame_test_m <- as.matrix(runspergame_test)
rmse_gpr_test <- rmse(predicted_test,runspergame_test_m)

rmse_gpr_train
rmse_gpr_test

# Trying with all descriptors
descriptors_all_train <- baseball_train[,3:16]
descriptors_all_train <- as.matrix(descriptors_all_train)
descriptors_all_test <- baseball_test[,3:16]

mdl1 <- gausspr(descriptors_all_train,runspergame_train)
pred_train <- predict(mdl1 , descriptors_all_train)
pred_test <- predict(mdl1, descriptors_all_test)
rmse_gpr_train_all <- rmse(pred_train,runspergame_train_m)
rmse_gpr_test_all <- rmse(pred_test,runspergame_test_m)

rmse_gpr_train_all
rmse_gpr_test_all

# Using SVM Model
optmodelsvm <- tune(svm, runspergame_train_m~descriptors_all_train, ranges = list(epsilon = seq(0,1,0.1),cost = 1:100))
bstModel <- optmodelsvm$best.model
predybestmodel <- predict(bstModel, descriptors_all_train)
predybest_test <- predict(bstModel, descriptors_all_test)[1:6]
rmse_svm_train <- rmse(predybestmodel, runspergame_train)
rmse_svm_train
predybestmodel
runspergame_train_m
descriptors_all_train
rmse_test <- rmse(predybest_test, runspergame_test)
rmse_test
