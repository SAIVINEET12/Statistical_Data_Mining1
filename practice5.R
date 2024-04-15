#GPR on baseball dataset without training and test split
baseballstats <- baseballstats[1:30,]
runspergame <- baseballstats[,2]
runspergame <- as.matrix(runspergame)
predictors <- baseballstats[,14:16]
library(kernlab)
mdl <- gausspr(predictors, runspergame,variance.model = TRUE)
plot(runspergame, predict(mdl, predictors) , xlab = 'actual value', ylab = 'Predicted value')

# R^2 calculation
sst <- sum((runspergame - mean(runspergame))^2)
ssr <- sum((runspergame - predict(mdl,predictors))^2)
R2 <- 1 - (ssr/sst)
R2
plot(baseballstats[,14],predict(mdl,predictors),xlab = 'batting avg', ylab='Runspergame') 
lines(baseballstats[,14], predict(mdl,predictors)+ predict(mdl,predictors , type='sdeviation'),col='red',lwd = 2)  
lines(baseballstats[,14], predict(mdl,predictors)+ 2*predict(mdl,predictors , type='sdeviation'),col='red')  
lines(baseballstats[,14], predict(mdl,predictors)- 2*predict(mdl,predictors , type='sdeviation'),col='red') 

