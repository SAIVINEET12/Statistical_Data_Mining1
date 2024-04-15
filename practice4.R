#Kernel Regression
data <- data.frame(Area = c(11,22,33,44,50,56,67,70,78,89,90,100), Riverflow = c(2337,2750,2301,2500,1700,2100,1100,1750,1000,1642,2000,1932))
x <- data$Area
y <- data$Riverflow #Function to calculate Guassian Kernel
gausinkernel <- function(x,b){
  K <- (1/(sqrt(2*pi)))*exp(-0.5*(x/b)^2)
  return(K)
}
b<- 10 #bandwidth
kdeEstimateYX <- seq(5,110,1)
ykernel <- NULL
for(xesti in kdeEstimateYX){
  xx <- xesti - x
  K<- gausinkernel(xx,b)
  ksum <- sum(K)
  weight <- K/ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y,xlab = 'Area',ylab = 'Flow',col = 'blue',cex = 2)
lines(ykernel[,1],ykernel[,2],col='red',lwd = 2)


