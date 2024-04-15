HW3 <- read.csv("F:/1_MASTERS/FALL 21/EAS 508 Statistical Data Mining In R/HW/HW2.csv", header=TRUE)
#HW3 <- as.data.frame(scale(HW3))

# Using property 1 as categorical for predictions
Property1 <- ifelse(HW3$Property > 7.835,'Yes','No')
summary(Property1)
HW3 <- data.frame(HW3, Property1)
HW3 <- HW3[,-1]

# Installing the rpart package and calling it
library(rpart)
library(rpart.plot)

# Building of tree using Rpart function
fit <- rpart(Property1~.,HW3)
rpart.plot(fit)
summary(fit)
plot(fit)
text(fit,pretty = 1)

