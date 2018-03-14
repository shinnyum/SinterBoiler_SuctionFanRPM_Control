###############################################
## Predict SteamYield with Linear Regression ##
###############################################

library(readxl)
SteamYield_prediction_train <- read_excel("D:/work/dataset/GY_ElectricPowerStation_TrainData.xlsx", sheet = "train")
SteamYield_prediction_test <- read_excel("D:/work/dataset/GY_ElectricPowerStation_TrainData.xlsx", sheet = "test")

SteamYield_prediction_train <- subset(SteamYield_prediction_train, select = c(-Date))
SteamYield_prediction_test <- subset(SteamYield_prediction_test, select = c(-Date))

##Input data
train.data.frame <- SteamYield_prediction_train
test.data.frame <- SteamYield_prediction_test

##Perform Linear Regression
SteamYield.LR <- lm(Sinter_WasteHeat_Boiler_SteamYield ~., data=train.data.frame)
summary(SteamYield.LR)

predicted.SteamYield.LR <- predict(SteamYield.LR, data=test.data.frame)
as.data.frame(cbind(predicted.SteamYield.LR, test.data.frame$Sinter_WasteHeat_Boiler_SteamYield))

##Linear Regression prediction plot with test.data.frame
plot(test.data.frame$Sinter_WasteHeat_Boiler_SteamYield,type="l",pch=16,col="blue",
     ylab="Sinter_WasteHeat_Boiler_SteamYield",xlab=NA,main="Linear Regression 모델",lwd=3)
lines(predicted.SteamYield.LR,col="red",lwd=3)

sqrt(mean((predicted.SteamYield.LR-test.data.frame$Sinter_WasteHeat_Boiler_SteamYield)^2))

##Check error rate of Linear Regression
error.rate.predicted.SteamYield.LR <- (abs(predicted.SteamYield.LR
                                           -test.data.frame$Sinter_WasteHeat_Boiler_SteamYield)
                                           /test.data.frame$Sinter_WasteHeat_Boiler_SteamYield)*100
mean.error.rate.predicted.SteamYield.LR <- mean(error.rate.predicted.SteamYield.LR)
print(mean.error.rate.predicted.SteamYield.LR)

#################################
## Predict SteamYield with SVR ##
#################################

install.packages("e1071")
library(e1071)

##Perform SVR
SteamYield.SVR <- svm(Sinter_WasteHeat_Boiler_SteamYield ~., kernal="radial", data=train.data.frame, cost = 1)
summary(SteamYield.SVR)

predicted.SteamYield.SVR <- predict(SteamYield.SVR, data=test.data.frame)
as.data.frame(cbind(predicted.SteamYield.SVR, test.data.frame$Sinter_WasteHeat_Boiler_SteamYield))

##SVR prediction plot with test.data.frame
plot(test.data.frame$Sinter_WasteHeat_Boiler_SteamYield,type="l",pch=16,col="blue",ylab="Sinter_WasteHeat_Boiler_SteamYield",xlab=NA,main="SVR 모델",lwd=4)
lines(predicted.SteamYield.SVR,col="red",lwd=4)

##Check error rate of SVR
error.rate.predicted.SteamYield.SVR <- (abs(predicted.SteamYield.SVR
                                            -test.data.frame$Sinter_WasteHeat_Boiler_SteamYield)
                                            /test.data.frame$Sinter_WasteHeat_Boiler_SteamYield)*100
error.rate.predicted.SteamYield.SVR <- mean(error.rate.predicted.SteamYield.SVR)
error.rate.predicted.SteamYield.SVR


#################################
## Predict SteamYield with H2O ##
#################################

install.packages("h2o")
install.packages("methods")
library(methods)
library(h2o)
h2o.init()

##Input data
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "Sinter_WasteHeat_Boiler_SteamYield"
x <- setdiff(names(train.data.frame.h2o),y)
# N <- length(NOx.Emission_Trainset) - 1

##Perform ANN
SteamYield.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Tanh", hidden = c(5,5))
SteamYield.h2o #결과보기

predicted.SteamYield.h2o <- h2o.predict(SteamYield.h2o, test.data.frame.h2o)
performance.SteamYield.h2o <- h2o.performance(SteamYield.h2o, test.data.frame.h2o)
performance.SteamYield.h2o #결과보기
as.data.frame(h2o.cbind(predicted.SteamYield.h2o$predict, test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield))

##ANN prediction plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.SteamYield.h2o$predict))
test.SteamYield <- as.data.frame(h2o.asnumeric(test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield))
test.SteamYield <- test.SteamYield[[1]]

##ANN prediction plot with test data
plot(test.SteamYield,type="l",pch=16,
     col="blue", ylab="Sinter_WasteHeat_Boiler_SteamYield",xlab=NA,main="DeepLearning Model",lwd=4)
lines(predict.line.h2o, col="red",lwd=4)

##Check error rate of ANN
error.rate.predicted.SteamYield.h2o <- (abs(predicted.SteamYield.h2o
                                            -test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield)
                                            /test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield)*100
error.rate.predicted.SteamYield.h2o <- mean(error.rate.predicted.SteamYield.h2o)
error.rate.predicted.SteamYield.h2o

