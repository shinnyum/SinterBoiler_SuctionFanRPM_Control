#  ==================================================================================
#   * Program Name       	: 소결보일러 Suction fan speed 결정모델 SUB RSCRIPT
#   * Source File Name   	: SteamYield_ANN_Model.R
#   * Author             	: 
#   * Version           	: 1.0.0
#   * Created Date       	: 2018-00-00
#   * Updated Date      	: 2018-00-00
#   * Last modifier      	: 
#   * Updated content    	: 최초작성
#   * Description       	: 실행주기(1분)
#  ==================================================================================

# -----------------------------------------------------------------------------------
# Data preparation
# -----------------------------------------------------------------------------------
# h2o Input data
h2o.init()
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "Sinter_WasteHeat_Boiler_SteamYield"
x <- setdiff(names(train.data.frame.h2o),y)
# N <- length(NOx.Emission_Trainset) - 1


# -----------------------------------------------------------------------------------
# DNN training
# -----------------------------------------------------------------------------------
SteamYield.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Tanh", hidden = c(5,5),
                                   export_weights_and_biases = TRUE, standardize = TRUE)
SteamYield.h2o #결과보기

predicted.SteamYield.h2o <- h2o.predict(SteamYield.h2o, test.data.frame.h2o)
performance.SteamYield.h2o <- h2o.performance(SteamYield.h2o, test.data.frame.h2o)
performance.SteamYield.h2o #결과보기
as.data.frame(h2o.cbind(predicted.SteamYield.h2o$predict, test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield))


# -----------------------------------------------------------------------------------
# Check performance
# -----------------------------------------------------------------------------------
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.SteamYield.h2o$predict))
test.SteamYield <- as.data.frame(h2o.asnumeric(test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield))
test.SteamYield <- test.SteamYield[[1]]

# ANN prediction plot with test data
plot(test.SteamYield[1:numOfDataSet],type="l",pch=16,
     col="blue", ylab="Sinter_WasteHeat_Boiler_SteamYield",xlab=NA,main="DNN Model",lwd=4)
lines(predict.line.h2o, col="red",lwd=4)

# Check error rate(MAE) of ANN
error.rate.predicted.SteamYield.h2o <- (abs(predicted.SteamYield.h2o
                                            -test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield)
                                        /test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield)*100
error.rate.predicted.SteamYield.h2o <- mean(error.rate.predicted.SteamYield.h2o)
error.rate.predicted.SteamYield.h2o


print(paste('ANN Percentage Error :', error.rate.predicted.SteamYield.h2o, '%'))
print("Model construction complete.")

