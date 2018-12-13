#  ==================================================================================
#   * Program Name       	: 
#   * Source File Name   	: SteamYield_RandomSearchMethod_ANN.R
#   * Author             	: 
#   * Version           	: 1.0.0
#   * Created Date       	: 2018-07-03
#   * Updated Date      	: 2018-07-03
#   * Last modifier      	: 
#   * Updated content    	: 최초작성
#   * Description       	: 실행주기(-)
#  ==================================================================================

# -----------------------------------------------------------------------------------
# Check SteamYield prediction performance SteamYield with MLP
# -----------------------------------------------------------------------------------

#install.packages("readxl")
library(readxl)
SteamYield_prediction_train <- read_excel("D:/work/dataset/GY_num1_SteamPowerStation_TrainData.xlsx", sheet = "train")
SteamYield_prediction_test <- read_excel("D:/work/dataset/GY_num1_SteamPowerStation_TrainData.xlsx", sheet = "test")

SteamYield_prediction_train <- subset(SteamYield_prediction_train, select = c(5:11))
SteamYield_prediction_test <- subset(SteamYield_prediction_test, select = c(5:11))

## General input
train.data.frame <- SteamYield_prediction_train
test.data.frame <- SteamYield_prediction_test

# #-------------------------------------------------------------------#
# #full_data <- rbind(RD_prediction_train, RD_prediction_test)
# 
# ## 80% of the sample size
# #smp_size <- floor(0.80 * nrow(full_data))
# 
# ## set the seed to make your partition reproductible
# set.seed(1)
# train_index <- sample(seq_len(nrow(full_data)), size = smp_size)
# 
# ##Input data
# train.data.frame <- full_data[train_index,]
# test.data.frame <- full_data[-train_index,]
# 
# ##Input data
# #train.data.frame <- RD_prediction_train
# #test.data.frame <- RD_prediction_test
# 
# 
# ##Abnormality detection
# # for(i in 1:length(predicted.RD.LR)){
# #   if(predicted.RD.LR[i] > 100){
# #     predicted.RD.LR[i] <- mean(RD_prediction_train$RD)
# #   }
# # }
# # 
# # as.data.frame(cbind(predicted.RD.LR, test.data.frame$RD))
# #-------------------------------------------------------------------#

#install.packages("h2o")
#install.packages("h2o_3.16.5.3.tar.gz", repos=NULL, type="source")
#install.packages("methods")
library(h2o)
library(methods)

h2o.init(max_mem_size = '5g',nthreads=-1)

## h2o input
train.data.frame.h2o <- as.h2o(train.data.frame)
test.data.frame.h2o <- as.h2o(test.data.frame)

y <- "Sinter_WasteHeat_Boiler_SteamYield"
x <- setdiff(names(train.data.frame.h2o),y)
# N <- length(NOx.Emission_Trainset) - 1

## Perform MLP
SteamYield.h2o <- h2o.deeplearning(x, y, train.data.frame.h2o, activation = "Tanh", hidden = c(5,5),
                                   export_weights_and_biases = TRUE, standardize = TRUE)
SteamYield.h2o #결과보기

predicted.SteamYield.h2o <- h2o.predict(SteamYield.h2o, test.data.frame.h2o)
performance.SteamYield.h2o <- h2o.performance(SteamYield.h2o, test.data.frame.h2o)
performance.SteamYield.h2o #결과보기
as.data.frame(h2o.cbind(predicted.SteamYield.h2o$predict, test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield))

## Resiual plot with test.data.frame
predict.line.h2o <- as.data.frame(h2o.cbind(predicted.SteamYield.h2o$predict))
test.SteamYield <- as.data.frame(h2o.asnumeric(test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield))
test.SteamYield <- test.SteamYield[[1]]

residuals <- predicted.SteamYield.h2o$predict - test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield
compare = cbind(as.data.frame(test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield),as.data.frame(residuals$predict))

plot(compare[,1:2])

## MLP prediction plot with test data
plot(test.SteamYield,type="l",pch=16,col="blue",
     ylab="SteamYield",xlab=NA,main="Simple MLP model",lwd=3)
lines(predict.line.h2o, col="red",lwd=3)

## Check error rate(MAPE/RMSE) of ANN
train.predicted.SteamYield.h2o <- h2o.predict(SteamYield.h2o, train.data.frame.h2o)
train.error.rate.predicted.SteamYield.h2o <- (abs(train.predicted.SteamYield.h2o
                                             -train.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield)
                                             /train.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield)*100
train.error.rate.predicted.SteamYield.h2o <- mean(train.error.rate.predicted.SteamYield.h2o)
test.error.rate.predicted.SteamYield.h2o <- (abs(predicted.SteamYield.h2o
                                            -test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield)
                                        /test.data.frame.h2o$Sinter_WasteHeat_Boiler_SteamYield)*100
test.error.rate.predicted.SteamYield.h2o <- mean(test.error.rate.predicted.SteamYield.h2o)

writeLines(c(paste('train MAPE : ', as.data.frame(round(train.error.rate.predicted.SteamYield.h2o, 2)), '%'),
             paste('test  MAPE : ', as.data.frame(round(test.error.rate.predicted.SteamYield.h2o, 2)), '%'),
             paste('train RMSE : ', round(h2o.rmse(SteamYield.h2o),2)),
             paste('test  RMSE : ', round(h2o.rmse(performance.SteamYield.h2o),2))))


# -----------------------------------------------------------------------------------
# Hyper parameter tuning - Random Search Method
# -----------------------------------------------------------------------------------
install.packages("reshape")
library(data.table)

run <- function(seed, name = paste0("m_", seed), run = TRUE) {
  set.seed(seed)
  
  p <- list(
    Name = name,
    seed = seed,
    depth = sample(1:10, 1),
    l1 = runif(1, 0, .01),
    l2 = runif(1, 0, .01),
    input_dropout = rbeta(1, 1, 12),
    rho = runif(1, .9, .999),
    epochs = sample(c(10,50),1),
    epsilon = runif(1, 1e-10, 1e-4))
  p$neurons <- sample(5:10, p$depth, TRUE)   
  p$hidden_dropout <- rbeta(p$depth, 1.5, 1)/5
  
  if (run) {
    model <- h2o.deeplearning(
      x = setdiff(names(train.data.frame.h2o),y),
      y = "Sinter_WasteHeat_Boiler_SteamYield",
      training_frame = train.data.frame.h2o,
      #activation = "RectifierWithDropout",
      activation = "TanhWithDropout",
      hidden = p$neurons,
      epochs = p$epochs,
      loss = "Automatic",
      input_dropout_ratio = p$input_dropout,
      hidden_dropout_ratios = p$hidden_dropout,
      l1 = p$l1,
      l2 = p$l2,
      rho = p$rho,
      epsilon = p$epsilon,
      export_weights_and_biases = TRUE,
      model_id = p$Name,
      nfolds = 4, seed = 0xDECAF
    )
    
    ## performance of training data
    p$MSE <- h2o.mse(model)
    p$R2 <- h2o.r2(model)
    p$Logloss <- h2o.logloss(model)
    p$CM <- h2o.confusionMatrix(model)
    
    ## performance of testing data
    perf <- h2o.performance(model,  h2o.rbind(h2odigits.train, h2odigits.test))  # h2o.rbind(train4, test4)濡? ?닔?젙
    p$T.MSE <- h2o.mse(perf)
    p$T.R2 <- h2o.r2(perf)
    p$T.Logloss <- h2o.logloss(perf)
    p$T.CM <- h2o.confusionMatrix(perf)
    
  } else {
    model <- NULL
  }
  
  return(list(
    Params = p,
    Model = model))
}


h2odigits.train <- train.data.frame.h2o
h2odigits.test <- test.data.frame.h2o

use.seeds = c(403574L, 3237957L, -7531021L, 123L, 6824L) #4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

system.time(model.res <- lapply(use.seeds, run))

model.res.dat <- do.call(rbind, lapply(model.res,
                                       function(x) with(x$Params,
                                                        data.frame(l1 = l1, l2 = l2,
                                                                   depth = depth, input_dropout = input_dropout,
                                                                   SumNeurons = sum(neurons),
                                                                   MeanHiddenDropout = mean(hidden_dropout),
                                                                   rho = rho, epsilon = epsilon, 
                                                                   epochs = epochs, 
                                                                   #sparsity = sparsity,  
                                                                   MSE = T.MSE))))

str(model.res.dat)

par(mfrow = c(2, 2))
plot(model.res.dat$l1, model.res.dat$l2, xlab="L1", ylab="L2")
plot(model.res.dat$depth , model.res.dat$input_dropout, xlab="depth", ylab="input_dropout")
plot(model.res.dat$rho , model.res.dat$epsilon , xlab="rho ", ylab="epsilon ")
plot(model.res.dat$SumNeurons, model.res.dat$MeanHiddenDropout, xlab="SumNeurons", ylab="MeanHiddenDropout")

model.res.dat[which.min(model.res.dat$MSE), ]

plot(sort(model.res.dat$MSE,decreasing = TRUE), type="b", ylab="Mean Square Errors", xlab="The number of model", 
     lwd=2, cex=0.8, col="red", main="N=5")
abline(v=0, lty=2);abline(v=1, lty=2);abline(v=2, lty=2);abline(v=3, lty=2);abline(v=4, lty=2);abline(v=5, lty=2);abline(v=20, lty=2);


library(ggplot2);library(reshape)
p.perf <- ggplot(melt(model.res.dat, id.vars = c("MSE")), aes(value, MSE)) +
  geom_point() +
  stat_smooth(colour = "black") +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  theme_classic()
print(p.perf)


#install.packages("mgcv")
library(mgcv)
m.gam <- gam(MSE ~ s(l1, k=2) +
               s(l2, k=2),
               #s(input_dropout) +
               #s(rho, k=2)
               #s(epsilon, k=2) +
               #s(MeanHiddenDropout, k=2),
             #s(epochs, k = 1),
             #s(sparsity, k = 4) +
             #ti(depth, SumNeurons, k = 2) +
             #te(depth, SumNeurons, k = 2),
             data = model.res.dat)

# m.gam <- gam(MSE ~ s(l1, k = 4) +
#              s(l2, k = 4) +
#              s(input_dropout) +
#              s(rho, k = 4) +
#              s(epsilon, k = 4) +
#              s(MeanHiddenDropout, k = 4) +
#              #s(epochs, k = 4) +
#              #s(sparsity, k = 4) +
#              #ti(depth, SumNeurons, k = 4),
#              te(depth, SumNeurons, k = 4),
#            data = model.res.dat)


par(mfrow = c(3, 2))
for (i in 1:6) {
  plot(m.gam, select = i, cex.axis=1.4, cex.lab=1.4)
}


plot(m.gam, select = 5, cex.axis=1.2, cex.lab=1.4, cex=1, pch="*")
plot(m.gam, select = 5)
dev.off()


model.optimized <- h2o.deeplearning(
  x = setdiff(names(train.data.frame.h2o),y),
  y = "Sinter_WasteHeat_Boiler_SteamYield",
  training_frame = h2odigits.train ,
  activation = "TanhWithDropout",
  hidden = c(500, 100, 500),
  epochs = 100,
  loss = "CrossEntropy",
  input_dropout_ratio = 0.1745018 ,
  hidden_dropout_ratios = c(0.05944409, 0.05944409, 0.05944409),
  l1 = 0.007450913,
  l2 = 0.002124112,
  rho = 0.9114875,
  epsilon = 1.745123e-05 ,
  export_weights_and_biases = TRUE,
  model_id = "optimized_model"
)

h2o.performance(model.optimized, h2oactivity.test)

