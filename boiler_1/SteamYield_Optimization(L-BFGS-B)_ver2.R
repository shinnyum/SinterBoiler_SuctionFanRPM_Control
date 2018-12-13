#  ==================================================================================
#   * Program Name       	: 소결보일러 Suction fan speed 결정모델 SUB RSCRIPT
#   * Source File Name   	: SteamYield_Optimization(L-BFGS-B)_ver2.R
#   * Author             	: 
#   * Version           	: 1.0.0
#   * Created Date       	: 2018-00-00
#   * Updated Date      	: 2018-00-00
#   * Last modifier      	: 
#   * Updated content    	: 최초작성
#   * Description       	: 실행주기(1분)
#  ==================================================================================

# -----------------------------------------------------------------------------------
# Define ANN Closed-form function(1)
# -----------------------------------------------------------------------------------
Closed.form.function.1 <- function(x){
  
  X.input <- as.matrix(subset(test.data.frame[i,], select=c(-Sinter_WasteHeat_Boiler_SteamYield)))
  X.input[,5] <- x[1]
 #X.input[,4] <- x[2]
  
  train.X.data <- as.matrix(subset(train.data.frame, select=c(-Sinter_WasteHeat_Boiler_SteamYield)))
  normalized.X.input <- NULL
  dataFrame.Y.output <- NULL
  mean.X.input <- NULL
  std.X.input <- NULL
  
  for(i in 1:ncol(train.X.data)){
    mean.data <-apply(as.data.frame(train.X.data[,i]), MARGIN = 2, FUN = function(x)(mean(x, na.rm = TRUE)))
    std.data <-apply(as.data.frame(train.X.data[,i]), MARGIN = 2, FUN = function(x)(sd(x, na.rm = TRUE)))
    mean.X.input <- cbind(mean.X.input, mean.data)
    std.X.input <- cbind(std.X.input, std.data)
  }
  
  for(i in 1:ncol(X.input)){
    normalized.data <- apply(as.data.frame(X.input[,i]), MARGIN = 2, 
                             FUN = function(x, na.rm = TRUE)((x - mean.X.input[,i]) / std.X.input[,i]))
    normalized.X.input <- cbind(normalized.X.input, normalized.data)
  }
  
  for(i in 1:nrow(X.input)){
    input.x.matrix <- matrix(normalized.X.input[i,], nrow = 1)
    first.hidden.layer.beforeA.F. <- input.x.matrix %*% first.Layer.Weights_Num.vector + first.Layer.Biases_Num.vector
    first.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(first.hidden.layer.beforeA.F.)))
    second.hidden.layer.beforeA.F. <- first.hidden.layer.afterA.F. %*% second.Layer.Weights_Num.vector + second.Layer.Biases_Num.vector
    second.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(second.hidden.layer.beforeA.F.)))
    third.hidden.layer.beforeA.F. <- second.hidden.layer.afterA.F. %*% third.Layer.Weights_Num.vector + third.Layer.Biases_Num.vector
    Y.output <- third.hidden.layer.beforeA.F.
    dataFrame.Y.output <- rbind(dataFrame.Y.output, Y.output)
  }
  
  mean.Y.output <- mean(train.data.frame$Sinter_WasteHeat_Boiler_SteamYield, na.rm = TRUE)
  std.Y.output <- sd(train.data.frame$Sinter_WasteHeat_Boiler_SteamYield, na.rm = TRUE)
  normalized.Y.output <- dataFrame.Y.output
  inverseTransform.normalized.Y.output <- apply(as.data.frame(normalized.Y.output), MARGIN = 2,
                                                FUN = function(x, na.rm = TRUE)((x * std.Y.output) + mean.Y.output))
  
  # Setting the "Target.SteamYield" (40 T/Hr)
  target.SteamYield <- 40
  
  # Define "Gap" between target value and predicted value
  predicted.SteamYield <- inverseTransform.normalized.Y.output
  gap.SteamYield <- abs(target.SteamYield - predicted.SteamYield)
  
  return(gap.SteamYield)
}


# -----------------------------------------------------------------------------------
# Define ANN Closed-form function(2)
# -----------------------------------------------------------------------------------
Closed.form.function.2 <- function(x){
  
  X.input <- as.matrix(subset(test.data.frame[i,], select=c(-Sinter_WasteHeat_Boiler_SteamYield)))
  X.input[,5] <- x[1]
 #X.input[,4] <- x[2]
  
  train.X.data <- as.matrix(subset(train.data.frame, select=c(-Sinter_WasteHeat_Boiler_SteamYield)))
  normalized.X.input <- NULL
  dataFrame.Y.output <- NULL
  mean.X.input <- NULL
  std.X.input <- NULL
  
  for(i in 1:ncol(train.X.data)){
    mean.data <-apply(as.data.frame(train.X.data[,i]), MARGIN = 2, FUN = function(x)(mean(x, na.rm = TRUE)))
    std.data <-apply(as.data.frame(train.X.data[,i]), MARGIN = 2, FUN = function(x)(sd(x, na.rm = TRUE)))
    mean.X.input <- cbind(mean.X.input, mean.data)
    std.X.input <- cbind(std.X.input, std.data)
  }
  
  for(i in 1:ncol(X.input)){
    normalized.data <- apply(as.data.frame(X.input[,i]), MARGIN = 2, 
                             FUN = function(x, na.rm = TRUE)((x - mean.X.input[,i]) / std.X.input[,i]))
    normalized.X.input <- cbind(normalized.X.input, normalized.data)
  }
  
  for(i in 1:nrow(X.input)){
    input.x.matrix <- matrix(normalized.X.input[i,], nrow = 1)
    first.hidden.layer.beforeA.F. <- input.x.matrix %*% first.Layer.Weights_Num.vector + first.Layer.Biases_Num.vector
    first.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(first.hidden.layer.beforeA.F.)))
    second.hidden.layer.beforeA.F. <- first.hidden.layer.afterA.F. %*% second.Layer.Weights_Num.vector + second.Layer.Biases_Num.vector
    second.hidden.layer.afterA.F. <- 1 - 2 / (1 + exp(2*(second.hidden.layer.beforeA.F.)))
    third.hidden.layer.beforeA.F. <- second.hidden.layer.afterA.F. %*% third.Layer.Weights_Num.vector + third.Layer.Biases_Num.vector
    Y.output <- third.hidden.layer.beforeA.F.
    dataFrame.Y.output <- rbind(dataFrame.Y.output, Y.output)
  }
  
  mean.Y.output <- mean(train.data.frame$Sinter_WasteHeat_Boiler_SteamYield, na.rm = TRUE)
  std.Y.output <- sd(train.data.frame$Sinter_WasteHeat_Boiler_SteamYield, na.rm = TRUE)
  normalized.Y.output <- dataFrame.Y.output
  inverseTransform.normalized.Y.output <- apply(as.data.frame(normalized.Y.output), MARGIN = 2, 
                                                FUN = function(x, na.rm = TRUE)((x * std.Y.output) + mean.Y.output))
  
  # Define predicted.SteamYield
  predicted.SteamYield <- inverseTransform.normalized.Y.output
  
  return(predicted.SteamYield)
}


# -----------------------------------------------------------------------------------
# E - Damper Open Rate Decision Logic
# -----------------------------------------------------------------------------------
FinalOutput.Data.Frame <- NULL

for(i in 1:numOfDataSet){

EdamperOpenRate <- 0

if(test.data.frame[i,"SteamReceiver_AfterPressure"] >= 11.5 && test.data.frame[i,"SteamReceiver_AfterPressure"] < 12){
    EdamperOpenRate <- as.numeric((test.data.frame[i,"SteamReceiver_AfterPressure"] * 50) - 550)
    predicted.SuctionFan.RPM_optim <- as.numeric((test.data.frame[i,"SteamReceiver_AfterPressure"] * -160) + 2220)
    predicted.SteamYield_optim <- Closed.form.function.2(as.numeric(predicted.SuctionFan.RPM_optim))
    
  } else if(test.data.frame[i,"SteamReceiver_AfterPressure"] >= 12 && test.data.frame[i,"SteamReceiver_AfterPressure"] < 12.5){
    EdamperOpenRate <- as.numeric((test.data.frame[i,"SteamReceiver_AfterPressure"] * 100) - 1150)
    predicted.SuctionFan.RPM_optim <- as.numeric((test.data.frame[i,"SteamReceiver_AfterPressure"] * -300) + 3900)
    predicted.SteamYield_optim <- Closed.form.function.2(as.numeric(predicted.SuctionFan.RPM_optim))

  } else if(test.data.frame[i,"SteamReceiver_AfterPressure"] >= 12.5 || test.data.frame[i,"SteamDrum_Pressure"] >= 13.5){
    EdamperOpenRate <- 100
    predicted.SuctionFan.RPM_optim <- 150
    predicted.SteamYield_optim <- Closed.form.function.2(as.numeric(predicted.SuctionFan.RPM_optim))
    
  } else{

    
# -----------------------------------------------------------------------------------
# Perform Optimization using "L-BFGS-B" method
# -----------------------------------------------------------------------------------
# Set initial values of Decision Variables
initial_par <- c(300) #,10)
upperbound <- rnorm(1, 465, 8); upperbound <- upperbound[upperbound > 400 & upperbound < 480]
  
  # Using "optim" package to minimize GAP value
  optim_Result <- optim(initial_par, Closed.form.function.1 , NULL, method = "L-BFGS-B", 
                        lower = c(100), #,10),
                        upper = c(upperbound), #,12),
                        hessian = FALSE) #control = list(fnscale = -1))
  initial_par <- optim_Result$par
  
  predicted.SuctionFan.RPM_optim <- optim_Result$par[[1]]
  predicted.SteamYield_optim <- Closed.form.function.2(initial_par)


  # Case 1 : 각각의 Steam Receiver 후단압력의 Case별로 나누어서 최적화모델 실행
  # Case 2 : 다음값 압력이 점점 높아지면 Panalty 적용?
  

  # if(test.data.frame[i,"EmergencyDamper_OpenRate"] >= 70){
  #       predicted.SuctionFan.RPM_optim <- optim_Result$par[[1]] * 0.55
  #       predicted.SteamYield_optim <- Closed.form.function.2(c(predicted.SuctionFan.RPM_optim, optim_Result$par[[2]]))
  #   }
  #     else if(test.data.frame[i,"EmergencyDamper_OpenRate"] >= 50){
  #       predicted.SuctionFan.RPM_optim <- optim_Result$par[[1]] * 0.75
  #       predicted.SteamYield_optim <- Closed.form.function.2(c(predicted.SuctionFan.RPM_optim, optim_Result$par[[2]]))
  # 
  #   }
  #     else if(test.data.frame[i,"EmergencyDamper_OpenRate"] >= 30){
  #       predicted.SuctionFan.RPM_optim <- optim_Result$par[[1]] * 0.80
  #       predicted.SteamYield_optim <- Closed.form.function.2(c(predicted.SuctionFan.RPM_optim, optim_Result$par[[2]]))
  #       
  #   }
  #     else{
  #       predicted.SteamYield_optim <- Closed.form.function.2(initial_par)
  #       
  #   }
  }

  # Print Output Values
  writeLines(c(paste('[', i, 'th OUTPUT VALUES ]'),
               paste('Steam Receiver Pressure :', test.data.frame$SteamReceiver_AfterPressure[i], 'kg/cm2'),
               paste('Steam Drum Pressure :', test.data.frame$SteamDrum_Pressure[i], 'kg/cm2'),
               paste('Optimal E-Damper Open Rate :', EdamperOpenRate, '%'),
               paste('Actual E-Damper Open Rate :', test.data.frame$EmergencyDamper_OpenRate[i], '%'),
               paste('Optimal SuctionFan RPM :', predicted.SuctionFan.RPM_optim),
               paste('Actual SuctionFan RPM :', test.data.frame$SuctionFan_RPM[i]),
               # paste('BFG_FUEL_GAS_FLOW :', test.data.frame$BFG_FUEL_GAS_FLOW_CONTROL[i]),
               # paste('LDG_FUEL_GAS_FLOW :', test.data.frame$LDG_FUEL_GAS_FLOW_CONTROL[i]),
               # paste('Combustion_AIR_Flow :', optim_Result$par[[2]]),
               # paste('Feed_Water_Flow :', optim_Result$par[[3]]),
               # paste('SH_Spray_Flow_Sum :', optim_Result$par[[4]]),
               # paste('Make-Up_Water_Flow :', optim_Result$par[[5]]),
               # paste('BFG_Temperature :', optim_Result$par[[6]]),
               # paste('Gap_Between_Target_NOx_Emission :', optim_Result$value),
               paste('Predicted Steam Yield :', predicted.SteamYield_optim, 'T/hr'),
               paste('Actual Steam Yield :', test.data.frame$Sinter_WasteHeat_Boiler_SteamYield[i], 'T/hr'),
               paste(' ')))
  
  # Save Output Values
  Output.Data.Frame <- data.frame("Steam_Receiver_Pressure" = test.data.frame$SteamReceiver_AfterPressure[i],
                                  "Steam_Drum_Pressure" = test.data.frame$SteamDrum_Pressure[i],
                                  "Optimal_Edamper_Open_Rate" = EdamperOpenRate,
                                  "Actual_Edamper_Open_Rate" = test.data.frame$EmergencyDamper_OpenRate[i],
                                  "Optimal_SuctionFan_RPM" = predicted.SuctionFan.RPM_optim,
                                  "Actual_SuctionFan_RPM" = test.data.frame$SuctionFan_RPM[i],
                                  # "BFG_FUEL_GAS_FLOW" = test.data.frame$BFG_FUEL_GAS_FLOW_CONTROL[i],
                                  # "LDG_FUEL_GAS_FLOW" = test.data.frame$LDG_FUEL_GAS_FLOW_CONTROL[i],
                                  # "Combustion_AIR_Flow" = optim_Result$par[[2]],
                                  # "Feed_Water_Flow" = optim_Result$par[[3]],
                                  # "SH_Spray_Flow_Sum" = optim_Result$par[[4]],
                                  # "Make-Up_Water_Flow" = optim_Result$par[[5]],
                                  # "BFG_Temperature" = optim_Result$par[[6]]),
                                  "Predicted_Steam_Yield" =  predicted.SteamYield_optim,
                                  "Actual_Steam_Yield" = test.data.frame$Sinter_WasteHeat_Boiler_SteamYield[i])
  
  FinalOutput.Data.Frame <- rbind(FinalOutput.Data.Frame, Output.Data.Frame)
  
}

#save.image(file = "D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/boiler_1/SuctionFanSpeed.RData")
#rdata <- load("D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/boiler_1/SuctionFanSpeed.RData")
#write.csv(rdata, "D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/boiler_1/SuctionFanSpeed.csv")

write_xlsx(FinalOutput.Data.Frame, "D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/Output/Optimal SuctionFan RPM_boiler1.xlsx")
print("SuctionFan RPM optimization complete.")

