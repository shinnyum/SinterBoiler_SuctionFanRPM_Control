#  ==================================================================================
#   * Program Name       	: 
#   * Source File Name   	: SteamYield_Compisition_Code.R
#   * Author             	: 
#   * Version           	: 1.0.0
#   * Created Date       	: 2017-11-20
#   * Updated Date      	: 2017-11-20
#   * Last modifier      	: 
#   * Updated content    	: 최초작성
#   * Description       	: 실행주기(1분)
#  ==================================================================================

# -----------------------------------------------------------------------------------
# Package 라이브러리 로딩
# -----------------------------------------------------------------------------------

#install.packages("h2o")
#install.packages("methods")
#install.packages("evd")
#install.packages("xlsx")
#install.packages(c("readxl","writexl"))
library(h2o)
library(methods)
library(evd)
library(readxl)
library(writexl)

## Read data from Excel worksheets
SteamYield_Trainset <- read_excel("D:/work/dataset/GY_num1_SteamPowerStation_TrainData.xlsx", sheet = "train")
SteamYield_Testset <- read_excel("D:/work/dataset/GY_num1_SteamPowerStation_TrainData.xlsx", sheet = "test")

## Input data
SteamYield_Trainset <- subset(SteamYield_Trainset, select = c(5:11))
SteamYield_Testset <- subset(SteamYield_Testset, select = c(5:11))
train.data.frame <- SteamYield_Trainset
test.data.frame <- SteamYield_Testset

## Construct ANN Model
source("D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/boiler_1/SteamYield_ANN_Model.R")

## Extract ANN's "Closed-form Formula (Feed-forward propagation) Formula"
source("D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/boiler_1/SteamYield_MultiLayer_Extracted_ANN_ClosedFormFormula.R")
# save.image(file = "D:/work/Rcode/BoilerEFF/NOx_Prediction/Average_O2/BoilerEFF.RData")

## Perform Non-linear Optimization (L-BFGS-B)
numOfDataSet <- 41120 # the number of data set
# load(file = "D:/work/Rcode/BoilerEFF/NOx_Prediction/Average_O2/BoilerEFF.RData")
source("D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/boiler_1/SteamYield_Optimization(L-BFGS-B)_ver2.R")

