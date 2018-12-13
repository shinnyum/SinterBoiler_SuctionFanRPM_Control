#  ==================================================================================
#   * Program Name       	: 소결보일러 Suction fan speed 결정모델 SUB RSCRIPT
#   * Source File Name   	: SteamYield_RNN_Model_(Keras).R
#   * Author             	: 
#   * Version           	: 1.0.0
#   * Created Date       	: 2018-00-00
#   * Updated Date      	: 2018-00-00
#   * Last modifier      	: 
#   * Updated content    	: 최초작성
#   * Description       	: 실행주기(1분)
#  ==================================================================================

# -----------------------------------------------------------------------------------
# Install required packages and load library
# -----------------------------------------------------------------------------------
install.packages("keras")
install.packages("kerasR")
install.packages("devtools")
install.packages("reticulate")
install.packages("memoise")
install.packages("curl")
install.packages("RCurl")
install.packages("RcppCNPy")
install.packages("ggplot2")
library(keras)
library(kerasR)
library(reticulate)
library(memoise)
library(curl)
library(RCurl)
library(RcppCNPy)
library(tibble)
library(readr)
library(ggplot2)

use_python("C:/ProgramData/Anaconda3/python.exe")
keras_init()


# -----------------------------------------------------------------------------------
# Compatibility check
# -----------------------------------------------------------------------------------
reticulate::py_module_available("keras")
reticulate::py_module_available("numpy")
reticulate::py_module_available("scipy")
reticulate::py_module_available("tensorflow")
reticulate::py_available()
reticulate::py_config()
reticulate::import("keras.models")


# -----------------------------------------------------------------------------------
# Define conditional options
# -----------------------------------------------------------------------------------
columnIndex_Y <- 2 # which is the column index number of Y data exits.
seq_size <- 5 # sequence length to look back.
#lengthOfTrainData <- 20000
#IndexOfTestDataStart <- 420500


# -----------------------------------------------------------------------------------
# Define custom-built functions
# -----------------------------------------------------------------------------------
# [function] generating sequence data
gen_dataset <- function(input_data, seq_size){
  
  dataset_F = data.frame(matrix(vector(), 0, ncol(train_data),
                                dimnames = list(c(), c(colnames(train_data)))),
                                stringsAsFactors=F)
  
  for(i in 1:(nrow(input_data) - seq_size + 1)){
    subset = input_data[i:(i + seq_size - 1),]
    dataset_F <- rbind(dataset_F, subset)
  }
  
  dataset_X <- dataset_F[,-columnIndex_Y]
  dataset_Y <- dataset_F[columnIndex_Y]
  dataset_list <- list(dataset_X, dataset_Y)
  
  return(dataset_list)
}


# [function] converting data type, dataframe to list
convert_to_list <- function(input_data, seq_size){
  
  combined_list <- list()
  list_index <- 1

  for(row_index in seq(1,nrow(input_data),seq_size)){
    combined_list[[list_index]] <- as.data.frame(input_data[row_index:(row_index + seq_size - 1),])
    list_index <- list_index + 1
  }
  
  return(combined_list)
}


# [function] filling data into empty array
fill_data <- function(input_list, seq_len, data_dim){
  
    empty_array <- array(0, dim = c(length(input_list), seq_len, data_dim))
  
    for(i in 1:length(input_list)){
      empty_array[i,1:nrow(input_list[[i]]),] <- unlist(input_list[[i]])
    }

  return(empty_array)
}


# -----------------------------------------------------------------------------------
# Data preparation
# -----------------------------------------------------------------------------------
# loading data
data_dir <- "D:/Downloads/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)
glimpse(data)

data <- data.matrix(data[,-1])
colnames(data) <- c("p", "T", "Tpot", "Tdew", "rh", "VPmax", "VPact",
                    "VPdef", "sh", "H2OC", "rho", "wv", "max.wv", "wd")

#ggplot(data, aes(x = 1:nrow(data), y = `T (degC)`)) + geom_line()
#ggplot(data[1:1440,], aes(x = 1:1440, y = `T (degC)`)) + geom_line()

train_data_before_scale <- data[1:200000,]


# scale data
mean <- apply(train_data_before_scale, 2, mean)
std <- apply(train_data_before_scale, 2, sd)
data <- scale(data, center = mean, scale = std)

train_data <- data[1:100,]
test_data <- data[420500:nrow(data),]


# transform input data for RNN
c(x_train, y_train) %<-% list(gen_dataset(train_data, seq_size)[[1]], gen_dataset(train_data, seq_size)[[columnIndex_Y]])
c(x_test, y_test) %<-% list(gen_dataset(test_data, seq_size)[[1]], gen_dataset(test_data, seq_size)[[columnIndex_Y]])

listed_x_train <- convert_to_list(x_train, seq_size)
listed_y_train <- convert_to_list(y_train, seq_size)
listed_x_test  <- convert_to_list(x_test, seq_size)
listed_y_test  <- convert_to_list(y_test, seq_size)

array_x_train <- fill_data(listed_x_train, seq_size, dim(x_train)[[-1]])
array_y_train <- fill_data(listed_y_train, seq_size, dim(y_train)[[-1]])
array_x_test  <- fill_data(listed_x_test,  seq_size, dim(x_test)[[-1]])
array_y_test  <- fill_data(listed_y_test,  seq_size, dim(y_test)[[-1]])

array_y_train <- array_y_train[,seq_size,]
array_y_test  <- array_y_test[,seq_size,]

#dim(array_x_train)
#dim(array_y_train)


# -----------------------------------------------------------------------------------
# RNN model
# -----------------------------------------------------------------------------------
# contruct simple RNN model
model <- keras_model_sequential() %>% 
  layer_gru(units = 32, input_shape = c(seq_size,dim(x_train)[[-1]])) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

# train
model %>% fit(array_x_train, array_y_train, epochs = 20, batch_size = 1)

# evaluate
score = model %>% evaluate(array_x_test, array_y_test, batch_size=1)

# get predictions from a KERAS model
predictions = model %>% predict(array_x_test, batch_size=1)

# descale data
actual_Y <- sapply(array_y_test, function(x){(x*std[[columnIndex_Y]]) + mean[[columnIndex_Y]]})
predicted_Y <- sapply(predictions, function(x){(x*std[[columnIndex_Y]]) + mean[[columnIndex_Y]]})

# visualize
plot(actual_Y ,type="l",pch=16,col="blue",
     ylab="y_test",xlab=NA,main="RNN 모델",lwd=3)
lines(predicted_Y,col="red",lwd=3)

