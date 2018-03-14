install.packages("kerasR")
install.packages("reticulate")
library(kerasR)
library(reticulate)

use_python("C:/Users/POSCOUSER/AppData/Local/Programs/Python/Python37-32")

reticulate::py_available()

reticulate::import("keras.models")

reticulate::py_config()



mod <- Sequential()
mod$add(Dense(units = 50, input_shape = 13))
