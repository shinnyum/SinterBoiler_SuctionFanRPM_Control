#  ==================================================================================
#   * Program Name       	: 소결보일러 Suction fan speed 결정모델 SUB RSCRIPT
#   * Source File Name   	: SteamYield_TrendAnalysis.R
#   * Author             	: 
#   * Version           	: 1.0.0
#   * Created Date       	: 2018-00-00
#   * Updated Date      	: 2018-00-00
#   * Last modifier      	: 
#   * Updated content    	: 최초작성
#   * Description       	: 실행주기(1분)
#  ==================================================================================

library(readxl)
library(lattice)
library(xlsx)

rm(list = is())

## Read data from Excel worksheets ##
SuctionFan_RPM_DATA <- read_excel("D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control/Output/Optimal SuctionFan RPM_ver2.xlsx",
                                  sheet = "Sheet1")
SuctionFan_RPM_DATA <- as.data.frame(SuctionFan_RPM_DATA)
print(str(SuctionFan_RPM_DATA))

## Factorize Abnormal_DATA ##
SuctionFan_RPM_DATA$Abnormal_DATA <- with(data = SuctionFan_RPM_DATA,
                                          ifelse((Actual_SuctionFan_RPM <= "360"), 1,
                                          ifelse((Actual_SuctionFan_RPM >  "360"), 2, 3)))

SuctionFan_RPM_DATA$Abnormal_DATA <- factor(SuctionFan_RPM_DATA$Abnormal_DATA, 
                                            levels = 1:3, labels = c("Abnormal", "Normal", "NA"))
View(SuctionFan_RPM_DATA$Abnormal_DATA)

## Visualize expected effect ##
fit1 <- lm(Actual_Steam_Yield ~ Boiler_Inlet_ExhaustGasTemp, data = SuctionFan_RPM_DATA)
lm.fit1 <- predict(fit1)
fit2 <- lm(Predicted_Steam_Yield ~ Boiler_Inlet_ExhaustGasTemp, data = SuctionFan_RPM_DATA)
lm.fit2 <- predict(fit2)

par('cin')
par('cex')
par('lheight')


line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(0:1, 'inches', 'user'))
  y_off <- diff(grconvertY(0:1, 'inches', 'user'))
  switch(side,
         `1` = par('usr')[3] - line * y_off * lh,
         `2` = par('usr')[1] - line * x_off * lh,
         `3` = par('usr')[4] + line * y_off * lh,
         `4` = par('usr')[2] + line * x_off * lh,
         stop("side must be 1, 2, 3, or 4", call.=TRUE))
}

par(mfrow = c(2,2))

with(data = SuctionFan_RPM_DATA, plot(Boiler_Inlet_ExhaustGasTemp, Actual_Steam_Yield,
                                      xlab = "Boiler_Inlet_ExhaustGas_Temp", xlim = c(200, 330),
                                      ylab = "Actual_Steam_Yield", ylim = c(5, 45),
                                      col = ifelse(Abnormal_DATA == "Abnormal", "green", "black"),
                                      las = 1, cex =0.5, pch = 18))
lines(lm.fit1 ~ SuctionFan_RPM_DATA$Boiler_Inlet_ExhaustGasTemp, col = "red", lwd = 2)

with(data = SuctionFan_RPM_DATA, plot(Boiler_Inlet_ExhaustGasTemp, Predicted_Steam_Yield,
                                      xlab = "Boiler_Inlet_ExhaustGas_Temp", xlim = c(200, 330),
                                      ylab = "Predicted_Steam_Yield", ylim = c(5, 45),
                                      col = ifelse(Abnormal_DATA == "Abnormal", "green", "black"),
                                      las = 1, cex =0.5, pch = 18))
lines(lm.fit2 ~ SuctionFan_RPM_DATA$Boiler_Inlet_ExhaustGasTemp, col = "red", lwd = 2)

#text(line2user(line=mean(par('mar')[c(2, 4)]), side=2), 
#     line2user(line=2, side=3), 'SuctionFan_RPM Optimization', xpd=NA, cex=2, font=1)

group.column <- c("Actual_SuctionFan_RPM", "Optimal_SuctionFan_RPM")

for(i in unique(group.column)) {
    with(data = SuctionFan_RPM_DATA, plot(SuctionFan_RPM_DATA[,i], Boiler_Inlet_ExhaustGasTemp,
                                          xlab = paste(i),
                                          ylab = "Boiler_Inlet_ExhaustGasTemp",
                                          col = ifelse(Abnormal_DATA == "Abnormal", "green", "black"),
                                          las = 1, cex =0.5, pch = 18))
}

#text(line2user(line=mean(par('mar')[c(2, 4)]), side=2), 
#     line2user(line=2, side=3), 'SuctionFan_RPM Optimization2', xpd=NA, cex=2, font=1)


title(main = "SuctionFan_RPM Optimization", outer = "T")
#mtext("My 'Title' in a strange place", side = 3, line = -25, outer = TRUE)

graphics.off()
