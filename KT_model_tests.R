setwd("C:/Users/Hampu/Desktop/STAT III C-uppsats")

library(dplyr)
library(forecast)
library(car)
library(orcutt)
library(lmtest)
library(stats)
library(urca)
library(vars)
library(strucchange)
library(aTSA)

################################## Possible, Control variables ##########################
KPI <- c(1846,1850, 1857,
         1842, 1855, 1860, 1868, 1872, 1876, 1886, 1882, 1891, 1888, 1887, 1895, 
         1876, 1890, 1895, 1908, 1913, 1910, 1917, 1909, 1918, 1919, 1921, 1928, 
         1900, 1910, 1907, 1901, 1912, 1923, 1928, 1925, 1926, 1924, 1924, 1938,
         1930, 1936, 1939, 1944, 1947, 1949, 1954, 1964, 1974)
KPIK <- (KPI/1846)*100
KPIK <- KPIK/100
KIH <- c(105.7,	107.5,	106.6,	106.5,	104.6,	102.2,	102.4,	102.1,	100.3, 100.4,	103.8,
         104.9,	99.6,	98.6,	95.1,	93.2,	93.7,	96.3,	98.3,	95.1,	96.6,	99.0,
         94.8, 90.9,	93.8,	94.2,	94.7,	93.4,	98.9,	89.6, 72.8,	77.2,	84.2,	
         84.3,	84.9,	87.4,	89.9,	89.6,	92.1,	94.2,	97.8,	96.8,	103.3,	111.3,	108.7,	106.1,	107.8, 106.3)

#säsongsrensad och utjämnad serie. https://www.ekonomifakta.se/fakta/arbetsmarknad/arbetsloshet/arbetsloshet/.  
AL <- c(6.6, 6.5, 6.4, 6.4, 6.3, 6.3, 6.2, 6.2, 6.3, 6.3, 6.3, 6.4, 6.5, 6.5, 
        6.5, 6.6, 6.6, 6.6, 6.7, 6.7, 6.8, 6.8, 6.9, 6.9, 7.0, 7.0, 7.1, 7.1,
        7.2, 7.3, 7.9, 7.9, 9.1, 9.0, 9.0, 8.9, 8.9, 8.9, 8.8, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 8.9, 8.8, 8.8)

################################## Indelning 1 ######################################
kt <- read.table("Data - makstat_villor - K_T_kommuner_imp.csv", header = TRUE, sep = ",", dec = ".")
kt_1 <- subset(kt, Indelning.typ.3 == "1")
rownames(kt_1)<-kt_1[,1]
kt_1<-kt_1[-c(1,2,3)]
kt_1 <- t(kt_1)
kt_1_rs <- rowSums(kt_1, na.rm=FALSE, dims=1)
(kt_1_mean <- kt_1_rs/ncol(kt_1))
kt_1_ts <- as.data.frame(kt_1_mean)
kt_1_ts$Time <- c(1:48)
colnames(kt_1_ts) <- c("KT", "Time")
kt_1_ts$KPIK <- KPIK
kt_1_ts$KIH <- KIH
kt_1_ts$AL <- AL

########################### Plots #############################################
plot(kt_1_ts$KT)
acf(kt_1_ts$KT, lag.max = 48, type="correlation", main= "ACF of KT")
acf(kt_1_ts$KT, lag.max = 48, type="partial", main= "ACF of KT")

########################### TS ###############################################
kt_1_ts <- ts(kt_1_ts, frequency = 12, start = c(2017, 10))
plot.ts(kt_1_ts) # Looks to be addiditive. 

########################### Arima #############################################
# matrix <- as.matrix(kt_x_ts[,3])
# matrix <- matrix[,c(-2)]
auto.fit.1 <- forecast::auto.arima(kt_1_ts[,1], trace = TRUE) # Simpler is better (:
summary(auto.fit.1)
plot(kt_1_ts[,1])
lines(auto.fit.1$fitted, col="red")
checkresiduals(auto.fit.1) 
acf(auto.fit.1$residuals, lag.max=48, type="partial")
# KT1 - arima(1,1,0)(0,1,0): mpe = 0.04032605, p-value = 0.9158 with none
# KT2 - ARIMA(3,2,1): mpe = 0.1242172, p-value = 0.1753 with none
# KT3 - ARIMA(0,2,0)(1,0,0): mpe = 0.02045033, p-value = 0.2493 with none

(fit.arima.1 <- Arima(kt_1_ts[,1], order=c(1,1,0), seasonal = c(0,1,0)))
summary(fit.arima.1)
checkresiduals(fit.arima.1) 
acf(fit.arima.1$residuals, lag.max=48, type="partial")

# Forecast
forecast::forecast(fit.arima.1, h = 12, level = 95) %>% 
  autoplot()
qqnorm(fit.arima.1$residuals, datax = FALSE)
qqline(fit.arima.1$residuals)
plot(residuals(fit.arima.1), type="p")
plot(kt_1_ts[,1])
lines(fit.arima.1$fitted, col="blue")

######################### Split Data ##########################################

kt_BC.1 <-  kt_1_ts[1:30,1:5]
kt_AC.1 <-  kt_1_ts[31:48,1:5]
# matrix <- as.matrix(kt_BC[,3])
# matrix <- matrix[,c(-2)]
fit.auto.1 <- forecast::auto.arima(kt_BC.1[,1], trace = TRUE, allowmean = F)
summary(fit.auto.1)
plot(kt_1_ts[,1])
lines(fit.auto.1$fitted)   ################################################################### INTE PÅ SAMMA XLIM SOM kb_1_ts[,1]. Värdena här är från 1-5. kb_1_ts[,1] är från 2017-10 till 2021-09. TESTA XLIM=RANGE(0,2021) I PLOT() SÅ FÅR MAN SE BÅDA GRAFERNA, MEN ÄR EN MENINGSLÖS TOLKNING.
checkresiduals(fit.auto.1)
acf(fit.auto.1$residuals, lag.max=48, type="partial")
# KT1 - ARIMA(2,1,0): MPE = 0.1502822, p = 0.354 with none.
# KT2 - ARIMA(3,0,0): MPE = -0.02377171, P = 0.06383 with KPIK
# KT3 - ARIMA(1,0,0): MPE = -0.1117458, P = 0.7123, with KPIK
####################### Forecast split data ##################################
(fit.arima.1 <- Arima(kt_BC.1[,1], order=c(2,1,0)))
summary(fit.arima.1)
checkresiduals(fit.arima.1)
acf(fit.arima.1$residuals, lag.max=48, type="partial")

# fmatrix <- as.matrix(kt_AC.1[,3:5])
# fmatrix <- fmatrix[,c(-2)]
forecast <- forecast::forecast(fit.arima.1, h = 18, level = 95) %>% 
  plot()

########################## Chow test 
KT_BC.1.Test <- kt_BC.1[,1]
KT_BC.1.Test  <- as.data.frame(KT_BC.1.Test )
names(KT_BC.1.Test)[1] <-"KT"
KT_BC_1.Test <- KT_BC.1.Test %>% add_row(KT=forecast$mean)
sctest(kt_1_ts[,1] ~ KT_BC_1.Test$KT ,type="Chow", point=31)
## PLOT to compare
plot(kt_1_ts[,1])
KT_BC_1.TS <- ts(KT_BC_1.Test$KT,frequency = 12, start = c(2017, 10))
lines(KT_BC_1.TS, col="red") ######## IT is quite clear that they differ at the breakpoint. 
########## 
## SWAP KT=forecast$mean to either upper or lower and rerun the whole Chow test code to see how it compares to the boundery. 
## For "upper" it still pass with flying colour. But there is reason to think that the arima model dont model the forecast in a good way. 
## The fitted values are practicaly flat at the breakpoint.

########### PLOT THE REAL DATA, THE MEAN AND THE UPPER AND LOWER IN SAME GRAPH
KT_BC_1.MEAN <- KT_BC.1.Test %>% add_row(KT=forecast$mean)
KT_BC_1.UPPER <- KT_BC.1.Test %>% add_row(KT=forecast$upper)
KT_BC_1.LOWER <- KT_BC.1.Test %>% add_row(KT=forecast$lower)
KT_BC_1.TS.MEAN <- ts(KT_BC_1.MEAN$KT,frequency = 12, start = c(2017, 10))
KT_BC_1.TS.UPPER <- ts(KT_BC_1.UPPER$KT,frequency = 12, start = c(2017, 10))
KT_BC_1.TS.LOWER <- ts(KT_BC_1.LOWER$KT,frequency = 12, start = c(2017, 10))
plot(KT_BC_1.TS.MEAN, col="red", ylim=range(1.65, 2.35))
lines(KT_BC_1.TS.UPPER, col="blue")
lines(KT_BC_1.TS.LOWER, col="blue")
lines(kt_1_ts[,1], col="black")





############################# Indelning 2 #######################################
kt_2 <- subset(kt, Indelning.typ.3 == "2")
rownames(kt_2)<-kt_2[,1]
kt_2<-kt_2[-c(1,2,3)]
kt_2 <- t(kt_2)
kt_2_rs <- rowSums(kt_2, na.rm=FALSE, dims=1)
(kt_2_mean <- kt_2_rs/ncol(kt_2))
kt_2_ts <- as.data.frame(kt_2_mean)
kt_2_ts$Time <- c(1:48)
colnames(kt_2_ts) <- c("KT", "Time")
kt_2_ts$KPIK <- KPIK
kt_2_ts$KIH <- KIH
kt_2_ts$AL <- AL

########################### Plots #############################################
plot(kt_2_ts$KT)
acf(kt_2_ts$KT, lag.max = 48, type="correlation", main= "ACF of KT")
acf(kt_2_ts$KT, lag.max = 48, type="partial", main= "ACF of KT")

########################### TS ###############################################
kt_2_ts <- ts(kt_2_ts, frequency = 12, start = c(2017, 10))
plot.ts(kt_2_ts) # Looks to be addiditive. 

########################### Arima #############################################
# matrix <- as.matrix(kt_x_ts[,3])
# matrix <- matrix[,c(-2)]
auto.fit.2 <- forecast::auto.arima(kt_2_ts[,1], trace = TRUE) # Simpler is better (:
summary(auto.fit.2)
plot(kt_2_ts[,1])
lines(auto.fit.2$fitted, col="red")
checkresiduals(auto.fit.2)
acf(auto.fit.2$residuals, lag.max=48, type="partial")
# KT1 - arima(1,1,0)(0,1,0): mpe = 0.04032605, p-value = 0.9158 with none
# KT2 - ARIMA(3,2,1): mpe = 0.1242172, p-value = 0.1753 with none
# KT3 - ARIMA(0,2,0)(1,0,0): mpe = 0.02045033, p-value = 0.2493 with none

(fit.arima.2 <- Arima(kt_2_ts[,1], order=c(3,2,1),))
summary(fit.arima.2)
checkresiduals(fit.arima.2)
acf(fit.arima.2$residuals, lag.max=48, type="partial")

# Forecast
forecast::forecast(fit.arima.2, h = 12, level = 95) %>% 
  autoplot()
qqnorm(fit.arima.2$residuals, datax = FALSE)
qqline(fit.arima.2$residuals)
plot(residuals(fit.arima.2), type="p")
plot(kt_2_ts[,1])
lines(fit.arima.2$fitted, col="blue")

######################### Split Data ##########################################

kt_BC.2 <-  kt_2_ts[1:30,1:5]
kt_AC.2 <-  kt_2_ts[31:48,1:5]

matrix <- as.matrix(kt_BC.2[,3])
# matrix <- matrix[,c(-2)]
fit.auto.2 <- forecast::auto.arima(kt_BC.2[,1], xreg = matrix, trace = TRUE, allowmean = F)
summary(fit.auto.2)
plot(kt_2_ts[,1])
lines(fit.auto.2$fitted)
checkresiduals(fit.auto.2)
acf(fit.auto.2$residuals, lag.max=48, type="partial")
# KT1 - ARIMA(2,1,0): MPE = 0.1502822, p = 0.354 with none.
# KT2 - ARIMA(3,0,0): MPE = -0.02377171, P = 0.06383 with KPIK
# KT3 - ARIMA(1,0,0): MPE = -0.1117458, P = 0.7123, with KPIK

####################### Forecast split data ##################################
(fit.arima.2 <- Arima(kt_BC.2[,1], order=c(3,0,0), xreg = matrix, include.constant = FALSE))
summary(fit.arima.2)
checkresiduals(fit.arima.2)
acf(fit.arima.2$residuals, lag.max=48, type="partial")

fmatrix <- as.matrix(kt_AC.2[,3])
# fmatrix <- fmatrix[,c(-2)]
forecast <- forecast::forecast(fit.arima.2, h = 18, level = 95, xreg = fmatrix) %>% 
  plot()

########################## Chow test 
KT_BC.2.Test <- kt_BC.2[,1]
KT_BC.2.Test  <- as.data.frame(KT_BC.2.Test )
names(KT_BC.2.Test)[1] <-"KT"
KT_BC_2.Test <- KT_BC.2.Test %>% add_row(KT=forecast$mean)
sctest(kt_2_ts[,1] ~ KT_BC_2.Test$KT ,type="Chow", point=31)
## PLOT to compare
plot(kt_2_ts[,1])
KT_BC_2.TS <- ts(KT_BC_2.Test$KT,frequency = 12, start = c(2017, 10))
lines(KT_BC_2.TS, col="red") ######## IT is quite clear that they differ at the breakpoint. 
########## 
### Not as bad of a forecast as for KT_1, it is clear that there is a slight increase in KT over time. 
### It is clear though looking at the upper level that there is a big difference between the forecast and the real data.

########### PLOT THE REAL DATA, THE MEAN AND THE UPPER AND LOWER IN SAME GRAPH
KT_BC_2.MEAN <- KT_BC.2.Test %>% add_row(KT=forecast$mean)
KT_BC_2.UPPER <- KT_BC.2.Test %>% add_row(KT=forecast$upper)
KT_BC_2.LOWER <- KT_BC.2.Test %>% add_row(KT=forecast$lower)
KT_BC_2.TS.MEAN <- ts(KT_BC_2.MEAN$KT,frequency = 12, start = c(2017, 10))
KT_BC_2.TS.UPPER <- ts(KT_BC_2.UPPER$KT,frequency = 12, start = c(2017, 10))
KT_BC_2.TS.LOWER <- ts(KT_BC_2.LOWER$KT,frequency = 12, start = c(2017, 10))
plot(KT_BC_2.TS.MEAN, col="red", ylim=range(1.665, 2.21))
lines(KT_BC_2.TS.UPPER, col="blue")
lines(KT_BC_2.TS.LOWER, col="blue")
lines(kt_2_ts[,1], col="black")




################################ Indelning - 3 ####################################
kt_3 <- subset(kt, Indelning.typ.3 == "3")
rownames(kt_3)<-kt_3[,1]
kt_3<-kt_3[-c(1,2,3)]
kt_3 <- t(kt_3)
kt_3_rs <- rowSums(kt_3, na.rm=FALSE, dims=1)
(kt_3_mean <- kt_3_rs/ncol(kt_3))
kt_3_ts <- as.data.frame(kt_3_mean)
kt_3_ts$Time <- c(1:48)
colnames(kt_3_ts) <- c("KT", "Time")
kt_3_ts$KPIK <- KPIK
kt_3_ts$KIH <- KIH
kt_3_ts$AL <- AL

########################### Plots #############################################
plot(kt_3_ts$KT)
acf(kt_3_ts$KT, lag.max = 48, type="correlation", main= "ACF of KT")
acf(kt_3_ts$KT, lag.max = 48, type="partial", main= "ACF of KT")

########################### TS ###############################################
kt_3_ts <- ts(kt_3_ts, frequency = 12, start = c(2017, 10))
plot.ts(kt_3_ts) # Looks to be addiditive. 

########################### Arima #############################################
# matrix <- as.matrix(kt_x_ts[,3])
# matrix <- matrix[,c(-2)]
auto.fit.3 <- forecast::auto.arima(kt_3_ts[,1], trace = TRUE) # Simpler is better (:
summary(auto.fit.3)
plot(kt_3_ts[,1])
lines(auto.fit.3$fitted, col="red")
checkresiduals(auto.fit.3) 
acf(auto.fit.3$residuals, lag.max=48, type="partial")
# KT1 - arima(1,1,0)(0,1,0): mpe = 0.04032605, p-value = 0.9158 with none
# KT2 - ARIMA(3,2,1): mpe = 0.1242172, p-value = 0.1753 with none
# KT3 - ARIMA(0,2,0)(1,0,0): mpe = 0.02045033, p-value = 0.2493 with none

(fit.arima.3 <- Arima(kt_3_ts[,1], order=c(0,2,0), seasonal = c(1,0,0)))
summary(fit.arima.3)
checkresiduals(fit.arima.3) 
acf(fit.arima.3$residuals, lag.max=48, type="partial")

# Forecast
forecast::forecast(fit.arima.3, h = 12, level = 95) %>% 
  autoplot()
qqnorm(fit.arima.3$residuals, datax = FALSE)
qqline(fit.arima.3$residuals)
plot(residuals(fit.arima.3), type="p")
plot(kt_3_ts[,1])
lines(fit.arima.3$fitted, col="blue")

######################### Split Data ##########################################

kt_BC.3 <-  kt_3_ts[1:30,1:5]
kt_AC.3 <-  kt_3_ts[31:48,1:5]

matrix <- as.matrix(kt_BC.3[,3])
# matrix <- matrix[,c(-2)]
fit.auto.3 <- forecast::auto.arima(kt_BC.3[,1], xreg = matrix, trace = TRUE, allowmean = F)
summary(fit.auto.3)
plot(kt_3_ts[,1])
lines(fit.auto.3$fitted)
checkresiduals(fit.auto.3) 
acf(fit.auto.3$residuals, lag.max=48, type="partial")
# KT1 - ARIMA(2,1,0): MPE = 0.1502822, p = 0.354 with none.
# KT2 - ARIMA(3,0,0): MPE = -0.02377171, P = 0.06383 with KPIK
# KT3 - ARIMA(1,0,0): MPE = -0.1117458, P = 0.7123, with KPIK

####################### Forecast split data ##################################
(fit.arima.3 <- Arima(kt_BC.3[,1], order=c(1,0,0), xreg = matrix, include.constant = FALSE))
summary(fit.arima.3)
checkresiduals(fit.arima.3) 
acf(fit.arima.3$residuals, lag.max=48, type="partial")

fmatrix <- as.matrix(kt_AC.3[,3])
# fmatrix <- fmatrix[,c(-2)]
forecast <- forecast::forecast(fit.arima.3, h = 18, level = 95, xreg = fmatrix) %>% 
  plot()

########################## Chow test 
KT_BC.3.Test <- kt_BC.3[,1]
KT_BC.3.Test  <- as.data.frame(KT_BC.3.Test )
names(KT_BC.3.Test)[1] <-"KT"
KT_BC_3.Test <- KT_BC.3.Test %>% add_row(KT=forecast$mean)
sctest(kt_3_ts[,1] ~ KT_BC_3.Test$KT ,type="Chow", point=31)
## PLOT to compare
plot(kt_3_ts[,1])
KT_BC_3.TS <- ts(KT_BC_3.Test$KT,frequency = 12, start = c(2017, 10))
lines(KT_BC_3.TS, col="red") ######## IT is quite clear that they differ at the breakpoint. 
########## We see that there is still a clear underestimation. 


KT_BC_3.MEAN <- KT_BC.3.Test %>% add_row(KT=forecast$mean)
KT_BC_3.UPPER <- KT_BC.3.Test %>% add_row(KT=forecast$upper)
KT_BC_3.LOWER <- KT_BC.3.Test %>% add_row(KT=forecast$lower)
KT_BC_3.TS.MEAN <- ts(KT_BC_3.MEAN$KT,frequency = 12, start = c(2017, 10))
KT_BC_3.TS.UPPER <- ts(KT_BC_3.UPPER$KT,frequency = 12, start = c(2017, 10))
KT_BC_3.TS.LOWER <- ts(KT_BC_3.LOWER$KT,frequency = 12, start = c(2017, 10))
plot(KT_BC_3.TS.MEAN, col="red", ylim=range(1.43, 1.93))
lines(KT_BC_3.TS.UPPER, col="blue")
lines(KT_BC_3.TS.LOWER, col="blue")
lines(kt_3_ts[,1], col="black")














# broken
KT_BC_3 <- kt_BC.3 %>% add_row(KT=forecast$mean)
sctest(kt_3_ts[,1] ~ kt_BC.3[,1], type = "Chow", point = 31)
























# Decompose to judge if additive or multiplicative. 
kt_comp <- stl(kt_2_ts[,1], t.window = 13,  s.window = "periodic") 
autoplot(kt_comp)
checkresiduals(kt_comp$time.series[,"remainder"]) # Additive

# Detrend & seasonally adjust KT.
kt_dt <- remainder(kt_comp)
kt_dt <- as.data.frame(kt_dt)
plot(kt_dt)
kt_dt$KIH <- KIH
kt_dt$KPIK <- KPIK
kt_dt$AL <- AL
colnames(kt_dt) <- c("KT", "KIH", "KPIK", "AL")

# Detrend KPIK.
kt_comp <- stl(kt_2_ts[,3], t.window = 13,  s.window = "periodic") 
autoplot(kt_comp)
checkresiduals(kt_comp$time.series[,"remainder"])

kpik_dt <- remainder(kt_comp)
plot(kpik_dt)
kt_dt$KPIK <- kpik_dt

# Detrend AL.
kt_comp <- stl(kt_2_ts[,5], t.window = 13,  s.window = "periodic") 
autoplot(kt_comp)
checkresiduals(kt_comp$time.series[,"remainder"])

al_dt <- remainder(kt_comp)
plot(al_dt)
kt_dt$AL <- al_dt

# Detrend KIH.
kt_comp <- stl(kt_2_ts[,4], t.window = 13,  s.window = "periodic") 
autoplot(kt_comp)
checkresiduals(kt_comp$time.series[,"remainder"])

kih_dt <- remainder(kt_comp)
plot(kih_dt)
kt_dt$KIH <- kih_dt

# ADF-test for stationarity. p-value < 0.05 to be stationary. 
library(aTSA)
adf.test(kt_dt[,1])
adf.test(kt_dt[,2])
adf.test(kt_dt[,3])
adf.test(kt_dt[,4])

############################# VAR - MODEL ################################## STRUGGLING TO WORK ???
kt_x_ts <- subset(kt_x_ts, select=-c(Time), frequency = 12, start = c(2017, 10))
VARselect(kt_x_ts[1:4], lag.max = 5, type="both") # Select the VAR() dependant on criterion, SC=BIC is the best criterion.
var1 <- VAR(kt_x_ts[1:4], p = 2, type="both") 
serial.test(var1, lags.pt = 12, type="PT.asymptotic") # Portmanteau test.
forecast(var1$varresult$KT$fitted.values, level = 95) %>% 
  plot()
summary(var1)
qqnorm(var1$varresult$KT$residuals, datax = FALSE)
qqline(var1$varresult$KT$residuals)
plot(var1)
plot(residuals(var1)[,1], type="p")
plot(kt_x_ts[,1])
points(kt_x_ts[,1], pch=16, cex=0.9)
lines(var1$varresult$KT$fitted.values, col="blue")

# For de-trended variables
VARselect(kt_dt[1:4], lag.max = 5, type="const") # Select the VAR() dependant on criterion, SC=BIC is the best criterion.
var1 <- VAR(kt_dt[1:4], p = 2, type="const") 
serial.test(var1, lags.pt = 12, type="PT.asymptotic") # Portmanteau test.
forecast(var1$varresult$KT$fitted.values, level = 95) %>% 
  plot()
summary(var1) 
qqnorm(var1$varresult$KT$residuals, datax = FALSE)
qqline(var1$varresult$KT$residuals)
plot(var1)
plot(residuals(var1)[,1], type="p")
plot(kt_x_ts[,1])
points(kt_x_ts[,1], pch=16, cex=0.9)
lines(var1$varresult$KT$fitted.values, col="blue")










