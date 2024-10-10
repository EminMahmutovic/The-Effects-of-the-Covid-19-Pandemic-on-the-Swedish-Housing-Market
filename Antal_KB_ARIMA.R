setwd("C:/Users/Hampu/Desktop/STAT III C-uppsats")
##Vi använder nog i dagsläget inte alla dessa paketen, men är bra att ha dem här =) 
library(forecast)
library(zoo)
library (fpp2)
library(lmtest)
library(missForest)
library(imputeTS)
library(qcc) ## use library(qcc) for function cusum, used for forecasting errors
library(tibble)
library(nlme)
library(car)
library(orcutt)
library(vars)
############# TEST KPI corrected. 
#### From 2017-10 to 2021-09
KPI <- c(1846,1850, 1857,
         1842, 1855, 1860, 1868, 1872, 1876, 1886, 1882, 1891, 1888, 1887, 1895, 1876, 1890, 1895, 1908, 1913, 1910, 1917, 1909, 1918, 1919, 1921, 1928, 1900, 1910, 1907, 1901, 1912, 1923, 1928, 1925, 1926, 1924, 1924, 1938,1930, 1936, 1939, 1944, 1947, 1949, 1954, 1964, 1974)
plot(KPI)
KPIK <- (KPI/1846)*100
##View(KPIK)
##plot(KPIK)
KPIK <- KPIK/100
KPIK <- as.data.frame(KPIK)

##Befolkning<- read.csv("000003O5_20211130-155339.csv",skip=2, header=TRUE, sep = ",", dec = ".")
##View(Befolkning)
##library(stringr)
##pop_kommun$V1 <- str_split_fixed(pop_kommun$V1, " ", 2)

Antal_KB<- read.csv("Data - makstat_bostadsratter - Antal_Kommuner.csv", header=TRUE, sep = ",", dec = ".")
#View(Antal_KB)
rownames(Antal_KB) <- Antal_KB[,1]
Antal_KB <- Antal_KB[,-1]
AKB_1<- subset(Antal_KB, Indelning.typ.3 == "1")
AKB_2<- subset(Antal_KB, Indelning.typ.3 == "2")
AKB_3<- subset(Antal_KB, Indelning.typ.3 == "3")
###View(AKB_1)
AKB_1 <- AKB_1[,-c(1:2)]
AKB_2 <- AKB_2[,-c(1:2)]
AKB_3 <- AKB_3[,-c(1:2)]
AKB_1 <- t(AKB_1)
AKB_1 <- as.data.frame(AKB_1)
AKB_2 <- t(AKB_2)
AKB_2 <- as.data.frame(AKB_2)
AKB_3 <- t(AKB_3)
AKB_3 <- as.data.frame(AKB_3)
SAKB_1 <- rowSums(AKB_1, na.rm=FALSE, dims=1)
SAKB_2 <- rowSums(AKB_2, na.rm=FALSE, dims=1)
SAKB_3 <- rowSums(AKB_3, na.rm=FALSE, dims=1)
############################################# STOP HERE IF YOU WANT TO USE THE CODE AT ROW 72
#View(SAKB_3)
AKB_1 <- ts(SAKB_1, frequency=12)
AKB_2 <- ts(SAKB_2, frequency=12)
AKB_3 <- ts(SAKB_3, frequency=12)
plot(AKB_1, ylim=range(500,8000))
lines(AKB_2, col="red")
lines(AKB_3, col="blue")
#### Make index with 2017-10 as 1.0
SAKB_1 <- SAKB_1/641
SAKB_2 <- SAKB_2/4518
SAKB_3 <- SAKB_3/5624
AKB_1 <- ts(SAKB_1, frequency=12, start=c(2017,10))
AKB_2 <- ts(SAKB_2, frequency=12, start=c(2017,10))
AKB_3 <- ts(SAKB_3, frequency=12, start=c(2017,10))
plot(AKB_1, ylim=range(0.45, 1.45))
lines(AKB_2, col="red")
lines(AKB_3, col="blue")
####################################
#### ARIMA FOR COMPLETE DATASETS
####################### AKB_1.... Recommended ARIMA(1,1,1)(1,1,0)[12]
SAKB_1 <- ts(SAKB_1, frequency = 12, start=c(2017,10))
auto.arima(SAKB_1)
AKB_1M <- arima(SAKB_1, seasonal=list(order=c(1L,1L,0L), period=12), order=c(1,1,1))
summary(AKB_1M)
Res.AKB_1M <- as.vector(residuals(AKB_1M))
fit.AKB_1M <- as.vector(fitted(AKB_1M))
Box.test(Res.AKB_1M,lag = 43, fitdf = 3, type="Ljung")
AKB_1M.forecast <- as.array(forecast(AKB_1M,h=12, level=c(80,95)))
autoplot(AKB_1M.forecast)
acf(AKB_1M$residuals)
plot(SAKB_1)
####################### AKB_2.... Recommended ARIMA(1,0,0)(0,1,1,)[12]
SAKB_2 <- ts(SAKB_2, frequency = 12, start=c(2017,10))
auto.arima(SAKB_2)
AKB_2M <- Arima(SAKB_2 ,order=c(1,0,0), seasonal = list(order=c(0L,1L, 1L), period=12), include.drift=TRUE)
summary(AKB_2M)
Res.AKB_2M <- as.vector(residuals(AKB_2M))
fit.AKB_2M <- as.vector(fitted(AKB_2M))
Box.test(Res.AKB_2M,lag = 47 , fitdf = 1, type="Ljung")
AKB_2M.forecast <- as.array(forecast(AKB_2M,h=12, level=c(80,95)))
autoplot(AKB_2M.forecast)
acf(AKB_2M$residuals)
##################### AKB_3 ...... Recommended ARIMA(1,0,0)(1,1,0)[12]
SAKB_3 <- ts(SAKB_3, frequency = 12, start=c(2017,10))
auto.arima(SAKB_3)
AKB_3M <- Arima(SAKB_3 ,order=c(1,0,0), seasonal = list(order=c(1L,1L, 0L), period=12), include.drift = TRUE )
summary(AKB_3M)
Res.AKB_3M <- as.vector(residuals(AKB_3M))
fit.AKB_3M <- as.vector(fitted(AKB_3M))
Box.test(Res.AKB_3M,lag = 47 , fitdf = 1, type="Ljung")
AKB_3M.forecast <- as.array(forecast(AKB_3M,h=12, level=c(80,95)))
autoplot(AKB_3M.forecast)
acf(AKB_3M$residuals)
#####################################################################
#####################################################################
#### SPLIT DATASETS AND CREATE ARIMA MODELS FOR THEM
############################################################# AKB_1
AKB_1_BC <-  SAKB_1[1:30]
AKB_1_BC <- as.data.frame(AKB_1_BC)
AKB_1_AC <-  SAKB_1[31:48]
AKB_1_AC <- as.data.frame(AKB_1_AC)
######################################
## BC                         ARIMA(1,0,0)(0,1,0)[12]
AKB_1_BC <- ts(AKB_1_BC)
auto.arima(AKB_1_BC, allowmean = FALSE)
AKB_1_BC <- Arima(AKB_1_BC, seasonal=list(order=c(0L,1L,0L), period=12), order=c(1,0,0))
summary(AKB_1_BC)
Res.AKB_1_BC <- as.vector(residuals(AKB_1_BC))
fit.AKB_1_BC <- as.vector(fitted(AKB_1_BC))
Box.test(Res.AKB_1_BC,lag = 28, fitdf = 1, type="Ljung")
AKB_1_BC.forecast <- as.array(forecast(AKB_1_BC,h=12, level=c(80,95)))
autoplot(AKB_1_BC.forecast)
acf(AKB_1_BC$residuals)
### AC                     ARIMA(0,0,1)(0,1,0)[12]
AKB_1_AC <- ts(AKB_1_AC)
auto.arima(AKB_1_AC, allowmean = FALSE)
AKB_1_AC <- Arima(AKB_1_AC,seasonal=list(order=c(0L,1L,0L), period=12), order=c(0,0,1))
summary(AKB_1_AC)
Res.AKB_1_AC <- as.vector(residuals(AKB_1_AC))
fit.AKB_1_AC <- as.vector(fitted(AKB_1_AC))
Box.test(Res.AKB_1_AC,lag = 17, fitdf = 1, type="Ljung")
AKB_1_AC.forecast <- as.array(forecast(AKB_1_AC,h=12, level=c(80,95)))
autoplot(AKB_1_AC.forecast)
acf(AKB_1_AC$residuals)

################################################### AKB_2
AKB_2_BC <-  SAKB_2[1:30]
AKB_2_BC <- as.data.frame(AKB_2_BC)
AKB_2_AC <-  SAKB_2[31:48]
AKB_2_AC <- as.data.frame(AKB_2_AC)
######################################
## BC                    ARIMA(1,0,0)(0,1,0)[12]
AKB_2_BC <- ts(AKB_2_BC)
auto.arima(AKB_2_BC, allowmean = FALSE)
AKB_2_BC <- Arima(AKB_2_BC,seasonal=list(order=c(0L,1L,0L),period=12), order=c(1,0,0))
summary(AKB_2_BC)
Res.AKB_2_BC <- as.vector(residuals(AKB_2_BC))
fit.AKB_2_BC <- as.vector(fitted(AKB_2_BC))
Box.test(Res.AKB_2_BC,lag = 28, fitdf = 1, type="Ljung")
AKB_2_BC.forecast <- as.array(forecast(AKB_2_BC,h=12, level=c(80,95)))
autoplot(AKB_2_BC.forecast)
acf(AKB_2_BC$residuals)
### AC                      ARIMA(0,0,0)(0,1,0)[12]
AKB_2_AC <- ts(AKB_2_AC)
auto.arima(AKB_2_AC, allowmean = FALSE)
AKB_2_AC <- Arima(AKB_2_AC,seasonal=list(order=c(0L,1L,0L),period=12), order=c(0,0,0))
summary(AKB_2_AC)
Res.AKB_2_AC <- as.vector(residuals(AKB_2_AC))
fit.AKB_2_AC <- as.vector(fitted(AKB_2_AC))
Box.test(Res.AKB_2_AC,lag = 17, fitdf = 0, type="Ljung")
AKB_2_AC.forecast <- as.array(forecast(AKB_2_AC,h=12, level=c(80,95)))
autoplot(AKB_2_AC.forecast)
acf(AKB_2_AC$residuals)
############################################# AKB_3
AKB_3_BC <-  SAKB_3[1:30]
AKB_3_BC <- as.data.frame(AKB_3_BC)
AKB_3_AC <-  SAKB_3[31:48]
AKB_3_AC <- as.data.frame(AKB_3_AC)
######################################
## BC                    ##### ARIMA(1,0,0)(0,1,0)[12]
AKB_3_BC <- ts(AKB_3_BC)
auto.arima(AKB_3_BC, allowmean = FALSE)
AKB_3_BC <- Arima(AKB_3_BC,seasonal=list(order=c(0L,1L,0L),period=12), order=c(1,0,0))
summary(AKB_3_BC)
Res.AKB_3_BC <- as.vector(residuals(AKB_3_BC))
fit.AKB_3_BC <- as.vector(fitted(AKB_3_BC))
Box.test(Res.AKB_3_BC,lag = 29, fitdf =1, type="Ljung")
AKB_3_BC.forecast <- as.array(forecast(AKB_3_BC,h=12, level=c(80,95)))
autoplot(AKB_3_BC.forecast)
acf(AKB_3_BC$residuals)
### AC
AKB_3_AC <- ts(AKB_3_AC)
auto.arima(AKB_3_AC, allowmean = FALSE)
AKB_3_AC <- Arima(AKB_3_AC,seasonal=list(order=c(0L,1L,0L),period=12), order=c(1,0,0))
summary(AKB_3_AC)
Res.AKB_3_AC <- as.vector(residuals(AKB_3_AC))
fit.AKB_3_AC <- as.vector(fitted(AKB_3_AC))
Box.test(Res.AKB_3_AC,lag = 17, fitdf = 1, type="Ljung")
AKB_3_AC.forecast <- as.array(forecast(AKB_3_AC,h=12, level=c(80,95)))
autoplot(AKB_3_AC.forecast)
acf(AKB_3_AC$residuals)



