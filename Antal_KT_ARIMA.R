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

#Antal_KT<- read.csv("Data - makstat_villor - Antal_Kommuner.csv", header=TRUE, sep = ",", dec = ".")
#Antal_KT <- t(Antal_KT)
#colnames(Antal_KT) <- Antal_KT[1,]
#Antal_KT <- as.data.frame(Antal_KT)
#names(Antal_KT)[1] <-"Indelning"
#AKT_1<- subset(Antal_KT, Indelning== "1")
#AKT_2<- subset(Antal_KT, Indelning== "2")
#AKT_3<- subset(Antal_KT, Indelning== "3")
#AKT_1 <- AKT_1[,-c(1:2)]
#AKT_2 <- AKT_2[,-c(1:2)]
#AKT_3 <- AKT_3[,-c(1:2)]
#AKT_1 <- t(AKT_1)
#AKT_1 <- as.data.frame(AKT_1)
#AKT_2 <- t(AKT_2)
#AKT_2 <- as.data.frame(AKT_2)
#AKT_3 <- t(AKT_3)
#AKT_3 <- as.data.frame(AKT_3)
#write.table(AKT_1, "AKT_1.csv", row.names = FALSE, sep = ",", dec = ".")
#write.table(AKT_2, "AKT_2.csv", row.names = FALSE, sep = ",", dec = ".")
#write.table(AKT_3, "AKT_3.csv", row.names = FALSE, sep = ",", dec = ".")
#View(AKT_2)



AKT_1<- read.table("AKT_1.csv", header = TRUE, sep = ",", dec = ".")
AKT_2<- read.table("AKT_2.csv", header = TRUE, sep = ",", dec = ".")
AKT_3<- read.table("AKT_3.csv", header = TRUE, sep = ",", dec = ".")
SAKT_1 <- rowSums(AKT_1, na.rm=FALSE, dims=1)
SAKT_2 <- rowSums(AKT_2, na.rm=FALSE, dims=1)
SAKT_3 <- rowSums(AKT_3, na.rm=FALSE, dims=1)
################################################### STOP HERE if you want to use the code down at row 84
#View(SAKT_1)
#View(SAKT_3)
AKT_1 <- ts(SAKT_1, frequency=12)
AKT_2 <- ts(SAKT_2, frequency=12)
AKT_3 <- ts(SAKT_3, frequency=12)
plot(AKT_1, ylim=range(0, 7000))
lines(AKT_2, col="red")
lines(AKT_3, col="blue")
#### Make index with 2017-10 as 1.0
SAKT_1 <- SAKT_1/1201
SAKT_2 <- SAKT_2/3014
SAKT_3 <- SAKT_3/1234
AKT_1 <- ts(SAKT_1, frequency=12, start=c(2017,10))
AKT_2 <- ts(SAKT_2, frequency=12, start=c(2017,10))
AKT_3 <- ts(SAKT_3, frequency=12, start=c(2017,10))
plot(AKT_1, ylim=range(0.35,1.3))    ######## NO clear differences
lines(AKT_2, col="red")
lines(AKT_3, col="blue")

####################################
#### ARIMA FOR COMPLETE DATASETS
####################### AKT_1.... Recommended ARIMA(0,0,0)(0,1,1)[12]
SAKT_1 <- ts(SAKT_1, frequency = 12, start=c(2017,10))
auto.arima(SAKT_1)
AKT_1M <- Arima(SAKT_1, seasonal=list(order=c(0L,1L,1L), period=12), order=c(0,0,0), include.drift = TRUE)
summary(AKT_1M)
Res.AKT_1M <- as.vector(residuals(AKT_1M))
fit.AKT_1M <- as.vector(fitted(AKT_1M))
Box.test(Res.AKT_1M,lag = 47, fitdf = 0, type="Ljung")
AKT_1M.forecast <- as.array(forecast(AKT_1M,h=12, level=c(80,95)))
autoplot(AKT_1M.forecast)
acf(AKT_1M$residuals)
plot(SAKT_1)
plot(AKT_1M$fitted, AKT_1M$residuals)
####################### AKT_2.... Recommended ARIMA(0,0,0)(1,1,0)[12]
SAKT_2 <- ts(SAKT_2, frequency = 12, start=c(2017,10))
auto.arima(SAKT_2)
AKT_2M <- Arima(SAKT_2 , seasonal = list(order=c(1L,1L, 0L), period=12),order=c(0,0,0), include.drift=TRUE)
summary(AKT_2M)
Res.AKT_2M <- as.vector(residuals(AKT_2M))
fit.AKT_2M <- as.vector(fitted(AKT_2M))
Box.test(Res.AKT_2M,lag = 47 , fitdf = 0, type="Ljung")
AKT_2M.forecast <- as.array(forecast(AKT_2M,h=12, level=c(80,95)))
autoplot(AKT_2M.forecast)
acf(AKT_2M$residuals)
plot(AKT_2M$fitted, AKT_2M$residuals)
####################### AKT_3   Recommended ARIMA(0,0,0)(1,1,0)[12]
SAKT_3 <- ts(SAKT_3, frequency = 12, start=c(2017,10))
auto.arima(SAKT_3)
AKT_3M <- Arima(SAKT_3 , seasonal = list(order=c(1L,1L, 0L), period=12),order=c(0,0,0), include.drift = TRUE )
summary(AKT_3M)
Res.AKT_3M <- as.vector(residuals(AKT_3M))
fit.AKT_3M <- as.vector(fitted(AKT_3M))
Box.test(Res.AKT_3M,lag = 47 , fitdf =0, type="Ljung")
AKT_3M.forecast <- as.array(forecast(AKT_3M,h=12, level=c(80,95)))
autoplot(AKT_3M.forecast)
acf(AKT_3M$residuals)
plot(AKT_3M$fitted, AKT_3M$residuals)
hist(AKT_3M$residuals)
#####################################################################
#####################################################################
#### SPLIT DATASETS AND CREATE ARIMA MODELS FOR THEM
############################################################# AKT_1
AKT_1_BC <-  SAKT_1[1:30]
AKT_1_BC <- as.data.frame(AKT_1_BC)
AKT_1_AC <-  SAKT_1[31:48]
AKT_1_AC <- as.data.frame(AKT_1_AC)
######################################
## BC          ARIMA (3,0,0)(0,1,0)[12]
AKT_1_BC <- ts(AKT_1_BC)
auto.arima(AKT_1_BC, allowmean = F)
AKT_1_BC <- Arima(AKT_1_BC,seasonal=list(order=c(0L,1L,0L), period=12), order=c(3,0,0))
summary(AKT_1_BC)
Res.AKT_1_BC <- as.vector(residuals(AKT_1_BC))
fit.AKT_1_BC <- as.vector(fitted(AKT_1_BC))
Box.test(Res.AKT_1_BC,lag = 27, fitdf =3, type="Ljung")
AKT_1_BC.forecast <- as.array(forecast(AKT_1_BC,h=12, level=c(80,95)))
autoplot(AKT_1_BC.forecast)
acf(AKT_1_BC$residuals)
### AC       ARIMA (1,0,0)(0,1,0)[12]
AKT_1_AC <- ts(AKT_1_AC)
auto.arima(AKT_1_AC, allowmean = F)
AKT_1_AC <- Arima(AKT_1_AC, seasonal=list(order=c(0L,1L,0L), period=12), order=c(1,0,0))
summary(AKT_1_AC)
Res.AKT_1_AC <- as.vector(residuals(AKT_1_AC))
fit.AKT_1_AC <- as.vector(fitted(AKT_1_AC))
Box.test(Res.AKT_1_AC,lag = 17, fitdf = 1, type="Ljung")
AKT_1_AC.forecast <- as.array(forecast(AKT_1_AC,h=12, level=c(80,95)))
autoplot(AKT_1_AC.forecast)
acf(AKT_1_AC$residuals)

################################################### AKT_2
AKT_2_BC <-  SAKT_2[1:30]
AKT_2_BC <- as.data.frame(AKT_2_BC)
AKT_2_AC <-  SAKT_2[31:48]
AKT_2_AC <- as.data.frame(AKT_2_AC)
######################################
## BC                         ARIMA(1,0,1)(0,1,0)[12]
AKT_2_BC <- ts(AKT_2_BC)
auto.arima(AKT_2_BC, allowmean = FALSE)
AKT_2_BC <- Arima(AKT_2_BC, seasonal=list(order=c(0L,1L,0L), period=12),order=c(1,0,1))
summary(AKT_2_BC)
Res.AKT_2_BC <- as.vector(residuals(AKT_2_BC))
fit.AKT_2_BC <- as.vector(fitted(AKT_2_BC))
Box.test(Res.AKT_2_BC,lag = 28, fitdf = 2, type="Ljung")
AKT_2_BC.forecast <- as.array(forecast(AKT_2_BC,h=12, level=c(80,95)))
autoplot(AKT_2_BC.forecast)
acf(AKT_2_BC$residuals)
### AC                   ARIMA(1,0,0)(0,1,0)[12]
AKT_2_AC <- ts(AKT_2_AC)
auto.arima(AKT_2_AC, allowmean = FALSE)
AKT_2_AC <- Arima(AKT_2_AC, seasonal=list(order=c(0L,1L,0L), period=12), order=c(1,0,0))
summary(AKT_2_AC)
Res.AKT_2_AC <- as.vector(residuals(AKT_2_AC))
fit.AKT_2_AC <- as.vector(fitted(AKT_2_AC))
Box.test(Res.AKT_2_AC,lag = 17, fitdf = 1, type="Ljung")
AKT_2_AC.forecast <- as.array(forecast(AKT_2_AC,h=12, level=c(80,95)))
autoplot(AKT_2_AC.forecast)
acf(AKT_2_AC$residuals)
############################################# AKT_3
AKT_3_BC <-  SAKT_3[1:30]
AKT_3_BC <- as.data.frame(AKT_3_BC)
AKT_3_AC <-  SAKT_3[31:48]
AKT_3_AC <- as.data.frame(AKT_3_AC)
######################################
## BC                    ARIMA(1,0,0)(0,1,0)[12]
AKT_3_BC <- ts(AKT_3_BC)
auto.arima(AKT_3_BC, allowmean = FALSE)
AKT_3_BC <- Arima(AKT_3_BC,seasonal=list(order=c(0L,1L,0L), period=12), order=c(1,0,0))
summary(AKT_3_BC)
Res.AKT_3_BC <- as.vector(residuals(AKT_3_BC))
fit.AKT_3_BC <- as.vector(fitted(AKT_3_BC))
Box.test(Res.AKT_3_BC,lag = 29, fitdf =1, type="Ljung")
AKT_3_BC.forecast <- as.array(forecast(AKT_3_BC,h=12, level=c(80,95)))
autoplot(AKT_3_BC.forecast)
acf(AKT_3_BC$residuals)
### AC                    ARIMA(0,1,0)(1,0,0)[12]
AKT_3_AC <- ts(AKT_3_AC)
auto.arima(AKT_3_AC, allowmean = FALSE)
AKT_3_AC <- Arima(AKT_3_AC,seasonal=list(order=c(0L,1L,0L), period=12), order=c(1,0,0))
summary(AKT_3_AC)
Res.AKT_3_AC <- as.vector(residuals(AKT_3_AC))
fit.AKT_3_AC <- as.vector(fitted(AKT_3_AC))
Box.test(Res.AKT_3_AC,lag = 17, fitdf = 1, type="Ljung")
AKT_3_AC.forecast <- as.array(forecast(AKT_3_AC,h=12, level=c(80,95)))
autoplot(AKT_3_AC.forecast)
acf(AKT_3_AC$residuals)