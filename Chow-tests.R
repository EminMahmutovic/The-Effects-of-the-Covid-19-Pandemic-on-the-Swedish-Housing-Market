setwd("C:/Users/Hampu/Desktop/STAT III C-uppsats")
##Vi använder nog i dagsläget inte alla dessa paketen, men är bra att ha dem här =) 
library(forecast)
library(zoo)
library (fpp2)
library(lmtest)
library(missForest)
library(imputeTS)
library(qcc)
library(tibble)
library(nlme)
library(car)
library(orcutt)
library(vars)
library(bsts)
################SPlit the data for the different KB_
##### KB_1
KB_1_Split<- read.table("KB_1_Mean.csv", header = TRUE, sep = ",", dec = ".")
KB_1_BC <- KB_1_Split[1:30, ]
KB_1_BC <- as.data.frame(KB_1_BC)
names(KB_1_BC)[1] <-"Kboy"
KB_1_BC <- KB_1_BC/22820.04
KB_1_AC <- KB_1_Split[31:48,]
KB_1_AC <- as.data.frame(KB_1_AC)
names(KB_1_AC)[1] <-"Kboy"
KB_1_AC <- KB_1_AC/23554.10
##### KB_2
KB_2_Split<- read.table("KB_2_Mean.csv", header = TRUE, sep = ",", dec = ".")
KB_2_BC <- KB_2_Split[1:30, ]
KB_2_BC <- as.data.frame(KB_2_BC)
names(KB_2_BC)[1] <-"Kboy"
KB_2_BC <- KB_2_BC/19491.76
KB_2_AC <- KB_2_Split[31:48,]
KB_2_AC <- as.data.frame(KB_2_AC)
names(KB_2_AC)[1] <-"Kboy"
KB_2_AC <- KB_2_AC/19913.49
##### KB_3
KB_3_Split<- read.table("KB_3_Mean.csv", header = TRUE, sep = ",", dec = ".")
KB_3_BC <- KB_3_Split[1:30, ]
KB_3_BC <- as.data.frame(KB_3_BC)
names(KB_3_BC)[1] <-"Kboy"
KB_3_BC <- KB_3_BC/39131.79
KB_3_AC <- KB_3_Split[31:48,]
KB_3_AC <- as.data.frame(KB_3_AC)
names(KB_3_AC)[1] <-"Kboy"
KB_3_AC <- KB_3_AC/38717.73

#################################
#################################
### FOR THE CHOW TEST, SCTEST(), IF P-VALUE > 0.05 THEN THERE IS NO STRUCTURE BREAK. IF THERE IS NO STRUCTURE BREAK WE CAN´T SAY THAT 
### THE DATASETS DIFFER. THEY MAY STILL BE ON DIFFERENT LEVELS, BUT THE OVERALL STRUCTURE (MOVEMENT OF THE GRAPHS) ARE THE SAME. 
################################
################################
##### FIRST WE TEST FOR COMPLETE DATASETS. SETTING BREAKPOINT AT T=30
KB_1_Split <- KB_1_Split/22820.04
KB_2_Split <- KB_2_Split/19491.76
KB_3_Split <- KB_3_Split/39131.79
sctest(KB_1_Split$x ~ KB_2_Split$x, type="Chow", point=30)
sctest(KB_2_Split$x ~ KB_1_Split$x, type="Chow", point=30)

sctest(KB_1_Split$x ~ KB_3_Split$x, type="Chow", point=30)
sctest(KB_3_Split$x ~ KB_1_Split$x, type="Chow", point=30)

sctest(KB_2_Split$x ~ KB_3_Split$x, type="Chow", point=30) 
sctest(KB_3_Split$x ~ KB_2_Split$x, type="Chow", point=30) 
### PLOT THE SERIES, PLOTTED AS INDEX, SINCE ITS EASIER TO GET AN OVERVIEW
KB_1_SPLIT_TS <- ts(KB_1_Split, frequency = 12, start=c(2017,10))
KB_2_SPLIT_TS <- ts(KB_2_Split, frequency = 12, start=c(2017,10))
KB_3_SPLIT_TS <- ts(KB_3_Split, frequency = 12, start=c(2017,10))
plot(KB_1_SPLIT_TS, main="Index of KB, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1")
lines(KB_2_SPLIT_TS, col="red")
lines(KB_3_SPLIT_TS, col="blue") ####### WE SEE THAT KB_1 AND KB_2 ARE SIMILAR AFTER T=30 IN THE COMPLETE DATASET. 
####ALSO KB_1 AND KB_3 ARE CONSIDERED SIMILAR ACCORDING TO THE TEST. THAT IS NOT SO CLEAR IN THE PLOT

########### CHOW TEST FOR BC. 
###### Test KB_1_BC against KB_2_BC
sctest(KB_1_BC$Kboy ~ KB_2_BC$Kboy, type="Chow", point=3)
sctest(KB_2_BC$Kboy ~ KB_1_BC$Kboy, type="Chow", point=3)
###### Test KB_1_BC against KB_3_BC
sctest(KB_1_BC$Kboy ~ KB_3_BC$Kboy, type="Chow", point=3)
sctest(KB_3_BC$Kboy ~ KB_1_BC$Kboy, type="Chow", point=3)
###### Test KB_1_BC against KB_3_BC
sctest(KB_2_BC$Kboy ~ KB_3_BC$Kboy, type="Chow", point=3)
sctest(KB_3_BC$Kboy ~ KB_2_BC$Kboy, type="Chow", point=3)
########### PLOT BC
KB_1_BC_TS <- ts(KB_1_BC, frequency=12, start=c(2017,10))
KB_2_BC_TS <- ts(KB_2_BC, frequency=12, start=c(2017,10))
KB_3_BC_TS <- ts(KB_3_BC, frequency=12, start=c(2017,10))
plot(KB_1_BC_TS, ylim=range(0.937, 1.045),main="Index of KB_BC, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1"  )
lines(KB_2_BC_TS, col="red")
lines(KB_3_BC_TS, col="blue")   ###### IT IS EASY TO SEE THAT KB_1 AND KB_3 ARE QUITE "CLOSE" TO EACHOTHER, WHILE KB_2 IS NOT. 
### BUT KB_2 AND KB_3 FOLLOWS A SIMILAR PATTERN. IT´S QUITE CLEAR FROM ABOUT 2018.5 AND FORWARD. 

########### CHOW TEST FOR AC. 
###### Test KB_1_AC against KB_2_AC
sctest(KB_1_AC$Kboy ~ KB_2_AC$Kboy, type="Chow", point=3)
sctest(KB_2_AC$Kboy ~ KB_1_AC$Kboy, type="Chow", point=3)
###### Test KB_1_AC against KB_3_AC
sctest(KB_1_AC$Kboy ~ KB_3_AC$Kboy, type="Chow", point=3)
sctest(KB_3_AC$Kboy ~ KB_1_AC$Kboy, type="Chow", point=3)
###### Test KB_1_AC against KB_3_AC
sctest(KB_2_AC$Kboy ~ KB_3_AC$Kboy, type="Chow", point=3)
sctest(KB_3_AC$Kboy ~ KB_2_AC$Kboy, type="Chow", point=3)
########### PLOT BC
KB_1_AC_TS <- ts(KB_1_AC, frequency=12, start=c(2020,03))
KB_2_AC_TS <- ts(KB_2_AC, frequency=12, start=c(2020,03))
KB_3_AC_TS <- ts(KB_3_AC, frequency=12, start=c(2020,03))
plot(KB_1_AC_TS , main="Index of KB_AC, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, march 2020 = 1" )
lines(KB_2_AC_TS, col="red")
lines(KB_3_AC_TS, col="blue") ######## WE CANT SEE ANY CLEAR DIFFERENCE IN THE GRAPHS HERE. THE CHANGE IS VERY SIMILAR 


#################################################
#################################################
#################################################
###   KT
###### DIVIDE THE DATASET INTO THE 3 DIFFERENT TYPES FIRST:
kt <- read.table("Data - makstat_villor - K_T_kommuner_imp.csv", header = TRUE, sep = ",", dec = ".")
KT_1_Split <- subset(kt, Indelning.typ.3 == "1")
rownames(KT_1_Split)<-KT_1_Split[,1]
KT_1_Split<-KT_1_Split[-c(1,2,3)]
KT_1_Split <- t(KT_1_Split)
KT_1_Split_rs <- rowSums(KT_1_Split, na.rm=FALSE, dims=1)
KT_1_Split <- KT_1_Split_rs/ncol(KT_1_Split)
KT_1_Split <- as.data.frame(KT_1_Split)
colnames(KT_1_Split)[1] <- "KT"

kt <- read.table("Data - makstat_villor - K_T_kommuner_imp.csv", header = TRUE, sep = ",", dec = ".")
KT_2_Split <- subset(kt, Indelning.typ.3 == "2")
rownames(KT_2_Split)<-KT_2_Split[,1]
KT_2_Split<-KT_2_Split[-c(1,2,3)]
KT_2_Split <- t(KT_2_Split)
KT_2_Split_rs <- rowSums(KT_2_Split, na.rm=FALSE, dims=1)
KT_2_Split <- KT_2_Split_rs/ncol(KT_2_Split)
KT_2_Split <- as.data.frame(KT_2_Split)
colnames(KT_2_Split)[1] <- "KT"

kt <- read.table("Data - makstat_villor - K_T_kommuner_imp.csv", header = TRUE, sep = ",", dec = ".")
KT_3_Split <- subset(kt, Indelning.typ.3 == "3")
rownames(KT_3_Split)<-KT_3_Split[,1]
KT_3_Split<-KT_3_Split[-c(1,2,3)]
KT_3_Split <- t(KT_3_Split)
KT_3_Split_rs <- rowSums(KT_3_Split, na.rm=FALSE, dims=1)
KT_3_Split <- KT_3_Split_rs/ncol(KT_3_Split)
KT_3_Split <- as.data.frame(KT_3_Split)
colnames(KT_3_Split)[1] <- "KT"
################SPlit the data for the different KT_
##### KT_1
KT_1_BC <- KT_1_Split[1:30, ]
KT_1_BC <- as.data.frame(KT_1_BC)
names(KT_1_BC)[1] <-"KT"
KT_1_BC <- KT_1_BC/1.744572
KT_1_AC <- KT_1_Split[31:48,]
KT_1_AC <- as.data.frame(KT_1_AC)
names(KT_1_AC)[1] <-"KT"
KT_1_AC <- KT_1_AC/1.927352
##### KT_2
KT_2_BC <- KT_2_Split[1:30, ]
KT_2_BC <- as.data.frame(KT_2_BC)
names(KT_2_BC)[1] <-"KT"
KT_2_BC <- KT_2_BC/1.692840
KT_2_AC <- KT_2_Split[31:48,]
KT_2_AC <- as.data.frame(KT_2_AC)
names(KT_2_AC)[1] <-"KT"
KT_2_AC <- KT_2_AC/1.807489
##### KT_3
KT_3_BC <- KT_3_Split[1:30, ]
KT_3_BC <- as.data.frame(KT_3_BC)
names(KT_3_BC)[1] <-"KT"
KT_3_BC <- KT_3_BC/1.493609
KT_3_AC <- KT_3_Split[31:48,]
KT_3_AC <- as.data.frame(KT_3_AC)
names(KT_3_AC)[1] <-"KT"
KT_3_AC <- KT_3_AC/1.539660

##### FIRST WE TEST FOR COMPLETE DATASETS. SETTING BREAKPOINT AT T=30
KT_1_Split <- KT_1_Split/1.744572
KT_2_Split <- KT_2_Split/1.692840
KT_3_Split <- KT_3_Split/1.493609

sctest(KT_1_Split$KT ~ KT_2_Split$KT, type="Chow", point=30)
sctest(KT_2_Split$KT ~ KT_1_Split$KT, type="Chow", point=30)

sctest(KT_1_Split$KT ~ KT_3_Split$KT, type="Chow", point=30)
sctest(KT_3_Split$KT ~ KT_1_Split$KT, type="Chow", point=30)

sctest(KT_2_Split$KT ~ KT_3_Split$KT, type="Chow", point=30) 
sctest(KT_3_Split$KT ~ KT_2_Split$KT, type="Chow", point=30) 
### PLOT THE SERIES, PLOTTED AS INDEX, SINCE ITS EASIER TO GET AN OVERVIEW
KT_1_SPLIT_TS <- ts(KT_1_Split, frequency = 12, start=c(2017,10))
KT_2_SPLIT_TS <- ts(KT_2_Split, frequency = 12, start=c(2017,10))
KT_3_SPLIT_TS <- ts(KT_3_Split, frequency = 12, start=c(2017,10))
KT_3_SPLIT_TS
plot(KT_1_SPLIT_TS, ylim=range(0.92, 1.40), main="Index of KT Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1")
lines(KT_2_SPLIT_TS, col="red")
lines(KT_3_SPLIT_TS, col="blue") ####### THERE ARE CLEAR SIMILARITIES IN THE PATTERN OF KT_2 AND KT_3. 
###KT_1 AND KT_2 ARE CLOSE TO EACHOTHER, BUT NOT SIMILIAR IN PATTERN.

########### CHOW TEST FOR BC. 
###### Test KT_1_BC against KT_2_BC
sctest(KT_1_BC$KT ~ KT_2_BC$KT, type="Chow", point=3)
sctest(KT_2_BC$KT ~ KT_1_BC$KT, type="Chow", point=3)
###### Test KT_1_BC against KT_3_BC
sctest(KT_1_BC$KT ~ KT_3_BC$KT, type="Chow", point=3)
sctest(KT_3_BC$KT ~ KT_1_BC$KT, type="Chow", point=3)
###### Test KT_1_BC against KT_3_BC
sctest(KT_2_BC$KT ~ KT_3_BC$KT, type="Chow", point=3)
sctest(KT_3_BC$KT ~ KT_2_BC$KT, type="Chow", point=3)
########### PLOT BC
KT_1_BC_TS <- ts(KT_1_BC, frequency=12, start=c(2017,10))
KT_2_BC_TS <- ts(KT_2_BC, frequency=12, start=c(2017,10))
KT_3_BC_TS <- ts(KT_3_BC, frequency=12, start=c(2017,10))
plot(KT_1_BC_TS, ylim=range(0.945, 1.11) ,main="Index of KT_BC Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1")
lines(KT_2_BC_TS, col="red")
lines(KT_3_BC_TS, col="blue")   ###### tHEY ARE FAR APART, BUT THEY ALL HAVE A SIMILAR PATTERN.

########### CHOW TEST FOR AC. 
###### Test KT_1_AC against KT_2_AC
sctest(KT_1_AC$KT ~ KT_2_AC$KT, type="Chow", point=3)
sctest(KT_2_AC$KT ~ KT_1_AC$KT, type="Chow", point=3)
###### Test KT_1_AC against KT_3_AC
sctest(KT_1_AC$KT ~ KT_3_AC$KT, type="Chow", point=3)
sctest(KT_3_AC$KT ~ KT_1_AC$KT, type="Chow", point=3)
###### Test KT_1_AC against KT_3_AC
sctest(KT_2_AC$KT ~ KT_3_AC$KT, type="Chow", point=3)
sctest(KT_3_AC$KT ~ KT_2_AC$KT, type="Chow", point=3)
########### PLOT BC
KT_1_AC_TS <- ts(KT_1_AC, frequency=12, start=c(2020,03))
KT_2_AC_TS <- ts(KT_2_AC, frequency=12, start=c(2020,03))
KT_3_AC_TS <- ts(KT_3_AC, frequency=12, start=c(2020,03))
plot(KT_1_AC_TS ,main="Index of KT_AC Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, march 2020 = 1", ylim=range(1.0, 1.25))
lines(KT_2_AC_TS, col="red")
lines(KT_3_AC_TS, col="blue")  ###### KT_1 AND KT_2 ARE HAVE SIMILAR PATTERN.

##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################
##########################################################

########## ANTAL KT

AKT_1<- read.table("AKT_1.csv", header = TRUE, sep = ",", dec = ".")
AKT_2<- read.table("AKT_2.csv", header = TRUE, sep = ",", dec = ".")
AKT_3<- read.table("AKT_3.csv", header = TRUE, sep = ",", dec = ".")
SAKT_1 <- rowSums(AKT_1, na.rm=FALSE, dims=1)
SAKT_2 <- rowSums(AKT_2, na.rm=FALSE, dims=1)
SAKT_3 <- rowSums(AKT_3, na.rm=FALSE, dims=1)
################################################### 
SAKT_1_BC <- SAKT_1[1:30]
SAKT_2_BC <- SAKT_2[1:30]
SAKT_3_BC <- SAKT_3[1:30]
SAKT_1_AC <- SAKT_1[31:48]
SAKT_2_AC <- SAKT_2[31:48]
SAKT_3_AC <- SAKT_3[31:48]

SAKT_1 <- SAKT_1/1201
SAKT_2 <- SAKT_2/3014
SAKT_3 <- SAKT_3/1234
AKT_1 <- ts(SAKT_1, frequency=12, start=c(2017,10))
AKT_2 <- ts(SAKT_2, frequency=12, start=c(2017,10))
AKT_3 <- ts(SAKT_3, frequency=12, start=c(2017,10))
plot(AKT_1, ylim=range(0.35,1.3),main="Index of houses sold, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1")    ######## NO clear differences
lines(AKT_2, col="red")
lines(AKT_3, col="blue")

############ CHOW TEST FOR AKT
######## TEST AKT_1 AGAINST AKT_2
sctest(AKT_1 ~AKT_2, type="Chow", point=30)
sctest(AKT_2 ~AKT_1, type="Chow", point=30)
######## Test AKT_1 AGAINST AKT_3
sctest(AKT_1 ~AKT_3, type="Chow", point=30)
sctest(AKT_3 ~AKT_1, type="Chow", point=30)
######## TEST AKT_2 AGAINST AKT_3
sctest(AKT_2 ~AKT_3, type="Chow", point=30)
sctest(AKT_3 ~AKT_2, type="Chow", point=30)
####
plot(AKT_1, ylim=range(0.35,1.3),main="Index of houses sold, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1")    ######## NO clear differences
lines(AKT_2, col="red")
lines(AKT_3, col="blue") #### According to the test all the timeseries differ. It´s quite hard to see if there is any differences or not in the graphs.

##### CHOW TEST FOR AKT_BC
AKT_1_BC <- SAKT_1_BC/1201
AKT_2_BC <- SAKT_2_BC/3014
AKT_3_BC <- SAKT_3_BC/1234
########## TEST AKT_1_BC and AKT_2_BC
sctest(AKT_1_BC ~ AKT_2_BC, type="Chow", point=3)
sctest(AKT_2_BC ~ AKT_1_BC, type="Chow", point=3)
########## TEST AKT_1_BC and AKT_3_BC
sctest(AKT_1_BC ~ AKT_3_BC, type="Chow", point=3)
sctest(AKT_3_BC ~ AKT_1_BC, type="Chow", point=3)
########## TEST AKT_2_BC and AKT_3_BC
sctest(AKT_2_BC ~ AKT_3_BC, type="Chow", point=3)
sctest(AKT_3_BC ~ AKT_2_BC, type="Chow", point=3)
#### Plot the series
AKT_1_BC_TS <- ts(AKT_1_BC, frequency = 12, start=c(2017,10))
AKT_2_BC_TS <- ts(AKT_2_BC, frequency = 12, start=c(2017,10))
AKT_3_BC_TS <- ts(AKT_3_BC, frequency = 12, start=c(2017,10))
plot(AKT_1_BC_TS ,ylim=range(0.35,1.3),main="Index of houses sold for time period BC, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1")
lines(AKT_2_BC_TS, col="red")
lines(AKT_3_BC_TS, col="blue") ##### All time series differ. It´s hard to tell from the graph however. 


##### CHOW TEST FOR AKT_AC
AKT_1_AC <- SAKT_1_AC/1220
AKT_2_AC <- SAKT_2_AC/2872
AKT_3_AC <- SAKT_3_AC/1159
########## TEST AKT_1_AC and AKT_2_AC
sctest(AKT_1_AC ~ AKT_2_AC, type="Chow", point=3)
sctest(AKT_2_AC ~ AKT_1_AC, type="Chow", point=3)

########## TEST AKT_1_AC and AKT_3_AC
sctest(AKT_1_AC ~ AKT_3_AC, type="Chow", point=3)
sctest(AKT_3_AC ~ AKT_1_AC, type="Chow", point=3)
########## TEST AKT_2_AC and AKT_3_AC
sctest(AKT_2_AC ~ AKT_3_AC, type="Chow", point=3)
sctest(AKT_3_AC ~ AKT_2_AC, type="Chow", point=3)
#### Plot the series
AKT_1_AC_TS <- ts(AKT_1_AC, frequency = 12, start=c(2017,10))
AKT_2_AC_TS <- ts(AKT_2_AC, frequency = 12, start=c(2017,10))
AKT_3_AC_TS <- ts(AKT_3_AC, frequency = 12, start=c(2017,10))
plot(AKT_1_AC_TS,ylim=range(0.,1.35),main="Index of houses sold for time period AC, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, march 2020 = 1" )
lines(AKT_2_AC_TS, col="red")
lines(AKT_3_AC_TS, col="blue") #### As can be seen in the other AKT tests this one differs aswell.
##################################

##### ANTAL KB
Antal_KB<- read.csv("Data - makstat_bostadsratter - Antal_Kommuner.csv", header=TRUE, sep = ",", dec = ".")
rownames(Antal_KB) <- Antal_KB[,1]
Antal_KB <- Antal_KB[,-1]
AKB_1<- subset(Antal_KB, Indelning.typ.3 == "1")
AKB_2<- subset(Antal_KB, Indelning.typ.3 == "2")
AKB_3<- subset(Antal_KB, Indelning.typ.3 == "3")

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

AKB_1 <- ts(SAKB_1, frequency=12, start=c(2017,10))
AKB_2 <- ts(SAKB_2, frequency=12, start=c(2017,10))
AKB_3 <- ts(SAKB_3, frequency=12, start=c(2017,10))
plot(AKB_1, ylim=range(500,8000) ,main="Total amount of sold condominiums, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="condominium")
lines(AKB_2, col="red")
lines(AKB_3, col="blue")

SAKB_1_BC <- SAKB_1[1:30]
SAKB_2_BC <- SAKB_2[1:30]
SAKB_3_BC <- SAKB_3[1:30]
AKB_1_BC <- SAKB_1_BC/641
AKB_2_BC <- SAKB_2_BC/4518
AKB_3_BC <- SAKB_3_BC/5624

SAKB_1_AC <- SAKB_1[31:48]
SAKB_2_AC <- SAKB_2[31:48]
SAKB_3_AC <- SAKB_3[31:48]

AKB_1_AC <- SAKB_1_AC/543
AKB_2_AC <- SAKB_2_AC/3838
AKB_3_AC <- SAKB_3_AC/4653

AKB_1 <- SAKB_1/641
AKB_2 <- SAKB_2/4518
AKB_3 <- SAKB_3/5624
#########

### CHOW TEST AKB
### TEST AKB_1 AGAINST AKB_2
sctest(AKB_1 ~ AKB_2, type="Chow", p=30)
sctest(AKB_2 ~ AKB_1, type="Chow", p=30)
### TEST AKB_1 AGAINST AKB_3
sctest(AKB_1 ~ AKB_3, type="Chow", p=30)
sctest(AKB_3 ~ AKB_1, type="Chow", p=30)
### TEST AKB_2 AGAINST AKB_3
sctest(AKB_2 ~ AKB_3, type="Chow", p=30)
sctest(AKB_3 ~ AKB_2, type="Chow", p=30)
## Plot
AKB_1 <- ts(AKB_1, frequency=12, start=c(2017,10))
AKB_2 <- ts(AKB_2, frequency=12, start=c(2017,10))
AKB_3 <- ts(AKB_3, frequency=12, start=c(2017,10))
plot(AKB_1, ylim=range(0.45, 1.45),main="Index of sold condominiums, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1")
lines(AKB_2, col="red")
lines(AKB_3, col="blue") #### The Chow test tells us that the only AKB_2 and AKB_3 have a structure break. AKB_1 and AKB_2 aswell as AKB_1 and AKB_3 
## does not have a structure break, as far as the test goes. Not something that can be seen in the graphs. 

### CHOW TEST AKB_BC
### TEST AKB_1_BC AGAINST AKB_2_BC
sctest(AKB_1_BC ~ AKB_2_BC, type="Chow", p=3)
sctest(AKB_2_BC ~ AKB_1_BC, type="Chow", p=3)
### TEST AKB_1_BC AGAINST AKB_3_BC
sctest(AKB_1_BC ~ AKB_3_BC, type="Chow", p=3)
sctest(AKB_3_BC ~ AKB_1_BC, type="Chow", p=3)
### TEST AKB_2_BC AGAINST AKB_3_BC
sctest(AKB_2_BC ~ AKB_3_BC, type="Chow", p=3)
sctest(AKB_3_BC ~ AKB_2_BC, type="Chow", p=3)
## Plot
AKB_1_BC <- ts(AKB_1_BC, frequency=12, start=c(2017,10))
AKB_2_BC <- ts(AKB_2_BC, frequency=12, start=c(2017,10))
AKB_3_BC <- ts(AKB_3_BC, frequency=12, start=c(2017,10))
plot(AKB_1_BC, ylim=range(0.45, 1.25),main="Index of sold condominiums for time period BC, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, october 2017 = 1 ")
lines(AKB_2_BC, col="red")
lines(AKB_3_BC, col="blue") #### Structure break in all the tests. 

## CHOW TEST AKB_AC
### TEST AKB_1_AC AGAINST AKB_2_AC
sctest(AKB_1_AC ~ AKB_2_AC, type="Chow", p=3)
sctest(AKB_2_AC ~ AKB_1_AC, type="Chow", p=3)
### TEST AKB_1_AC AGAINST AKB_3_AC
sctest(AKB_1_AC ~ AKB_3_AC, type="Chow", p=3)
sctest(AKB_3_AC ~ AKB_1_AC, type="Chow", p=3)
### TEST AKB_2_AC AGAINST AKB_3_AC
sctest(AKB_2_AC ~ AKB_3_AC, type="Chow", p=3)
sctest(AKB_3_AC ~ AKB_2_AC, type="Chow", p=3)

## Plot
AKB_1_AC <- ts(AKB_1_AC, frequency=12, start=c(2017,10))
AKB_2_AC <- ts(AKB_2_AC, frequency=12, start=c(2017,10))
AKB_3_AC <- ts(AKB_3_AC, frequency=12, start=c(2017,10))
plot(AKB_1_AC, ylim=range(0.70,1.75),main="Index of sold condominiums for time period AC, Type=1 (Black), Type=2 (Red), Type=3 (Blue)", xlab="Years",ylab="Index, march 2020 = 1 ")
lines(AKB_2_AC, col="red")
lines(AKB_3_AC, col="blue") ## Structure break in all the tests. 

ggplot(AKB_1_AC, aes(colour= "red")) + geom_histogram()
