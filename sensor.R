library(tidyr, dplyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(rpart)
library(ROCR)
library(Ecdat)
library(fpp)
library(forecast)

E1 <- read.csv("2016-12-15_12-00-32.csv", header = TRUE, stringsAsFactors = FALSE)
N1 <- read.csv("2016-12-15_12-00-36.csv", header = TRUE, stringsAsFactors = FALSE)

toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
} 
#code function reference: http://stackoverflow.com/questions/10835908/is-there-a-way-to-convert-mmss-00-to-seconds-00-in-r
N1$Time <- toSeconds(N1$loggingTime)
E1$Time <- toSeconds(E1$loggingTime)

N2 <- dplyr::select(N1, Time,locationAltitude,locationSpeed,accelerometerAccelerationX,accelerometerAccelerationY,+
                      accelerometerAccelerationZ,gyroRotationX,gyroRotationY,gyroRotationZ,+
                      motionYaw,motionRoll,motionPitch,motionRotationRateX,motionRotationRateY,motionRotationRateZ,+
                      motionUserAccelerationX,motionUserAccelerationY,motionUserAccelerationZ,motionQuaternionX,+
                      motionQuaternionY,motionQuaternionZ,motionQuaternionW,motionGravityX,motionGravityY,motionGravityZ,+
                      motionMagneticFieldX,motionMagneticFieldY,motionMagneticFieldZ)

N3 <- tidyr::gather(N2,Time)
names(N3) <- c("time","variable","value")
ggplot(N3)+ geom_line(aes(x=time, y=value))+facet_wrap(~variable, scales = "free")

E2 <- dplyr::select(E1, Time,locationAltitude,locationSpeed,accelerometerAccelerationX,accelerometerAccelerationY,+
                      accelerometerAccelerationZ,gyroRotationX,gyroRotationY,gyroRotationZ,motionYaw,+
                      motionRoll,motionPitch,motionRotationRateX,motionRotationRateY,motionRotationRateZ,motionUserAccelerationX,+
                      motionUserAccelerationY,motionUserAccelerationZ,motionQuaternionX,motionQuaternionY,motionQuaternionZ,+
                      motionQuaternionW,motionGravityX,motionGravityY,motionGravityZ,motionMagneticFieldX,motionMagneticFieldY,+
                      motionMagneticFieldZ)

E3 <- tidyr::gather(E2,Time)
names(E3) <- c("time","variable","value")
ggplot(E3)+ geom_line(aes(x=time, y=value))+facet_wrap(~variable, scales = "free")

E3$name <- "expert"
N3$name <- "naive"
names(E3) <- c("time","variable","value","name")
names(N3) <- c("time","variable","value","name")
D1 <- dplyr::bind_rows(N3, E3)

ggplot(D1)+ geom_line(aes(x=time, y=value,color=name))+facet_wrap(~variable, scales = "free")



E2$name <- "expert"
N2$name <- "naive"
D <- dplyr::bind_rows(N2, E2)
D$name<-ifelse(D$name=="naive",0,1)
D <- as.data.frame(D)

COR <- cor(D)
corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.01, insig = "blank")

pairs(D)

#Xiaoting's prediction model1: motionQuaternionZ(.98), motionQuaternionW(.96), motionQuaternionX(-.75)
c.tree1 <- rpart(name ~ motionQuaternionZ + motionQuaternionW + motionQuaternionX, method="class", data=D,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
#Plot and generate a CP table for your tree 
post(c.tree1, file = "tree1.ps", title = "tree1")
printcp(c.tree1)
#Since we were using two sensors of different phones for motion recording, this experiment might contain threats of internal validity
#due to instrumentation (looking at the graphs of motionQuaternionZ, motionQuaternionW and motionYaw, they might not be good variables to predict expect level)

#Xiaoting's prediction model2: motionQuaternionX(-.75), motionQuaternionY(0.67), motionGravityZ(-0.45)
c.tree2 <- rpart(name ~ motionQuaternionY + motionQuaternionX + motionGravityZ, method="class", data=D,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
#Plot and generate a CP table for your tree 
post(c.tree2, file = "tree2.ps", title = "tree2")
printcp(c.tree2)

#Variables actually used in tree construction:
#  [1] motionGravityZ    motionQuaternionX motionQuaternionY
#Root node error: 1153/3114 = 0.37026
#n= 3114 
#CP nsplit rel error   xerror      xstd
#1  0.6912402      0  1.000000 1.000000 0.0233703
#2  0.1465742      1  0.308760 0.318300 0.0156054
#3  0.0563747      2  0.162186 0.172593 0.0118374
#4  0.0199480      3  0.105811 0.117953 0.0098911
#5  0.0060711      4  0.085863 0.096271 0.0089733
#6  0.0039029      6  0.073721 0.077190 0.0080643
#7  0.0030356      8  0.065915 0.077190 0.0080643
#8  0.0026019     10  0.059844 0.066782 0.0075159
#9  0.0023128     12  0.054640 0.068517 0.0076103
#10 0.0021683     15  0.047702 0.067650 0.0075633
#11 0.0017346     17  0.043365 0.067650 0.0075633
#12 0.0010000     19  0.039896 0.064180 0.0073716

D$pred <- predict(c.tree2, type = "prob")[,2]
#Now you can generate the ROC curve for your model. You will need to install the package ROCR to do this.
#Plot the curve
pred1 <- prediction(D$pred, D$name) 
plot(performance(pred1, "tpr", "fpr"),colorize=TRUE)
abline(0, 1, lty = 2)
#Calculate the Area Under the Curve
AUC1 <- unlist(slot(performance(pred1,"auc"), "y.values"))#Unlist liberates the AUC value from the "performance" object created by ROCR
#[1] 0.9931463

#Model2: from ROC1, we can set the optimal threshold.pred1 as 0.58
threshold.pred1 = 0.58
D$threshold.pred1 <- ifelse(D$pred <= threshold.pred1, "0 r","1 r") 
#Now generate three diagnostics:
table1 <- table(D$name, D$threshold.pred1)
table1
#     0    1
#0 1930    31
#1   15  1138
accuracy1 <- (1930+1138)/(1930+1138+31+15) # [1] 0.985228
precision1 <- 1138/(1138+31) #[1] 0.9734816
recall1 <- 1138/(1138+15) #[1] 0.9869905

#________________________________________________________________________________________________________

#Xiaoyue's prediction model3: motionQuaternionX,accelerometerAccelerationX and motionPitch
c.tree3 <- rpart(name ~ motionQuaternionX + accelerometerAccelerationX + motionPitch, method="class", data=D,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
#Plot and generate a CP table for your tree 
post(c.tree3, file = "tree3.ps", title = "tree3")
printcp(c.tree3)

#Variables actually used in tree construction:
#  [1] accelerometerAccelerationX motionPitch                motionQuaternionX         
#Root node error: 1153/3114 = 0.37026
#n= 3114 
#CP nsplit rel error   xerror      xstd
#1  0.6912402      0  1.000000 1.000000 0.0233703
#2  0.0823938      1  0.308760 0.318300 0.0156054
#3  0.0260191      2  0.226366 0.240243 0.0137778
#4  0.0234172      6  0.082394 0.108413 0.0095001
#5  0.0156114      7  0.058977 0.067650 0.0075633
#6  0.0057820      8  0.043365 0.058109 0.0070224
#7  0.0026019     11  0.026019 0.043365 0.0060833
#8  0.0017346     12  0.023417 0.036427 0.0055827
#9  0.0013010     15  0.018213 0.032958 0.0053137
#10 0.0010000     20  0.011275 0.032090 0.0052442

D$pred2 <- predict(c.tree3, type = "prob")[,2]
#Now you can generate the ROC curve for your model. You will need to install the package ROCR to do this.
#Plot the curve
pred2 <- prediction(D$pred2, D$name) 
plot(performance(pred2, "tpr", "fpr"),colorize=TRUE)
abline(0, 1, lty = 2)
#Calculate the Area Under the Curve
AUC2 <-unlist(slot(performance(pred2,"auc"), "y.values"))#Unlist liberates the AUC value from the "performance" object created by ROCR
#[1] 0.9992698

#Model3: from ROC2, we can set the optimal threshold.pred1 as 0.7
threshold.pred2 = 0.7
D$threshold.pred2 <- ifelse(D$pred2 <= threshold.pred2, "0 r","1 r") 
#Now generate three diagnostics:
table2 <- table(D$name, D$threshold.pred2)
table2
#   0 r  1 r
#0 1950   11
#1    2 1151
accuracy2 <- (1950+1151)/(1950+1151+11+2) # [1] 0.9958253
precision2 <- 1151/(1151+11) #[1] 0.9905336
recall2 <- 1151/(1151+2) #[1] 0.9982654

#-------------------------------------------------------------------------------------------------------------------------------
#Jiaxi's prediction model4: accelerometerAccelerationY, motionpitch and motionQuaternionY
c.tree4 <- rpart(name ~ accelerometerAccelerationY + motionPitch + motionQuaternionY, method="class", data=D,control=rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))
#Plot and generate a CP table for your tree 
post(c.tree4, file = "tree4.ps", title = "tree4")
printcp(c.tree4)

#Variables actually used in tree construction:
#  [1] accelerometerAccelerationY motionPitch motionQuaternionY         
#Root node error: 1153/3114 = 0.37026
#n= 3114 
#CP nsplit rel error   xerror      xstd
#1  0.6973114      0  1.000000 1.000000 0.0233703
#2  0.0641804      1  0.302689 0.307892 0.0153816
#3  0.0384504      2  0.238508 0.239376 0.0137554
#4  0.0368604      5  0.123157 0.182134 0.0121372
#5  0.0052038      7  0.049436 0.062446 0.0072737
#6  0.0043365      8  0.044232 0.058109 0.0070224
#7  0.0026019     10  0.035559 0.045967 0.0062601
#8  0.0021683     11  0.032958 0.043365 0.0060833
#9  0.0017346     15  0.024284 0.041631 0.0059624
#10 0.0013010     18  0.019081 0.032958 0.0053137
#11 0.0010000     24  0.011275 0.030356 0.0051021

D$pred3 <- predict(c.tree4, type = "prob")[,2]
#Now you can generate the ROC curve for your model. You will need to install the package ROCR to do this.
#Plot the curve
pred3 <- prediction(D$pred3, D$name) 
plot(performance(pred3, "tpr", "fpr"),colorize=TRUE)
abline(0, 1, lty = 2)
#Calculate the Area Under the Curve
AUC3 <- unlist(slot(performance(pred3,"auc"), "y.values"))#Unlist liberates the AUC value from the "performance" object created by ROCR
#[1] 0.9988828

#Model4: from ROC3, we can set the optimal threshold.pred1 as 0.7
threshold.pred3 = 0.4
D$threshold.pred3 <- ifelse(D$pred3 <= threshold.pred3, "0 r","1 r") 
#Now generate three diagnostics:
table3 <- table(D$name, D$threshold.pred3)
table3
#   0 r  1 r
#0 1955    6
#1    7 1146
accuracy3 <- (1955+1146)/(1955+1146+6+7) # [1] 0.9958253
precision3 <- 1146/(1146+6) #[1] 0.9947917
recall3 <- 1146/(1146+7) #[1] 0.9939289

# summary-----------------------------------------------------------------------------

ModelCompare = matrix(c(AUC1, accuracy1,precision1,recall1,AUC2, accuracy2,precision2,recall2, AUC3, accuracy3,precision3,recall3),nrow=4,ncol=3) 
ModelCompare <- t(ModelCompare)
ModelCompare <- as.data.frame(ModelCompare)
names(ModelCompare) <- c("AUC", "accuracy","precision","recall")
ModelCompare
#       AUC  accuracy precision    recall
#1 0.9931463 0.9852280 0.9734816 0.9869905
#2 0.9992698 0.9958253 0.9905336 0.9982654
#3 0.9988828 0.9958253 0.9947917 0.9939289

#1. Xiaoting's model 2.Xiaoyue's model (best in recall, most accurate according to AUC) 3. Jiaxi's model(best in precision)
#---------------------------------------------------------------------------------------------------

#locationSpeed is +0.41 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(locationSpeed))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(locationSpeed))
locationSpeed <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(locationSpeed) <- c("time","naive","expert")
ggplot(locationSpeed) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("locationSpeed")

#motionYaw is +0.7 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionYaw))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionYaw))
motionYaw <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionYaw) <- c("time","naive","expert")
ggplot(motionYaw) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionYaw")

#motionRoll is -0.43 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionRoll))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionRoll))
motionRoll <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionRoll) <- c("time","naive","expert")
ggplot(motionRoll) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionRoll")

#motionQuaternionX is -0.75 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionQuaternionX))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionQuaternionX))
motionQuaternionX <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionQuaternionX) <- c("time","naive","expert")
ggplot(motionQuaternionX) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionQuaternionX")

#motionQuaternionY is +0.67 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionQuaternionY))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionQuaternionY))
motionQuaternionY <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionQuaternionY) <- c("time","naive","expert")
ggplot(motionQuaternionY) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionQuaternionY")

#motionQuaternionZ is +0.98 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionQuaternionZ))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionQuaternionZ))
motionQuaternionZ <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionQuaternionZ) <- c("time","naive","expert")
ggplot(motionQuaternionZ) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionQuaternionZ")

#motionQuaternionW is +0.96 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionQuaternionW))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionQuaternionW))
motionQuaternionW <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionQuaternionW) <- c("time","naive","expert")
ggplot(motionQuaternionW) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionQuaternionW")

#motionGravityZ is -0.45 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionGravityZ))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionGravityZ))
motionGravityZ <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionGravityZ) <- c("time","naive","expert")
ggplot(motionGravityZ) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionGravityZ")


