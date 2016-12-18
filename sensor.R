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

#locationSpeed is +0.41 correlated with expert level
E3 <- E2 %>% group_by(Time) %>% summarise(mean(locationSpeed))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(locationSpeed))
locationSpeed <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(locationSpeed) <- c("time","naive","expert")
ggplot(locationSpeed) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("locationSpeed")

#locationSpeed is +0.7 correlated with expert level
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





