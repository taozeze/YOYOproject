library(tidyr, dplyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(rpart)
library(ROCR)
library(Ecdat)
library(fpp)
library(forecast)

N1 <- read.csv("2016-12-15_12-00-32.csv", header = TRUE, stringsAsFactors = FALSE)
E1 <- read.csv("2016-12-15_12-00-36.csv", header = TRUE, stringsAsFactors = FALSE)

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

E2 <- dplyr::select(E1, Time,locationAltitude,locationSpeed,accelerometerAccelerationX,accelerometerAccelerationY,+
                      accelerometerAccelerationZ,gyroRotationX,gyroRotationY,gyroRotationZ,motionYaw,+
                      motionRoll,motionPitch,motionRotationRateX,motionRotationRateY,motionRotationRateZ,motionUserAccelerationX,+
                      motionUserAccelerationY,motionUserAccelerationZ,motionQuaternionX,motionQuaternionY,motionQuaternionZ,+
                      motionQuaternionW,motionGravityX,motionGravityY,motionGravityZ,motionMagneticFieldX,motionMagneticFieldY,+
                      motionMagneticFieldZ)

as.numeric(D$Time)

E2$name <- "expert"
N2$name <- "naive"
D <- dplyr::bind_rows(N2, E2)
D$name<-ifelse(D$name=="naive",0,1)

COR <- cor(D)
corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.01, insig = "blank")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(locationAltitude))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(locationAltitude))
locationAltitude <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(locationAltitude) <- c("time","naive","expert")
ggplot(locationAltitude) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("locationAltitude")


E3 <- E2 %>% group_by(Time) %>% summarise(mean(locationSpeed))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(locationSpeed))
locationSpeed <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(locationSpeed) <- c("time","naive","expert")
ggplot(locationSpeed) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("locationSpeed")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(accelerometerAccelerationX))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(accelerometerAccelerationX))
accelerometerAccelerationX <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(accelerometerAccelerationX) <- c("time","naive","expert")
ggplot(accelerometerAccelerationX) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("accelerometerAccelerationX")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(accelerometerAccelerationY))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(accelerometerAccelerationY))
accelerometerAccelerationY <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(accelerometerAccelerationY) <- c("time","naive","expert")
ggplot(accelerometerAccelerationY) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("accelerometerAccelerationY")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(accelerometerAccelerationZ))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(accelerometerAccelerationZ))
accelerometerAccelerationZ <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(accelerometerAccelerationZ) <- c("time","naive","expert")
ggplot(accelerometerAccelerationZ) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("accelerometerAccelerationZ")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(gyroRotationX))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(gyroRotationX))
gyroRotationX <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(gyroRotationX) <- c("time","naive","expert")
ggplot(gyroRotationX) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("gyroRotationX")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(gyroRotationY))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(gyroRotationY))
gyroRotationY <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(gyroRotationY) <- c("time","naive","expert")
ggplot(gyroRotationY) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("gyroRotationY")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(gyroRotationZ))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(gyroRotationZ))
gyroRotationZ <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(gyroRotationZ) <- c("time","naive","expert")
ggplot(gyroRotationZ) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("gyroRotationZ")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionYaw))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionYaw))
motionYaw <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionYaw) <- c("time","naive","expert")
ggplot(motionYaw) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionYaw")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionRoll))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionRoll))
motionRoll <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionRoll) <- c("time","naive","expert")
ggplot(motionRoll) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionRoll")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionPitch))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionPitch))
motionPitch <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionPitch) <- c("time","naive","expert")
ggplot(motionPitch) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionPitch")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionRotationRateX))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionRotationRateX))
motionRotationRateX <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionRotationRateX) <- c("time","naive","expert")
ggplot(motionRotationRateX) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionRotationRateX")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionRotationRateY))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionRotationRateY))
motionRotationRateY <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionRotationRateY) <- c("time","naive","expert")
ggplot(motionRotationRateY) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionRotationRateY")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionRotationRateZ))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionRotationRateZ))
motionRotationRateZ <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionRotationRateZ) <- c("time","naive","expert")
ggplot(motionRotationRateZ) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionRotationRateZ")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionQuaternionX))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionQuaternionX))
motionQuaternionX <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionQuaternionX) <- c("time","naive","expert")
ggplot(motionQuaternionX) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionQuaternionX")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionQuaternionY))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionQuaternionY))
motionQuaternionY <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionQuaternionY) <- c("time","naive","expert")
ggplot(motionQuaternionY) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionQuaternionY")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionQuaternionZ))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionQuaternionZ))
motionQuaternionZ <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionQuaternionZ) <- c("time","naive","expert")
ggplot(motionQuaternionZ) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionQuaternionZ")

E3 <- E2 %>% group_by(Time) %>% summarise(mean(motionQuaternionW))
N3 <- N2 %>% group_by(Time) %>% summarise(mean(motionQuaternionW))
motionQuaternionW <- as.data.frame(dplyr::inner_join(N3, E3, by="Time"))
names(motionQuaternionW) <- c("time","naive","expert")
ggplot(motionQuaternionW) + geom_line(aes(time, naive, color="naive")) + 
  geom_line(aes(time, expert, color="expert")) + 
  xlab("Time(s)") + ylab("motionQuaternionW")





#  motionUserAccelerationX,motionUserAccelerationY,motionUserAccelerationZ,+
#  motionGravityX,motionGravityY,motionGravityZ,+
#  motionMagneticFieldX,motionMagneticFieldY,motionMagneticFieldZ)



#E3 <- dplyr::select(E2, loggingTime)
#E3 <- unique(E3)
#N3 <- dplyr::select(N2, loggingTime)
#N3 <- unique(N3)
#D3 <- dplyr::inner_join(N3, E3, by="loggingTime")

