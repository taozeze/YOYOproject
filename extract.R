#By @Edward
library(fpp)
normal <- read.csv("2016-12-15_12-00-36.csv", header = TRUE, stringsAsFactors = FALSE)
export <- read.csv("2016-12-15_12-00-32.csv", header = TRUE, stringsAsFactors = FALSE)
par(mfrow=c(2,2))
plot(normal$loggingSample, normal$motionYaw)
lines(normal$loggingSample, normal$motionYaw)
plot(export$loggingSample, export$motionYaw)
lines(export$loggingSample, export$motionYaw)
summary(lm(normal$motionYaw~normal$loggingSample))
summary(lm(export$motionYaw~export$loggingSample))
# a weird W shape distribution, what is this?????


plot(normal$loggingSample, normal$motionPitch)
lines(normal$loggingSample, normal$motionPitch)
abline(lm(normal$motionPitch~normal$loggingSample))
plot(export$loggingSample, export$motionPitch)
lines(export$loggingSample, export$motionPitch)
abline(lm(export$motionPitch~export$loggingSample))
summary(lm(normal$motionPitch~normal$loggingSample))
summary(lm(export$motionPitch~export$loggingSample))
sum(resid(lm(normal$motionPitch~normal$loggingSample)))*10^15
sum(resid(lm(export$motionPitch~export$loggingSample)))*10^15
# like an upside down version of the previous one, dense on the bottom

plot(normal$loggingSample, normal$motionRotationRateX)
lines(normal$loggingSample, normal$motionRotationRateX)
plot(export$loggingSample, export$motionRotationRateX)
lines(export$loggingSample, export$motionRotationRateX)
summary(lm(normal$loggingSample~normal$motionRotationRateX))
summary(lm(export$loggingSample~export$motionRotationRateX))

plot(normal$loggingSample, normal$motionRotationRateY)
lines(normal$loggingSample, normal$motionRotationRateY)
plot(export$loggingSample, export$motionRotationRateY)
lines(export$loggingSample, export$motionRotationRateY)
summary(lm(normal$loggingSample~normal$motionRotationRateY))
summary(lm(export$loggingSample~export$motionRotationRateY))

plot(normal$loggingSample, normal$motionRotationRateZ)
lines(normal$loggingSample, normal$motionRotationRateZ)
plot(export$loggingSample, export$motionRotationRateZ)
lines(export$loggingSample, export$motionRotationRateZ)
normal_rot = lm(normal$loggingSample~normal$motionRotationRateZ)
export_rot = lm(export$loggingSample~export$motionRotationRateZ)
summary(normal_rot)
mean(export$motionRotationRateZ)
resid = export$motionRotationRateZ - 0
plot(resid)
sum(resid)
sum(normal$motionRotationRateZ)
# dense around x = 0, the three look pretty similar

plot(normal$loggingSample, normal$motionUserAccelerationX)
lines(normal$loggingSample, normal$motionUserAccelerationX)
plot(export$loggingSample, export$motionUserAccelerationX)
lines(export$loggingSample, export$motionUserAccelerationX)
summary(lm(normal$loggingSample~normal$motionUserAccelerationX))
summary(lm(export$loggingSample~export$motionUserAccelerationX))

plot(normal$loggingSample, normal$motionUserAccelerationY)
lines(normal$loggingSample, normal$motionUserAccelerationY)
plot(export$loggingSample, export$motionUserAccelerationY)
lines(export$loggingSample, export$motionUserAccelerationY)
summary(lm(normal$loggingSample~normal$motionUserAccelerationY))
summary(lm(export$loggingSample~export$motionUserAccelerationY))

plot(normal$loggingSample, normal$motionUserAccelerationZ)
lines(normal$loggingSample, normal$motionUserAccelerationZ)
plot(export$loggingSample, export$motionUserAccelerationZ)
lines(export$loggingSample, export$motionUserAccelerationZ)
summary(lm(normal$loggingSample~normal$motionUserAccelerationZ))
summary(lm(export$loggingSample~export$motionUserAccelerationZ))
# same behavior, like the rotation rates

plot(normal$loggingSample, normal$motionQuaternionX)
lines(normal$loggingSample, normal$motionQuaternionX)
plot(export$loggingSample, export$motionQuaternionX)
lines(export$loggingSample, export$motionQuaternionX)
summary(lm(normal$loggingSample~normal$motionQuaternionX))
summary(lm(export$loggingSample~export$motionQuaternionX))

plot(normal$loggingSample, normal$motionQuaternionY)
lines(normal$loggingSample, normal$motionQuaternionY)
plot(export$loggingSample, export$motionQuaternionY)
lines(export$loggingSample, export$motionQuaternionY)
summary(lm(normal$loggingSample~normal$motionQuaternionY))
summary(lm(export$loggingSample~export$motionQuaternionY))

plot(normal$loggingSample, normal$motionQuaternionZ)
lines(normal$loggingSample, normal$motionQuaternionZ)
plot(export$loggingSample, export$motionQuaternionZ)
lines(export$loggingSample, export$motionQuaternionZ)
summary(lm(normal$loggingSample~normal$motionQuaternionZ))
summary(lm(export$loggingSample~export$motionQuaternionZ))

plot(normal$loggingSample, normal$motionQuaternionW)
lines(normal$loggingSample, normal$motionQuaternionW)
plot(export$loggingSample, export$motionQuaternionW)
lines(export$loggingSample, export$motionQuaternionW)
summary(lm(normal$loggingSample~normal$motionQuaternionW))
summary(lm(export$loggingSample~export$motionQuaternionW))
# the X one is all over the place, but bottom dense,
# increased very fast, but dropped very fast in the end
#a W shaped thing
# same as Z
i=100
q= i+250
plot(normal$loggingSample, normal$motionGravityX)
lines(normal$loggingSample, normal$motionGravityX)
plot(export$loggingSample, export$motionGravityX)
lines(export$loggingSample, export$motionGravityX)
normal_mg = lm(normal$motionGravityX[200:1000]~normal$loggingSample[200:1000])
export_mg = lm(export$motionGravityX[200:1000]~export$loggingSample[200:1000])
mean(normal$motionGravityX[i:q])
mean(export$motionGravityX[i:q])
norm_out = (abs(normal$motionGravityX[i:q])-mean(normal$motionGravityX[i:q])-5*var(normal$motionGravityX[i:q]))
norm_out = norm_out[which(norm_out>0)]
sum(norm_out^2)
expo_out = (abs(export$motionGravityX[i:q])-mean(export$motionGravityX[i:q])-5*var(export$motionGravityX[i:q]))
expo_out = expo_out[which(expo_out>0)]
sum(expo_out^2)
var(normal$motionGravityX[i:q])
var(export$motionGravityX[i:q])
summary(normal_mg)
summary(export_mg)
anova(normal_mg)
anova(export_mg)


plot(normal$loggingSample, normal$motionGravityY)
lines(normal$loggingSample, normal$motionGravityY)
plot(export$loggingSample, export$motionGravityY)
lines(export$loggingSample, export$motionGravityY)
normal_mg = lm(normal$motionGravityY[200:1000]~normal$loggingSample[200:1000])
export_mg = lm(export$motionGravityY[200:1000]~export$loggingSample[200:1000])
mean(normal$motionGravityY[i:q])
mean(export$motionGravityY[i:q])
norm_out = (abs(normal$motionGravityY[i:q])-mean(normal$motionGravityY[i:q])-5*var(normal$motionGravityY[i:q]))
norm_out = norm_out[which(norm_out>0)]
sum(norm_out^2)
expo_out = (abs(export$motionGravityY[i:q])-mean(export$motionGravityY[i:q])-5*var(export$motionGravityY[i:q]))
expo_out = expo_out[which(expo_out>0)]
sum(expo_out^2)
var(normal$motionGravityY[i:q])
var(export$motionGravityY[i:q])
summary(lm(normal$loggingSample~normal$motionGravityY))
summary(lm(export$loggingSample~export$motionGravityY))





plot(normal$loggingSample, normal$motionRoll)
lines(normal$loggingSample, normal$motionRoll)
plot(export$loggingSample, export$motionRoll)
lines(export$loggingSample, export$motionRoll)
mean(normal$motionRoll[i:q])
mean(export$motionRoll[i:q])
norm_out = (abs(normal$motionRoll[i:q])-mean(normal$motionRoll[i:q])-5*var(normal$motionRoll[i:q]))
norm_out = norm_out[which(norm_out>0)]
sum(norm_out^2)
expo_out = (abs(export$motionRoll[i:q])-mean(export$motionRoll[i:q])-5*var(export$motionRoll[i:q]))
expo_out = expo_out[which(expo_out>0)]
sum(expo_out^2)
var(normal$motionRoll[i:q])
var(export$motionRoll[i:q])
summary(lm(normal$loggingSample~normal$motionRoll))
summary(lm(export$loggingSample~export$motionRoll))
# looks like an M, dense on the top





plot(normal$loggingSample, normal$motionGravityZ)
lines(normal$loggingSample, normal$motionGravityZ)
plot(export$loggingSample, export$motionGravityZ)
lines(export$loggingSample, export$motionGravityZ)
mean(normal$motionGravityZ[i:q])
mean(export$motionGravityZ[i:q])
norm_out = (abs(normal$motionGravityZ[i:q])-mean(normal$motionGravityZ[i:q])-5*var(normal$motionGravityZ[i:q]))
norm_out = norm_out[which(norm_out>0)]
sum(norm_out^2)
expo_out = (abs(export$motionGravityZ[i:q])-mean(export$motionGravityZ[i:q])-5*var(export$motionGravityZ[i:q]))
expo_out = expo_out[which(expo_out>0)]
sum(expo_out^2)
var(normal$motionGravityZ[i:q])
var(export$motionGravityZ[i:q])
summary(lm(normal$loggingSample~normal$motionGravityZ))
summary(lm(export$loggingSample~export$motionGravityZ))
# X and Y behave alike, but Z has an increasing trend

