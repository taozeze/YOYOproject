#By @Edward

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

plot(normal$loggingSample, normal$motionRoll)
lines(normal$loggingSample, normal$motionRoll)
plot(export$loggingSample, export$motionRoll)
lines(export$loggingSample, export$motionRoll)
summary(lm(normal$loggingSample~normal$motionRoll))
summary(lm(export$loggingSample~export$motionRoll))
# looks like an M, dense on the top

plot(normal$loggingSample, normal$motionPitch)
lines(normal$loggingSample, normal$motionPitch)
plot(export$loggingSample, export$motionPitch)
lines(export$loggingSample, export$motionPitch)
summary(lm(normal$loggingSample~normal$motionPitch))
summary(lm(export$loggingSample~export$motionPitch))
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
summary(lm(normal$loggingSample~normal$motionRotationRateZ))
summary(lm(export$loggingSample~export$motionRotationRateZ))
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


plot(normal$loggingSample, normal$motionGravityX)
lines(normal$loggingSample, normal$motionGravityX)
plot(export$loggingSample, export$motionGravityX)
lines(export$loggingSample, export$motionGravityX)
summary(lm(normal$loggingSample, normal$motionGravityX))
summary(lm(export$loggingSample, export$motionGravityX))

plot(normal$loggingSample, normal$motionGravityY)
lines(normal$loggingSample, normal$motionGravityY)
plot(export$loggingSample, export$motionGravityY)
lines(export$loggingSample, export$motionGravityY)
summary(lm(normal$loggingSample~normal$motionGravityY))
summary(lm(export$loggingSample~export$motionGravityY))

plot(normal$loggingSample, normal$motionGravityZ)
lines(normal$loggingSample, normal$motionGravityZ)
plot(export$loggingSample, export$motionGravityZ)
lines(export$loggingSample, export$motionGravityZ)
summary(lm(normal$loggingSample~normal$motionGravityZ))
summary(lm(export$loggingSample~export$motionGravityZ))
# X and Y behave alike, but Z has an increasing trend

