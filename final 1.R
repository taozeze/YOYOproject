#@by XiaoYue


setwd("~/Nino Zhang/study at TC/fall 2016/EDM/final project")
small <- read.csv("2016-12-15_12-00-32.csv", header = TRUE, stringsAsFactors = FALSE)
small$ID <- "0"
class(small)
colnames(small)




# logging sample is a sequence, 1:n

plot(x <- small$loggingSample, y <- small$locationTimestamp_since1970)
# location time stamp is increasing linearly

plot(x <- small$loggingSample, y <- small$locationLatitude)
hist(small$loggingSample, small$locationLatitude)
# can't really see a pattern on location latitude, wut is dis? do we need dis?

plot(small$loggingSample, y <- small$locationLongitude)
# longitude increased very fast at first, dropped a bit, increased again, then dropped fast

IDnum <- small$loggingSample

plot(IDnum, small$locationAltitude)
# it decreased, kinda in a linear manner, gun try a reg on it
reg_altitude <- lm(small$locationAltitude~IDnum)
abline(reg_altitude, col = "red") 
summary(reg_altitude)
# R^2 is only 0.6, not so good, prolly its not linear

plot(IDnum, small$locationSpeed)
#a total mess, cant see any pattern, maybe we should pay more attention on this 

plot(IDnum, small$locationCourse)
# same as above, but what does location course even mean?

plot(IDnum, small$locationVerticalAccuracy)
#interesting discrete step funtion, why they are behaving like dis?

plot(IDnum, small$locationHorizontalAccuracy)
# something like the vertical accuracy, do we need these two in the analysis?




plot(IDnum, small$accelerometerTimestamp_sinceReboot)
#a straight line
plot(IDnum, small$motionUserAccelerationX)
# some pretty messed up distribution of seperate dots, prolly need to take a look on this !!!!!!!!!!!!!!!!
plot(IDnum, small$motionUserAccelerationY)
# same as above
plot(IDnum, small$motionUserAccelerationZ)
# the same



plot(IDnum, small$gyroTimestamp_sinceReboot)
# a straight line 
plot(IDnum, small$gyroRotationX)
plot(IDnum, small$gyroRotationY)
plot(IDnum, small$gyroRotationZ)
# basically the same as the acceleration data


plot(IDnum, small$motionTimestamp_sinceReboot)
# a line
plot(IDnum, small$motionYaw)
# a weird W shape distribution, what is this?????

plot(IDnum, small$motionRoll)
# looks like an M, dense on the top

plot(IDnum, small$motionPitch)
# like an upside down version of the previous one, dense on the bottom

plot(IDnum, small$motionRotationRateX)
plot(IDnum, small$motionRotationRateY)
plot(IDnum, small$motionRotationRateZ)
# dense around x = 0, the three look pretty similar

plot(IDnum, small$motionUserAccelerationX)
plot(IDnum, small$motionUserAccelerationY)
plot(IDnum, small$motionUserAccelerationZ)
# same behavior, like the rotation rates

class(small$motionAttitudeReferenceFrame)
length(unique(small$motionAttitudeReferenceFrame))
# the output is 1, so there is only one character in this column

plot(IDnum, small$motionQuaternionX)
plot(IDnum, small$motionQuaternionY)
plot(IDnum, small$motionQuaternionZ)
plot(IDnum, small$motionQuaternionW)
# the X one is all over the place, but bottom dense,
# increased very fast, but dropped very fast in the end
#a W shaped thing
# same as Z

plot(IDnum, small$motionGravityX)
plot(IDnum, small$motionGravityY)
plot(IDnum, small$motionGravityZ)
# X and Y behave alike, but Z has an increasing trend

plot(IDnum, small$motionMagneticFieldX)
plot(IDnum, small$motionMagneticFieldY)
plot(IDnum, small$motionMagneticFieldZ)
# didnt think info on magnet field has anything to do with the analysis, but plotted them out anyway
length(unique(small$motionMagneticFieldCalibrationAccuracy))
# = 1

plot(IDnum, small$activityTimestamp_sinceReboot)
# an increasing step function

length(unique(small$activityActivityConfidence))
# = 2
unique(small$activityActivityConfidence)
# = 2 and 1

summary(small$activityActivityStartDate)
unique(small$activityActivityStartDate)
# I dont know why there are 5 start dates in a single file??????? Shouldn't be only one tho?

# didn't do the pedometer data, since I don't think thats useful

plot(small$altimeterReset)
# all 0

plot(IDnum, small$altimeterRelativeAltitude)
# a N shape graph, looks familiar

plot(small$altimeterPressure)
# inverse of the previous one, weird

# IP addresses are ommitted 

unique(small$deviceOrientation)
# = 1

# the rest are ommited as well, since little useful info contained
