#selected data by ed
normal <- read.csv("2016-12-15_12-00-36.csv", header = TRUE, stringsAsFactors = FALSE)
export <- read.csv("2016-12-15_12-00-32.csv", header = TRUE, stringsAsFactors = FALSE)
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

