library(rvest)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(RColorBrewer)
library(rpart.plot)

normal <- read.csv("2016-12-15_12-00-36.csv", header = TRUE, stringsAsFactors = FALSE)
export <- read.csv("2016-12-15_12-00-32.csv", header = TRUE, stringsAsFactors = FALSE)

par(mfrow=c(3,2))

plot(normal$loggingSample, normal$motionGravityX)
lines(normal$loggingSample, normal$motionGravityX)
plot(export$loggingSample, export$motionGravityX)
lines(export$loggingSample, export$motionGravityX)

plot(normal$loggingSample, normal$motionGravityY)
lines(normal$loggingSample, normal$motionGravityY)
plot(export$loggingSample, export$motionGravityY)
lines(export$loggingSample, export$motionGravityY)

plot(normal$loggingSample, normal$motionRoll)
lines(normal$loggingSample, normal$motionRoll)
plot(export$loggingSample, export$motionRoll)
lines(export$loggingSample, export$motionRoll)

#From the plots above, we can notice that export's motions is more cycle and periodic. 
#However, normal person would have lots of data stay together in a close position with lots of large outliers.
#So, I would use mean, sum of outliers away from 1 standard deviation, variance as three predictors for the data.

############################Start Generate Training table#############################################
num_of_sd = 1 #setting the number of standard deviation
RGxy_table_export = c(NULL) #create an empty list to store export data
RGxy_table_normal= c(NULL) #create an empty list to store normal data
for(i in seq(250, 750, by=100))
{
  RGxy_row_export = c(NULL) #create an empty list to store 
  RGxy_row_normal = c(NULL) #create an empty list to store
  q = i+250       
  RGxy_row_normal = c(RGxy_row_normal, mean(normal$motionGravityX[i:q]))
  RGxy_row_export = c(RGxy_row_export, mean(export$motionGravityX[i:q]))
  norm_out = (abs(normal$motionGravityX[i:q])-mean(normal$motionGravityX[i:q])-num_of_sd*sd(normal$motionGravityX[i:q]))
  norm_out = norm_out[which(norm_out>0)]
  RGxy_row_normal = c(RGxy_row_normal, sum(norm_out^2))
  expo_out = (abs(export$motionGravityX[i:q])-mean(export$motionGravityX[i:q])-num_of_sd*sd(export$motionGravityX[i:q]))
  expo_out = expo_out[which(expo_out>0)]
  RGxy_row_export = c(RGxy_row_export, sum(expo_out^2))
  RGxy_row_normal = c(RGxy_row_normal, var(normal$motionGravityX[i:q]))
  RGxy_row_export = c(RGxy_row_export, var(export$motionGravityX[i:q]))
  
  RGxy_row_normal = c(RGxy_row_normal, mean(normal$motionGravityY[i:q]))
  RGxy_row_export = c(RGxy_row_export, mean(export$motionGravityY[i:q]))
  norm_out = (abs(normal$motionGravityY[i:q])-mean(normal$motionGravityY[i:q])-num_of_sd*sd(normal$motionGravityY[i:q]))
  norm_out = norm_out[which(norm_out>0)]
  RGxy_row_normal = c(RGxy_row_normal, sum(norm_out^2))
  expo_out = (abs(export$motionGravityY[i:q])-mean(export$motionGravityY[i:q])-num_of_sd*sd(export$motionGravityY[i:q]))
  expo_out = expo_out[which(expo_out>0)]
  RGxy_row_export = c(RGxy_row_export, sum(expo_out^2))
  RGxy_row_normal = c(RGxy_row_normal, var(normal$motionGravityY[i:q]))
  RGxy_row_export = c(RGxy_row_export, var(export$motionGravityY[i:q]))
  
  RGxy_row_normal = c(RGxy_row_normal, mean(normal$motionRoll[i:q]))
  RGxy_row_export = c(RGxy_row_export, mean(export$motionRoll[i:q]))
  norm_out = (abs(normal$motionRoll[i:q])-mean(normal$motionRoll[i:q])-num_of_sd*sd(normal$motionRoll[i:q]))
  norm_out = norm_out[which(norm_out>0)]
  RGxy_row_normal = c(RGxy_row_normal, sum(norm_out^2))
  expo_out = (abs(export$motionRoll[i:q])-mean(export$motionRoll[i:q])-num_of_sd*sd(export$motionRoll[i:q]))
  expo_out = expo_out[which(expo_out>0)]
  RGxy_row_export = c(RGxy_row_export, sum(expo_out^2))
  RGxy_row_normal = c(RGxy_row_normal, var(normal$motionRoll[i:q]))
  RGxy_row_export = c(RGxy_row_export, var(export$motionRoll[i:q]))
  RGxy_row_normal = c(RGxy_row_normal, 0)
  RGxy_row_export = c(RGxy_row_export, 1)
  RGxy_table_export = rbind(RGxy_table_export, RGxy_row_export)
  RGxy_table_normal = rbind(RGxy_table_normal, RGxy_row_normal)
}

#0 stands for normal, 1 stands for export
RGxy_training = rbind(RGxy_table_normal, RGxy_table_export)
colnames(RGxy_training) = c("mean_Gx","sum_of_outliers_Gx","var_Gx","mean_Gy","sum_of_outliers_Gy","var_Gy","mean_Roll","sum_of_outliers_Roll","var_Roll","exportornot")
rownames(RGxy_training) = c()
RGxy_training = as.data.frame(RGxy_training)
View(RGxy_training)
#######################################################################################################


############################Start Generate Testing table#############################################
num_of_sd = 1
RGxy_table_export = c(NULL)
RGxy_table_normal= c(NULL)
for(i in seq(700, 1000, by=100))
{
  RGxy_row_export = c(NULL)
  RGxy_row_normal = c(NULL)
  q = i+250
  RGxy_row_normal = c(RGxy_row_normal, mean(normal$motionGravityX[i:q]))
  RGxy_row_export = c(RGxy_row_export, mean(export$motionGravityX[i:q]))
  norm_out = (abs(normal$motionGravityX[i:q])-mean(normal$motionGravityX[i:q])-num_of_sd*sd(normal$motionGravityX[i:q]))
  norm_out = norm_out[which(norm_out>0)]
  RGxy_row_normal = c(RGxy_row_normal, sum(norm_out^2))
  expo_out = (abs(export$motionGravityX[i:q])-mean(export$motionGravityX[i:q])-num_of_sd*sd(export$motionGravityX[i:q]))
  expo_out = expo_out[which(expo_out>0)]
  RGxy_row_export = c(RGxy_row_export, sum(expo_out^2))
  RGxy_row_normal = c(RGxy_row_normal, var(normal$motionGravityX[i:q]))
  RGxy_row_export = c(RGxy_row_export, var(export$motionGravityX[i:q]))
  
  RGxy_row_normal = c(RGxy_row_normal, mean(normal$motionGravityY[i:q]))
  RGxy_row_export = c(RGxy_row_export, mean(export$motionGravityY[i:q]))
  norm_out = (abs(normal$motionGravityY[i:q])-mean(normal$motionGravityY[i:q])-num_of_sd*sd(normal$motionGravityY[i:q]))
  norm_out = norm_out[which(norm_out>0)]
  RGxy_row_normal = c(RGxy_row_normal, sum(norm_out^2))
  expo_out = (abs(export$motionGravityY[i:q])-mean(export$motionGravityY[i:q])-num_of_sd*sd(export$motionGravityY[i:q]))
  expo_out = expo_out[which(expo_out>0)]
  RGxy_row_export = c(RGxy_row_export, sum(expo_out^2))
  RGxy_row_normal = c(RGxy_row_normal, var(normal$motionGravityY[i:q]))
  RGxy_row_export = c(RGxy_row_export, var(export$motionGravityY[i:q]))
  
  RGxy_row_normal = c(RGxy_row_normal, mean(normal$motionRoll[i:q]))
  RGxy_row_export = c(RGxy_row_export, mean(export$motionRoll[i:q]))
  norm_out = (abs(normal$motionRoll[i:q])-mean(normal$motionRoll[i:q])-num_of_sd*sd(normal$motionRoll[i:q]))
  norm_out = norm_out[which(norm_out>0)]
  RGxy_row_normal = c(RGxy_row_normal, sum(norm_out^2))
  expo_out = (abs(export$motionRoll[i:q])-mean(export$motionRoll[i:q])-num_of_sd*sd(export$motionRoll[i:q]))
  expo_out = expo_out[which(expo_out>0)]
  RGxy_row_export = c(RGxy_row_export, sum(expo_out^2))
  RGxy_row_normal = c(RGxy_row_normal, var(normal$motionRoll[i:q]))
  RGxy_row_export = c(RGxy_row_export, var(export$motionRoll[i:q]))
  RGxy_row_normal = c(RGxy_row_normal, 0)
  RGxy_row_export = c(RGxy_row_export, 1)
  RGxy_table_export = rbind(RGxy_table_export, RGxy_row_export)
  RGxy_table_normal = rbind(RGxy_table_normal, RGxy_row_normal)
}

#0 stands for normal, 1 stands for export
RGxy_testing = rbind(RGxy_table_normal, RGxy_table_export)
colnames(RGxy_testing) = c("mean_Gx","sum_of_outliers_Gx","var_Gx","mean_Gy","sum_of_outliers_Gy","var_Gy","mean_Roll","sum_of_outliers_Roll","var_Roll","exportornot")
rownames(RGxy_testing) = c()
RGxy_testing = as.data.frame(RGxy_testing)
#View(RGxy_testing)
#######################################################################################################

RGxy_training$exportornot = as.factor(RGxy_training$exportornot)
fit <- rpart(exportornot ~ ., method="class", data=RGxy_training, control = rpart.control(minsplit=1, cp=0.00001))
fit
prp(fit, main="Classification Tree",
    #extra="auto", # display prob of survival and percent of obs
    fallen.leaves=TRUE, # put the leaves on the bottom of the page
    shadow.col="gray", # shadows under the leaves
    branch.lty=2, # draw branches using dotted lines
    #branch=.5, # change angle of branch lines
    faclen=0, # faclen=0 to print full factor names
    trace=1, # print the automatically calculated cex
    split.cex=1.2, # make the split text larger than the node text
    split.prefix="Is ", # put "is " before split text
    split.suffix="?", # put "?" after split text
    #col=cols, border.col=cols, # red if spam, blue if not
    split.box.col="lightgray", # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5) # round the split box corners a tad

predict_try = predict(fit, RGxy_training, type="class")
predict_try
conf.matrix = table(RGxy_training$exportornot, predict_try)
conf.matrix
accuracy_training_ori = sum(diag(conf.matrix))/sum(conf.matrix)
accuracy_training_ori

predict = predict(fit, RGxy_testing, type = "class")
predict
conf.matrix2 = table(RGxy_testing$exportornot, predict)
conf.matrix2
accuracy_testing = sum(diag(conf.matrix2))/sum(conf.matrix2)
accuracy_testing





