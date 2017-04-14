library(car)
library(ggplot2)
# read whole dataset 748 instances
train <-  read.csv('C:/Users/Alexandros/Dropbox/MSc/2nd Semester/Data analysis/transfusiondata.csv')
#-------------------------------------------------------------------
unique(sort(train$Recency..months.))
index <- (train$Recency..months. == train$Time..months.) & train$Frequency..times. > 1
dim(train[index,])
#--------------------------------------------------------------------
par(mar = rep(2, 4))
scatterplot(train$Recency..months. ~ train$whether.he.she.donated.blood.in.March.2007, data=train, 
            xlab="Recency", ylab="Donation", 
            main="Scatter Plot")
#------------------------------------------------
# read training and testing datasets
traindata <-  read.csv('C:/Users/Alexandros/Dropbox/MSc/2nd Semester/Data analysis/transfusion.csv')
testdata <-  read.csv('C:/Users/Alexandros/Dropbox/MSc/2nd Semester/Data analysis/test.csv')
#----------------------------------------------------------
lmts <- range(train, test)    #  compare (visually) both datasets
par (mfrow = c(1, 2))
boxplot(train, ylim=lmts)
boxplot(test,ylim=lmts)
#-----------------------------------------------
#  Keep only the 4 attributes to show similar distribution
train1 <- train[,c("Recency", "Frequency", "Time", "C") ,drop=FALSE]
test1 <- train[,c("Recency", "Frequency", "Time", "C") ,drop=FALSE]
lmts <- range(train1,test1)
par(mfrow = c(1, 2))
boxplot(train1,ylim=lmts)
boxplot(test1,ylim=lmts)
#------------------------------------------------
cor_df <- data.frame(train) # convert file to dataframe
cor_df$CustID <- NULL
cor_df$Time..months.<- NULL
cor_df$whether.he.she.donated.blood.in.March.2007 <- NULL
M <- cor(cor_df) # get correlations
install.packages("corrplot")
library("corrplot") #package corrplot
corrplot(M, method = "circle") #plot matrix
#------------------------------------------------
sd(train$Recency..months.)
sd(train$Frequency..times.)
par(mar = rep(2, 4))
plot(train$Recency..months.,train$whether.he.she.donated.blood.in.March.2007)
plot(train$Frequency..times.,train$whether.he.she.donated.blood.in.March.2007)
#-------------------------------------------------
dftrain <-data.frame(traindata)
dftest <- data.frame(testdata)
sapply(dftrain, typeof)
names(dftrain)[1]<-"ID"
names(dftrain)[2]<-"recency"
names(dftrain)[3]<-"frequency"
names(dftrain)[4]<-"cc"
names(dftrain)[5]<-"time"
names(dftrain)[6]<-"donated"
#--------------------------
names(dftest)[1]<-"ID"
names(dftest)[2]<-"recency"
names(dftest)[3]<-"frequency"
names(dftest)[4]<-"cc"
names(dftest)[5]<-"time"
names(dftest)[6]<-"donated"
#----------------------------------------------
dftrain$time <- NULL
dftest$time <- NULL
#------------------------------------------------
head(dftrain)
head(dftest)
#*************************************************
sorted_dftrain <- dftrain[ order( dftrain[,2] ), ]
sorted_dftrain[ , "Rrank"] <- 0
matrix_train <- as.matrix(sapply(sorted_dftrain, as.numeric))
#************************************************test
sorted_dftest <- dftest[ order( dftest[,2] ), ]
sorted_dftest[ , "Rrank"] <- 0
matrix_test <- as.matrix(sapply(sorted_dftest, as.numeric))
#**************************************************
for(i in 1:nrow(matrix_train)) {
  if (matrix_train[i,2] < 15) {
    matrix_train[i,6] <- 3
  } else if ((matrix_train[i,2] < 26) & (matrix_train[i,2] >= 15)) {
    matrix_train[i,6] <- 2
  } else {
    matrix_train[i,6] <- 1
  }
}
#----------------------------------------------------
for(i in 1:nrow(matrix_test)) {
  if (matrix_test[i,2] < 15) {
    matrix_test[i,6] <- 3
  } else if ((matrix_test[i,2] < 26) & (matrix_test[i,2] >= 15)) {
    matrix_test[i,6] <- 2
  } else {
    matrix_test[i,6] <- 1
  }
}
#-----------------------------------------------------
sorted_dftrain <- data.frame(matrix_train)
sorted_dftrain_2 <- sorted_dftrain[ order( -sorted_dftrain[,6], -sorted_dftrain[,3] ), ]
sorted_dftrain_2[ , "Frank"] <- 0
matrix_train <- as.matrix(sapply(sorted_dftrain_2, as.numeric))
#-------------------------------------------------------------
sorted_dftest <- data.frame(matrix_test)
sorted_dftest2 <- sorted_dftest[ order( -sorted_dftest[,6], -sorted_dftest[,3] ), ]
sorted_dftest2[ , "Frank"] <- 0
matrix_test <- as.matrix(sapply(sorted_dftest2, as.numeric))
#----------------------------------------------------------------
for(i in 1:nrow(matrix_train)){ 
  if (matrix_train[i,3] >= 25) {
    matrix_train[i,7] <- 3
  } else if ((matrix_train[i,3] > 15) & (matrix_train[i,3] < 25)) {
    matrix_train[i,7] <- 2
  } else {
    matrix_train[i,7] <- 1
  }
}
#---------------------------------------------------------------
for(i in 1:nrow(matrix_test)){ 
  if (matrix_test[i,3] >= 25) {
    matrix_test[i,7] <- 3
  } else if ((matrix_test[i,3] > 15) & (matrix_test[i,3] < 25)) {
    matrix_test[i,7] <- 2
  } else {
    matrix_test[i,7] <- 1
  }
}
#-----------------------------------------------------------
sorted_dftrain <- data.frame(matrix_train)
sorted_dftrain_2 <- sorted_dftrain[ order( -sorted_dftrain[,6], -sorted_dftrain[,7] ), ]
sorted_dftrain_2[ , "SumRankRAndF"] <- 0
matrix_train <- as.matrix(sapply(sorted_dftrain_2, as.numeric))
#-----------------------------------------------------------
sorted_dftest <- data.frame(matrix_test)
sorted_dftest2 <- sorted_dftest[ order( -sorted_dftest[,6], -sorted_dftest[,7] ), ]
sorted_dftest2[ , "SumRankRAndF"] <- 0
matrix_test <- as.matrix(sapply(sorted_dftest2, as.numeric))
#------------------------------------------------------------
for(i in 1:nrow(matrix_train)) { 
  matrix_train[i,8] <- matrix_train[i,6] + matrix_train[i,7]
}
#------------------------------------------------------------
for(i in 1:nrow(matrix_test)) { 
  matrix_test[i,8] <- matrix_test[i,6] + matrix_test[i,7]
}
#--------------------------------------------------------------
sorted_dftrain <- data.frame(matrix_train)
sorted_dftrain_2 <- sorted_dftrain[ order( -sorted_dftrain[,8] ), ]
matrix_train <- as.matrix(sapply(sorted_dftrain_2, as.numeric))
#-----------------------------------------------------------------
sorted_dftest <- data.frame(matrix_test)
sorted_dftest2 <- sorted_dftest[ order( -sorted_dftest[,8] ), ]
matrix_test <- as.matrix(sapply(sorted_dftest2, as.numeric))
#--------------------------------------------------------------------
i<-0
count_train_predicted_donations <- 0
counter_train <- 0
number_donation_instances_whole_train <- 0
false_positives_train_counter <- 0
for(i in 1:nrow(matrix_train)) {
  if ((matrix_train[i,8] >= 4) & (matrix_train[i,5] == 1)) {
    count_train_predicted_donations = count_train_predicted_donations + 1}
  if ((matrix_train[i,8] >= 4) & (matrix_train[i,5] == 0)) {
    false_positives_train_counter = false_positives_train_counter + 1}
  if (matrix_train[i,8] >= 4) {
    counter_train <- counter_train + 1
  }
  if (matrix_train[i,5] == 1) {
    number_donation_instances_whole_train <- number_donation_instances_whole_train + 1
  }
}
#------------------------------------------------------------
count_test_predicted_donations <- 0
counter_test<-0
number_donation_instances_whole_test <- 0
false_positives_test_counter <- 0
for(i in 1:nrow(matrix_test)) {
  if ((matrix_test[i,8] >= 4) & (matrix_test[i,5] == 1)) {
    count_test_predicted_donations = count_test_predicted_donations + 1 }
  if ((matrix_test[i,8] >= 4) & (matrix_test[i,5] == 0)) {
    false_positives_test_counter = false_positives_test_counter + 1}
  if (matrix_test[i,8] >= 4) {
    counter_test <- counter_test + 1
  }
  if (matrix_test[i,5] == 1) {
    number_donation_instances_whole_test <- number_donation_instances_whole_test + 1
  }
}
#-----------------------------------------------------
dftrain <- data.frame(matrix_train)
dftrain_final <- dftrain[c(1:counter_train),1:8]
#----------------------------------------------------
dftest <- data.frame(matrix_test)
dftest_final <- dftest[c(1:counter_test),1:8]
#------------------------------------------------------
write.csv(dftrain_final, file = "C:\\Users\\Alexandros\\Dropbox\\MSc\\2nd Semester\\Data analysis\\train_output.csv", row.names = FALSE)
write.csv(dftest_final, file = "C:\\Users\\Alexandros\\Dropbox\\MSc\\2nd Semester\\Data analysis\\test_output.csv", row.names = FALSE)
#-----------------------------------------------------------------------------------
# train precision = number of relevant instances retrieved / number of retrieved instances from collection (530)
precision_train <-  count_train_predicted_donations / counter_train 
# train recall = number of relevant instances retrieved / number of relevant instances in collection (530)
recall_train <- count_train_predicted_donations / number_donation_instances_whole_train 
# measure combines Precision&Recall is harmonic mean of Precision&Recall balanced F-score for train file 
f_balanced_score_train <- 2*(precision_train*recall_train)/(precision_train+recall_train)
# test precision
precision_test <- count_test_predicted_donations / counter_test
# test recall 
recall_test <- count_test_predicted_donations / number_donation_instances_whole_test
# the balanced F-score for test file 
f_balanced_score_test <- 2*(precision_test*recall_test)/(precision_test+recall_test)
# error in precision 
error_precision <- abs(precision_train-precision_test)
# error in recall 
error_recall <- abs(recall_train-recall_test)
# error in f-balanced scores
error_f_balanced_scores <- abs(f_balanced_score_train-f_balanced_score_test)
#-----------------------------------------------------
# Print Statistics for verification and validation
cat("Precision with training dataset: ", precision_train)
cat("Recall with training dataset: ", recall_train)
cat("Precision with testing dataset: ", precision_test)
cat("Recall with testing dataset: ", recall_test)
cat("The F-balanced scores with training dataset: ", f_balanced_score_train)
cat("The F-balanced scores with testing dataset:  ", f_balanced_score_test)
cat("Error in precision: ", error_precision)
cat("Error in recall: ", error_recall)
cat("Error in F-balanced scores: ", error_f_balanced_scores)
#---------------------------------------------------------------
# confusion matrix (true positives, false positives, false negatives, true negatives) 
# calculate true positives for train which is the variable 'count_train_predicted_donations'
# calculate false positives for train which is the variable 'false_positives_train_counter'
# calculate false negatives for train
false_negatives_for_train <- number_donation_instances_whole_train - count_train_predicted_donations
# calculate true negatives for train 
true_negatives_for_train <- (nrow(matrix_train) - number_donation_instances_whole_train) - false_positives_train_counter
collect_train <- c(false_positives_train_counter, true_negatives_for_train, count_train_predicted_donations, false_negatives_for_train) 
#--------------------------------------------
# calculate true positives for test which is the variable 'count_test_predicted_donations'
# calculate false positives for test which is the variable 'false_positives_test_counter'
# calculate false negatives for test
false_negatives_for_test <- number_donation_instances_whole_test - count_test_predicted_donations
# calculate true negatives for test
true_negatives_for_test <- (nrow(matrix_test) - number_donation_instances_whole_test) - false_positives_test_counter
collect_test <- c(false_positives_test_counter, true_negatives_for_test, count_test_predicted_donations, false_negatives_for_test)
#  print a confusion matrix for train
TrueCondition <- factor(c(0, 0, 1, 1))
PredictedCondition <- factor(c(1, 0, 1, 0))
df_conf_mat_train <- data.frame(TrueCondition,PredictedCondition,collect_train)
library(ggplot2)
ggplot(data =  df_conf_mat_train, mapping = aes(x = PredictedCondition, y = TrueCondition)) +
  geom_tile(aes(fill = collect_train), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", collect_train)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")
#  print a confusion matrix for test
df_conf_mat_test <- data.frame(TrueCondition,PredictedCondition,collect_test)
ggplot(data =  df_conf_mat_test, mapping = aes(x = PredictedCondition, y = TrueCondition)) +
  geom_tile(aes(fill = collect_test), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", collect_test)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")
# MCC = (TP * TN - FP * FN)/sqrt((TP+FP)*(TP+FN)*(FP+TN)*(TN+FN)) for train values
mcc_train <- ((count_train_predicted_donations * true_negatives_for_train) - (false_positives_train_counter * false_negatives_for_train))/sqrt((count_train_predicted_donations+false_positives_train_counter)*(count_train_predicted_donations+false_negatives_for_train)*(false_positives_train_counter+true_negatives_for_train)*(true_negatives_for_train+false_negatives_for_train))
# print MCC for train
cat("Matthews Correlation Coefficient for train: ",mcc_train)
# MCC = (TP * TN - FP * FN)/sqrt((TP+FP) (TP+FN) (FP+TN) (TN+FN)) for test values 
mcc_test <- ((count_test_predicted_donations * true_negatives_for_test) - (false_positives_test_counter * false_negatives_for_test))/sqrt((count_test_predicted_donations+false_positives_test_counter)*(count_test_predicted_donations+false_negatives_for_test)*(false_positives_test_counter+true_negatives_for_test)*(true_negatives_for_test+false_negatives_for_test))
# print MCC for test 
cat("Matthews Correlation Coefficient for test: ",mcc_test)
# print MCC err between train and err
cat("Matthews Correlation Coefficient error: ",abs(mcc_train-mcc_test))
# Total = TP + TN + FP + FN for train values 
total_train <- count_train_predicted_donations + true_negatives_for_train + false_positives_train_counter + false_negatives_for_train
# Total = TP + TN + FP + FN for test values
total_test <- count_test_predicted_donations + true_negatives_for_test + false_positives_test_counter + false_negatives_for_test
# totalAccuracy = (TP + TN) / Total - for train values 
totalAccuracyTrain <- (count_train_predicted_donations + true_negatives_for_train)/ total_train
# totalAccuracy = (TP + TN) / Total - for test values 
totalAccuracyTest <- (count_test_predicted_donations + true_negatives_for_test)/ total_test 
# randomAccuracy = 	((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP)) / (Total*Total)  for train values 
randomAccuracyTrain <- ((true_negatives_for_train+false_positives_train_counter)*(true_negatives_for_train+false_negatives_for_train)+(false_negatives_for_train+count_train_predicted_donations)*(false_positives_train_counter+count_train_predicted_donations))/(total_train*total_train)
# randomAccuracy = 	((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP)) / (Total*Total)  for test values 
randomAccuracyTest <- ((true_negatives_for_test+false_positives_test_counter)*(true_negatives_for_test+false_negatives_for_test)+(false_negatives_for_test+count_test_predicted_donations)*(false_positives_test_counter+count_test_predicted_donations))/(total_test*total_test)
# kappa = (totalAccuracy - randomAccuracy)/(1-randomAccuracy) for train
kappa_train <- (totalAccuracyTrain-randomAccuracyTrain)/(1-randomAccuracyTrain)   
# kappa = (totalAccuracy - randomAccuracy)/(1-randomAccuracy) for test
kappa_test <- (totalAccuracyTest-randomAccuracyTest)/(1-randomAccuracyTest)
# print kappa error
cat("Kappa error: ",abs(kappa_train-kappa_test))
