# read training and testing datasets
traindata <-  read.csv('C:/Users/Alexandros/Dropbox/MSc/2nd Semester/Data analysis/Assignment/transfusion.csv')
testdata <-  read.csv('C:/Users/Alexandros/Dropbox/MSc/2nd Semester/Data analysis/Assignment/test.csv')
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
for(i in 1:nrow(matrix_train)) {
  if ((matrix_train[i,8] >= 4) & (matrix_train[i,5] == 1)) {
    count_train_predicted_donations = count_train_predicted_donations + 1
  }
}
#------------------------------------------------------------
count_test_predicted_donations <- 0
for(i in 1:nrow(matrix_test)) {
  if ((matrix_test[i,8] >= 4) & (matrix_test[i,5] == 1)) {
    count_test_predicted_donations = count_test_predicted_donations + 1
  }
}
#-------------------------------------------------------------
count_train_all_donations <- 0
for(i in 1:nrow(matrix_train)) {
  if (matrix_train[i,5] == 1) {
    count_train_all_donations = count_train_all_donations + 1
  }
}
#----------------------------------------------------------------
count_test_all_donations <- 0
for(i in 1:nrow(matrix_test)) {
  if (matrix_test[i,5] == 1) {
    count_test_all_donations = count_test_all_donations + 1
  }
}
#-----------------------------------------------------
train_accuracy <- (count_train_predicted_donations/count_train_all_donations)
test_accuracy <- (count_test_predicted_donations / count_test_all_donations )
err = abs(test_accuracy-train_accuracy)
cat("Accuracy with training dataset: ", train_accuracy)
cat("Accuracy with testing dataset: ", test_accuracy)
cat("Error: ", err)