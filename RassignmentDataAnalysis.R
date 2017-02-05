library(car)
# read whole dataset 748 instances
train <-  read.csv('C:/Users/Alexandros/Dropbox/MSc/2nd Semester/Data analysis/Assignment/transfusiondata.csv')
#-------------------------------------------------------------------
unique(sort(train$Recency..months.))
index <- (train$Recency..months. == train$Time..months.) & train$Frequency..times. > 1
dim(train[index,])
#--------------------------------------------------------------------
scatterplot(train$Recency..months. ~ train$whether.he.she.donated.blood.in.March.2007, data=train, 
            xlab="Recency", ylab="Donation", 
            main="Scatter Plot")
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
counter_train <- 0
for(i in 1:nrow(matrix_train)) {
  if ((matrix_train[i,8] >= 4) & (matrix_train[i,5] == 1)) {
    count_train_predicted_donations = count_train_predicted_donations + 1}
  if (matrix_train[i,8] >= 4) {
    counter_train <- counter_train + 1
  }
}
#------------------------------------------------------------
count_test_predicted_donations <- 0
counter_test<-0
for(i in 1:nrow(matrix_test)) {
  if ((matrix_test[i,8] >= 4) & (matrix_test[i,5] == 1)) {
    count_test_predicted_donations = count_test_predicted_donations + 1 }
  if (matrix_test[i,8] >= 4) {
    counter_test <- counter_test + 1
  }
}
#-------------------------------------------------------------
count_train_all_donations <- 0
for(i in 1:nrow(matrix_train)) {
  if (matrix_train[i,5] == 1) {
    count_train_all_donations = count_train_all_donations + 1  }
}
#----------------------------------------------------------------
count_test_all_donations <- 0
for(i in 1:nrow(matrix_test)) {
  if (matrix_test[i,5] == 1) {
    count_test_all_donations = count_test_all_donations + 1  }
}
#-----------------------------------------------------
train_donations_accuracy <- (count_train_predicted_donations/count_train_all_donations)
test_donations_accuracy <- (count_test_predicted_donations / count_test_all_donations )
donation_err = abs(test_donations_accuracy - train_donations_accuracy)
cat("Accuracy for predicting donations with training dataset: ", train_donations_accuracy)
cat("Accuracy for predicting donations with testing dataset: ", test_donations_accuracy)
cat("Predicting Donations Error: ", donation_err)
#-----------------------------------------------------
dftrain <- data.frame(matrix_train)
dftrain_final <- dftrain[c(1:counter_train),1:8]
#----------------------------------------------------
dftest <- data.frame(matrix_test)
dftest_final <- dftest[c(1:counter_test),1:8]
#------------------------------------------------------
write.csv(dftrain_final, file = "C:\\Users\\Alexandros\\Dropbox\\MSc\\2nd Semester\\Data analysis\\Assignment\\train_output.csv", row.names = FALSE)
write.csv(dftest_final, file = "C:\\Users\\Alexandros\\Dropbox\\MSc\\2nd Semester\\Data analysis\\Assignment\\test_output.csv", row.names = FALSE)
#--------------------------------------------------------