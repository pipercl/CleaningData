#Cleaning Data Course Project
setwd("~/Coursera/Cleaning Data/Course Project/UCI HAR Dataset")
getwd()

library(data.table)
library(plyr)

#Load the test data
subjtest <- read.table("./test/subject_test.txt", header=FALSE) ; #30% or 9 test subjects
xtest <- read.table("./test/X_test.txt", header=FALSE)
ytest <- read.table("./test/y_test.txt", header=FALSE)
bodyacctest.x <- read.table("./test/inertial Signals/body_acc_x_test.txt", header=FALSE)

unique(subjtest)
unique(ytest)  ; #activity category

#Load the training data
subjtrain <- read.table("./train/subject_train.txt", header=FALSE) ; #21 test subjects
xtrain <- read.table("./train/X_train.txt", header=FALSE)
ytrain <- read.table("./train/y_train.txt", header=FALSE)

unique(subjtrain)
unique(ytrain) 

merge_subj = rbind(subjtest,subjtrain)
merge_y = rbind(ytest,ytrain)

#Get mean and stdev of BodyAcc test
tBodyAcc_ms_test <- xtest[,1:6]
tBodyAcc_ms_train <- xtrain[,1:6]
merge_tBodyAcc_ms <- rbind(tBodyAcc_ms_test,tBodyAcc_ms_train)

#Give col names descriptive names
colnames(ytest)

colnames(merge_y)[1] <- "Test_Type"; 
colnames(merge_subj)[1] <- "Subj_No"; 
colnames(merge_tBodyAcc_ms) <- c("Mean_X","Mean_Y", "Mean_Z", "Stdv_X", "Stdv_Y", "Stdv_Z")
colnames(merge_tBodyAcc_ms)

# Assign descriptive names to activities
merge_y[,2]=""
colnames(merge_y)[2]="Test_Name"
merge_y[merge_y$Test_Type == 1,2] <- "walking"
merge_y[merge_y$Test_Type == 2,2] <- "walking upstairs"
merge_y[merge_y$Test_Type == 3,2] <- "walking downstairs"
merge_y[merge_y$Test_Type == 4,2] <- "sitting"
merge_y[merge_y$Test_Type == 5,2] <- "standing"
merge_y[merge_y$Test_Type == 6,2] <- "laying"

#Create a dataframe with Subj Nos, Test Type/Name, and Body Acc Test Results
#Sort Results by Subj_No and Test_Type
BAT <- cbind(merge_subj,merge_y,merge_tBodyAcc_ms)
BAT <- arrange(BAT, Subj_No, Test_Type)

#Summary Stats
#create a subset of first subj & first test type
BAT11 <- BAT[(BAT$Subj_No==1 & BAT$Test_Type==1),]
sapply(BAT11[,4:9],mean)

#create a subset of first subj and all tests
BAT1all <- BAT[(BAT$Subj_No==1),]
sapply(BAT1all[BAT1all$Test_Type == 1,4:9],mean)

#do summary stats for all tests
df=NULL
for (k in 1:6)  {
  df <- rbind(df, sapply(BAT1all[BAT1all$Test_Type == k,4:9],mean));
  }
df

#apply to all subjs
df=NULL
sn=NULL
tt=NULL
for (j in 1:30) {
       for (k in 1:6)  {
            df <- rbind(df, sapply(BAT[(BAT$Subj_No ==j & BAT$Test_Type == k),4:9],mean));
            sn <- rbind(sn,j);
            tt <- rbind(tt,k);
                       }
                }
df <- cbind(sn,tt,df)
colnames(df)[1]="Subj_No"
colnames(df)[2]="Test_Type"
colnames(df)[3:8]= c("Mean of Mean_X", "Mean of Mean_Y", "Mean of Mean_Z", "Mean of Stdv_X", "Mean of Stdv_Y", "Mean of Stdv_Z")

df
#write out results to a txt file
write.table(df, "BATms.txt", row.name=FALSE)
