##You should create one R script called run_analysis.R that does the following. 
##1. Merges the training and the test sets to create one data set.

##First we will complete dataset by merging X_train (analyzed data as described
##in key),y_train(activities performed), subject_train(subject names), and do
##equivalent operations on the test data. To do this we upload respective tables,
##as a data.table, add column names from the above files to merge, then 
##when each table has been completed we will combine them vertically, as all
##columns should have same values, making one large dataset

##BEFORE STARTING SET WD as the UCI HAR Dataset folder##
##Also load libraries, install data.table package if you have not
library(data.table)

#Reading
x_test <- read.table("test/X_test.txt", header=FALSE, sep="")
y_test <- read.table("test/y_test.txt", header=FALSE, sep="")
subject_test <- read.table("test/subject_test.txt", header=FALSE, sep="")
x_train <- read.table("train/X_train.txt", header=FALSE, sep="")
y_train <- read.table("train/y_train.txt", header=FALSE, sep="")
subject_train <- read.table("train/subject_train.txt", header=FALSE, sep="")

#Will convert all to data.table class
setDT(x_test)
setDT(y_test)
setDT(subject_test)
setDT(x_train)
setDT(y_train)
setDT(subject_train)

#Assigning colnames based on features.txt file, extract col 2 as character
#vector, then setnames function to change all names of columns
colnamesfile <- read.table("features.txt", header=FALSE, sep="", stringsAsFactors=F)
setDT(colnamesfile)
colnames <- make.names(colnamesfile[,V2])
##Make sure returning a string
setnames(x_test, names(x_test), colnames)
setnames(x_train, names(x_train), colnames)

#Combining x, y, subject as above
x_test[,c("activity", "subjectid") := c(y_test, subject_test)]
x_train[,c("activity", "subjectid") := c(y_train, subject_train)]

#Now processed tables, merge them vertically
merged_test_train <- rbind(x_test,x_train)

##2. Extracts only the measurements on the mean and standard deviation for each 
##measurement.

##To do this we will just subset based on columns with mean and std deviation
##in the name, also activity and subjectid since we still need those
colmeanstdnumbers <- grep("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]", names(merged_test_train))
colmeanstd <- names(merged_test_train)[colmeanstdnumbers]
colmeanstd <- append("subjectid", "activity", colmeanstd)
meanstd_merged <- merged_test_train[,colmeanstd, with=FALSE]

##3. Uses descriptive activity names to name the activities in the data set

##To do this we will just use a function to replace numbers with strings
##for each of the six activities
meanstd_merged[,activity := as.character(activity)]
#set(meanstd_merged, i=which(meanstd_merged[["activity"]]=="1"), j=meanstd_merged[["activity"]], "walking")
meanstd_merged[activity == "6", activity := "laying"]
meanstd_merged[activity == "5", activity := "standing"]
meanstd_merged[activity == "4", activity := "sitting"]
meanstd_merged[activity == "3", activity := "walking_downstairs"]
meanstd_merged[activity == "2", activity := "walking_upstairs"]
meanstd_merged[activity == "1", activity := "walking"]

##4. Appropriately labels the data set with descriptive variable names.

##We actually did this above already, the variables tested are complicated,
##but this should be good enough for someone who understands the data collected

##5. From the data set in step 4, creates a second, independent tidy data set
##with the average of each variable for each activity and each subject.

##Here we will just subset again using the "by" function in data.table
finalmeanstdtable <- meanstd_merged[,lapply(.SD,mean),by=.(activity,subjectid)]