##Assume files have been downloaded and upzipped to working directory

##PART 1
##Get files from UCI HAR Dataset folder
xtest<-read.table("./UCI HAR Dataset/test/X_test.txt") ##data
ytest<-read.table("./UCI HAR Dataset/test/y_test.txt") ##activity labels
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt") #subject IDs
##combine into one test data set
test_data<-cbind(subject_test, ytest, xtest)

##repeat the process for the training data set
xtrain<-read.table("./UCI HAR Dataset/train/X_train.txt") ##data
ytrain<-read.table("./UCI HAR Dataset/train/y_train.txt") ##activity labels
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt") #subject IDs
train_data<-cbind(subject_train, ytrain, xtrain)

##Then rbind the test and training into one large data set
data<-rbind(test_data, train_data)

##PART 2
##Extracts only the measurements on the mean and standard deviation for each measurement. 
##Read in features.txt for variable names
var_names<-read.table("./UCI HAR Dataset/features.txt")
##Keep only variables related to mean and standard deviation
keep<-grep(".*mean.*|.*std.*", var_names[,2])
##Subset out variable names with only those we keep
var_names<-var_names[keep,]
##Add 2 to value of keep because ID is in column 1 and activity is in column 2
keep<-keep+2
##Add 1 and 2 to list of columns to keep
keep<-c(1,2,keep)
##Subset data to only keep ID, activity, and data columns with mean and sd information
data<-data[,keep]

##PART 3, name activities
activity_names<-read.table("./UCI HAR Dataset/activity_labels.txt")
numActivity = 1
for (numActivityName in activity_names[,2]){
        data[,2]<-gsub(numActivity, numActivityName, data[,2])
        numActivity <- numActivity + 1
        }

##Part 4, label other variable names
colnames(data)<-c("subject", "activity", as.character(var_names[,2]))
##clean up labels and replace abbreviations with full descriptions
labels<-names(data)
labels<-gsub("[()]", "",labels)
labels<-gsub("BodyBody","Body",labels)
labels<-gsub("^(t)", "time", labels)
labels<-gsub("^(f)", "frequency", labels)
labels<-gsub("Acc","Accelerometer",labels)
labels<-gsub("Gyro","Gyroscope",labels)
labels<-gsub("Mag","Magnitude",labels)
labels<-gsub("Freq","Frequency",labels)
colnames(data)<-labels

##Part 5, create independent tidy data set with the average of 
##each variable for each activity and each subject
averages<-aggregate(data, by=list(data$activity, data$subject), mean)
averages<-cbind(averages[,1:2], averages[,5:83])
colnames(averages)<-c("activity", "subject", labels[3:81])

##Save as .txt file in working directory
write.table(averages, "./tidy_averages.txt", row.names=FALSE)
