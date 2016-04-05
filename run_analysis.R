# Set working directory

setwd("C:/Users/rockyama/Desktop/R/Getting and Cleaning Data/Course Project")

# 1 Merges the training and the test sets to create one data set.

#Read training data
features <- read.table("features.txt", header = FALSE)
activity <- read.table("activity_labels.txt", header = FALSE)
subject_train <- read.table("./train/subject_train.txt", header = FALSE)
X_train <- read.table("./train/X_train.txt", header = FALSE)
Y_train <- read.table("./train/Y_train.txt", header = FALSE)

#Training column names
colnames(activity)  = c("activityId","activityType");
colnames(subject_train)  = "subjectId";
colnames(X_train)        = features[,2]; 
colnames(Y_train)        = "activityId";

#Combine training data
training <- cbind(Y_train, subject_train, X_train)

#Read test data
subject_test <- read.table("./test/subject_test.txt", header = FALSE)
X_test <- read.table("./test/X_test.txt", header = FALSE)
Y_test <- read.table("./test/Y_test.txt", header = FALSE)

#Test column names
colnames(subject_test)  = "subjectId";
colnames(X_test)        = features[,2]; 
colnames(Y_test)        = "activityId";

#Combine test data
test <- cbind(Y_test, subject_test, X_test)

#Combine all data
finalData <- rbind(training, test)

#Column names
colNames = colnames(finalData)

# 2 Extracts only the measurements on the mean and standard deviation for each measurement.

logicalVector = (grepl("labels..",colNames) | 
                   grepl("subject..",colNames) | 
                   grepl("-mean..",colNames) & !
                   grepl("-meanFreq..",colNames) & 
                   !grepl("mean..-",colNames) | 
                   grepl("-std..",colNames) & 
                   !grepl("-std()..-",colNames));

finalData = finalData[logicalVector==TRUE];
# 3 Uses descriptive activity names to name the activities in the data set

#Merge final data
finalData = merge(finalData,activity,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# 4 Appropriately label the data set with descriptive variable names.


# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","Acc",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerk",colNames[i])
  colNames[i] = gsub("JerkMag","Jerk",colNames[i])
  colNames[i] = gsub("GyroMag","Gyro",colNames[i])
};

# Reassigning  column names to the finalData set
colnames(finalData) = colNames;

# 5 From the data set in step 4, create a second, tidy data set with the 
#   average of each variable for each activity and each subject.


# Create a new table without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData  = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) 
              != c("activityId","subjectId")],by = list(activityId = 
              finalDataNoActivityType$activityId,subjectId = 
              finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activity,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
