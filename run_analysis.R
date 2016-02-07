library(dplyr)

setwd("/Users/bradychiu/Dropbox (Uber Technologies)/R/Coursera/03_GettingAndCleaningData/Week4/Assignment4")

# activity labels
activity_labels <- fread("data/UCI HAR Dataset/activity_labels.txt",col.names=c("id","name"))

# feature labels
feature_labels <- fread("data/UCI HAR Dataset/features.txt",col.names=c("id","name"))

train_x <- fread("data/UCI HAR Dataset/train/X_train.txt",col.names=make.names(feature_labels$name,unique=T))
train_y <- fread("data/UCI HAR Dataset/train/y_train.txt",col.names="activity_id")
train_subject <- fread("data/UCI HAR Dataset/train/subject_train.txt",col.names="subject_id")

test_x <- fread("data/UCI HAR Dataset/test/X_test.txt",col.names=make.names(feature_labels$name,unique=T))
test_y <- fread("data/UCI HAR Dataset/test/y_test.txt",col.names="activity_id")
test_subject <- fread("data/UCI HAR Dataset/test/subject_test.txt",col.names="subject_id")

# 1. Merges the training and the test sets to create one data set.
x_data <- rbind(train_x,test_x)
y_data <- rbind(train_y,test_y)
subject_data <- rbind(train_subject,test_subject)
master_data <- cbind(subject_data,y_data,x_data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std_data <- master_data %>%
  select(subject_id,activity_id,grep('mean|std',names(master_data),ignore.case=T))

# 3. Uses descriptive activity names to name the activities in the data set
mean_std_data <- merge(mean_std_data,activity_labels,by.x="activity_id",by.y="id") %>%
  rename(activity_name = name) %>%
  select(2,activity_name,3:ncol(.))

# 4 Appropriately labels the data set with descriptive variable names.
names <- names(mean_std_data)

names <- gsub("subject_id","SubjectID",names,ignore.case=T)
names <- gsub("activity_name","ActivityName",names,ignore.case=T)
names <- gsub("bodybody","Body",names,ignore.case=T)
names <- gsub("tbody","Body",names,ignore.case=T)
names <- gsub("fbody", "FFTBody",names,ignore.case=T)
names <- gsub("gravity","Gravity",names,ignore.case=T)
names <- gsub("tgravity","Gravity",names,ignore.case=T)
names <- gsub("fgravity", "FFTGravity",names,ignore.case=T)
names <- gsub("acc", "Accelerometer",names,ignore.case=T)
names <- gsub("gyro", "Gyroscope",names,ignore.case=T)
names <- gsub("mag", "Magnitude",names,ignore.case=T)
names <- gsub("mean", "Mean",names,ignore.case=T)
names <- gsub("std", "StandardDeviation",names,ignore.case=T)
names <- gsub("freq", "Frequency",names,ignore.case=T)
names <- gsub("\\.X", "XAxis",names,ignore.case=T)
names <- gsub("\\.Y", "YAxis",names,ignore.case=T)
names <- gsub("\\.Z", "ZAxis",names,ignore.case=T)
names <- gsub("angle","Angle",names,ignore.case=T)
names <- gsub("\\.","",names,ignore.case=T)

colnames(mean_std_data) <- names

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

final_data <- aggregate(.~SubjectID + ActivityName,mean_std_data,mean) %>%
  arrange(SubjectID,ActivityName)

write.csv(final_data,"tidy.csv",row.names=F)