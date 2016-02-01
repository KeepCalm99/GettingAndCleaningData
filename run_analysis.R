## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.



run_analysis <- function(){
  
    # set working directory  
    getwd()
    setwd('U:/datasciencecoursera')
  
    # call libraries for functions
    library(data.table)
    library(reshape2)
  

    # Load: activity labels
    ActivityLables <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
    
    # Load: data column names
    Features <- read.table("./UCI HAR Dataset/features.txt")[,2]
    
    # Extract: the measurements on the mean and standard deviation for each measurement.
    ExtractMeasurements <- grepl("mean|std", Features)
    
    # Load:  X_test and y_test data.
    XTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
    YTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
    SubjectText <- read.table("./UCI HAR Dataset/test/subject_test.txt")
    
    names(XTest) = Features
    
    # Extract: the measurements on the mean and standard deviation for each measurement.
    XTest = XTest[,ExtractMeasurements]
    
    # Load: activity labels.
    YTest[,2] = ActivityLables[YTest[,1]]
    names(YTest) = c("Activity_ID", "Activity_Label")
    names(SubjectText) = "Subject"
    
    # Bind: test data
    TestData <- cbind(as.data.table(SubjectText), YTest, XTest)
    
    # Load:  X_train and y_train data.
    XTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
    YTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
    
    SubjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    
    names(XTrain) = Features
    
    #Extract: the measurements on the mean and standard deviation for each measurement.
    XTrain = XTrain[,ExtractMeasurements]
    
    #Load: activity data
    YTrain[,2] = ActivityLables[YTrain[,1]]
    names(YTrain) = c("Activity_ID", "Activity_Label")
    names(SubjectTrain) = "Subject"
    
    #Bind: train data
    TrainData <- cbind(as.data.table(SubjectTrain), YTrain, XTrain)
    
    #Merge: train and test data
    CombinedData = rbind(TestData, TrainData)
    
    #Add:  labels
    labels= c("Subject", "Activity_ID", "Activity_Label")
    data_labels = setdiff(colnames(CombinedData), labels)
    melt_data      = melt(CombinedData, id = labels, measure.vars = data_labels)
    
    #Apply: average to dataset using dcast function
    tidy_data   = dcast(melt_data, Subject + Activity_Label ~ variable, mean)
    
    write.table(tidy_data, file = "./UCI HAR Dataset/TidyData.txt", row.name=FALSE)
}