
## run_analysis.R  this the script which used to do the following.
## 1. Getting raw data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
## 2.  Merges the training and the test sets to create one data set.
## 3.  Extracts only the measurements on the mean and standard deviation for each measurement. 
## 4.  Uses descriptive activity names to name the activities in the data set
## 5.  Appropriately labels the data set with descriptive variable names. 
## 6.  From the data set in step 5, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# requirs two packages, data.table and reshape 2
library(data.table)
library(reshape2)


## runAnalysis() function will analysis the raw which in the given raw file url.
runAnalysis <- function(rawFileUrl){
  
  # isDownloadedData() function Check the whether the raw dataset is downloaded or not. If yes, then will call the mergeData()
  # function to get a tidy data set and apply the mean function to the data set.
  if(isDownloadedData(rawFileUrl)){
    tidyData <- mergeData("./data/UCI HAR Dataset/")
    
    
    
    # Apply mean function to tidy dataset via dcast function
    tidyData <- dcast(tidyData , subject + Activity_Label ~ variable, mean)
    
    # write the tidy data to the current directy
    write.table(tidyData, file = "./data/tidyData.txt",row.names=FALSE)
  }
  
}


## isDownloadedData()  function is for download the raw data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## to the ./data directory. return a logical variable which indicate the files was downloaded or not

isDownloadedData <- function(fileUrl){
   rawFileUrl <- fileUrl
  if(!file.exists("./data")){dir.create("./data")}
 
   download.file(rawFileUrl,destfile = "./data/Dataset.zip",method="curl")
   
   # Unzip the download data
   unzip(zipfile="./data/Dataset.zip",exdir="./data")
  
   if(file.exists("./data/Dataset.zip")){
     return(TRUE)
   }else{
     return(FALSE)
     stop("The file can not be download, please try again !")
   }
   
 
}

## mergeData() function will read data from the unzip the files ,
## then Merges the training and the test sets to create one data set
## parameter dataFilesPath： specify the path of the the training and the test sets
## the mergeData() function will return a tidy data set
mergeData <- function(dataFilesPath){
  
 
  # read the  X_test , y_test  and subject_test data.
  xTestData <- read.table(paste(dataFilesPath,"test/X_test.txt",sep = ""))
  yTestData <- read.table(paste(dataFilesPath,"test/y_test.txt",sep = ""))
  subjectTestData <- read.table(paste(dataFilesPath,"test/subject_test.txt",sep = ""))
  
  # read features names
  features <- read.table(paste(dataFilesPath,"features.txt",sep = ""))[,2]
  
  # read activity labels
  activityLabels <- read.table(paste(dataFilesPath,"activity_labels.txt",sep = ""))[,2]
  
  #Extracts only the measurements on the mean and standard deviation for each measurement. 
  extractMeandSD <- grepl("mean",features) | grepl("std",features)
  names(xTestData) <- features
  xTestData <- xTestData[,extractMeandSD]
  
  
  # Uses descriptive activity names to name the activities in the data set
  yTestData[,2] <- activityLabels[yTestData[,1]]
  names(yTestData) <- c("Activity_ID", "Activity_Label")
  names(subjectTestData) <- "subject"
  
  # Bind the new test data set
  testData <- cbind(as.data.table(subjectTestData), yTestData, xTestData)
  
  
  # read the  X_train , y_train and subject_train data.
  xTrainData <- read.table(paste(dataFilesPath,"train/X_train.txt",sep = ""))
  yTrainData <- read.table(paste(dataFilesPath,"train/y_train.txt",sep = ""))
  subjectTrainData <- read.table(paste(dataFilesPath,"train/subject_train.txt",sep = ""))

  #Extracts only the measurements on the mean and standard deviation for each measurement. 
 
  names(xTrainData) <- features
  xTrainData <- xTrainData[,extractMeandSD]
  
  # Uses descriptive activity names to name the activities in the data set
  yTrainData[,2] <- activityLabels[yTrainData[,1]]
  names(yTrainData) <- c("Activity_ID", "Activity_Label")
  names(subjectTrainData) <- "subject"
  
  # Bind the new train data set
  trainData <- cbind(as.data.table(subjectTrainData), yTrainData, xTrainData)
  
  # Merge the new train data and new test data to new tidy date set
  testAndTrain <- rbind(testData,trainData)
  
  
  # Appropriately labels the data set with descriptive variable names
  # and return a melted data set ,
  IDlabels <- c("subject", "Activity_ID", "Activity_Label")
  varLabels <- setdiff(colnames(testAndTrain), IDlabels)
  meltedData <- melt(testAndTrain, id = IDlabels, measure.vars = varLabels)
  
  return(meltedData)
  
}
