Getting and Cleaning Data Course Project
========================================


## Project Description
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 



One of the most exciting areas in all of data science right now is wearable computing.
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced
algorithms to attract new users. The data linked to from the course website represent
data collected from the accelerometers from the Samsung Galaxy S smartphone.


A full description is available at the site where the data was obtained:


http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The R script called run_analysis.R that does the following.

* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive activity names. 
* Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## The repository include 

* CodeBook.md: information about raw and tidy data set and elaboration made to
  transform them
* README.md: this file
* run_analysis.R: R script to transform raw data set in a tidy datasets

##  The run_analysis.R implemented the as following
1. It will require the two two packages, data.table and reshape 2
2. The main function is runAnalysis(),it will analysis the raw which in the given raw file url
3. The main function will  call isDownloadedData() function Check the whether the raw dataset is downloaded or not. If yes, then will call the mergeData()
 function to get a tidy data set and apply the mean function to the data set.

