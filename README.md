Getting-and-Cleaning-Data-Project
=================================

Smartphone motion tidy data set for Coursera Getting and Cleaning Data course project
==================================================================
Human Activity Recognition Using Smartphones Dataset Compiled Tidy Data Set

==================================================================

This is a tidy data summary of experimental results from a group of 30 volunteers wearing smartphone sensors while performing various activities.  

Data was processed using an R script created as a project for the Coursera Data Science Specialization course "Getting and Cleaning Data."

All raw data was derived from this publication: 
	Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012




==================================================================
R Script Description
==================================================================
This code reads in the test and train data files and merges the X files to generate a data set containing all data values.
Subject number files are also read in from the test and train files and merged to generate a complete list of subjects corresponding to each measurement.  Mean and standard deviation values are extracted from the data set and combined to form a merged data frame.  Activity numbers are then replaced by activity names and a new column is added to indicate activity names.  
Column names are extracted from the data descriptions and added to the data frame.  The columns containg mean values are extracted and saved in a data frame, and the plyr package is used to find the mean of each activity for each subject and save that value in a data frame.  The data frame is then written to a text file.  Complete code is included below.


==================================================================
R Script Code
==================================================================
TidyScript <- function() {
  X_test <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"))
  subject_test <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"))
  Y_test <- read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"))
  X_train <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"))
  y_train <- read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"))
  subject_train <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"))
  activity_labels <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"))
  features <- read.table(file.path("UCI HAR Dataset", "features.txt"))
  MergedData <- rbind(X_train, X_test)
  colnames(MergedData) <- features[ ,2]
  mergedSubjects <- rbind(subject_train, subject_test)
  
  meanNames <- grep("mean()", colnames(MergedData))
  stdNames <- grep("std()", colnames(MergedData))
  MeanAndStd <- MergedData[,c(meanNames, stdNames)]
  ActivityNames <- rbind(y_train, Y_test)
  ActivityNames <- replace(ActivityNames, ActivityNames == 1, "WALKING")
  ActivityNames <- replace(ActivityNames, ActivityNames == 2, "WALKING_UPSTAIRS")
  ActivityNames <- replace(ActivityNames, ActivityNames == 3, "WALKING_DOWNSTAIRS")
  
  ActivityNames <- replace(ActivityNames, ActivityNames == 4, "SITTING")
  ActivityNames <- replace(ActivityNames, ActivityNames == 5, "STANDING")
  ActivityNames <- replace(ActivityNames, ActivityNames == 6, "LYING")
  LabeledMergedData <- cbind(ActivityNames, MeanAndStd)
  TidyData <- cbind(mergedSubjects, LabeledMergedData)
  ColumnNames <- colnames(TidyData)
  ColumnNames[1] <- "SubjectNumber"
  ColumnNames[2] <- "ActivityName"
  
  ColumnNames <- gsub("-", "", ColumnNames)
  ColumnNames <- gsub("\\(", "", ColumnNames)
  ColumnNames <- gsub("\\)", "", ColumnNames)
  ColumnNames <- tolower(ColumnNames)
  colnames(TidyData) <- ColumnNames
  stdcolnames <- grep("std", colnames(TidyData))
  meanTidyData <- TidyData[,c(1:48)]
  
  FinalTidyData <- ddply(meanTidyData, c("subjectnumber", "activityname"), function(df) c(colMeans(df[c(3:48)])))
  write.table(FinalTidyData, file = "TidyData.txt", row.name = FALSE)
  
}
TidyScript()




======================================
