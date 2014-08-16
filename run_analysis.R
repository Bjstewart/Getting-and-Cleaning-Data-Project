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
