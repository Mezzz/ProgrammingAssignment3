library(plyr)

# This function allow to get tidy data set that can be used for later analysis from data collected from the accelerometers from the Samsung Galaxy S smartphone.
# A full description is available at the site where the data was obtained:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
runAnalysis = function() {
    # Download, unzip and merges the training and the test sets to create one data set.
    merged <- getData()
    # Extract measurements on the mean and standard deviation for each measurement. 
    meanStd <- extractMeanAndStd(merged$x)
    # Set activities
    activities <- setActivities(merged$y)
    # Set label
    colnames(merged$subject) <- "subject"
    # Merge mean and standard deviation with activities data frame
	binded <- cbind(meanStd, activities, merged$subject)
    # Create tidy data set
	tidy <- ddply(binded, .(subject, activity), function(x) colMeans(x[,1:60]))
    # Write tidy data set 
    write.table(tidy, "tidy_dataset.txt", row.name=FALSE)
}

getData = function() 
{
    if (!file.exists("data")) 
	{
        dir.create("data")
    }
    if (!file.exists("data/Source Dataset")) 
	{
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        zipName="data/Source Dataset.zip"
		download.file(fileURL, destfile=zipName)
        unzip(zipName, exdir="data/Source Dataset")
    }
    train_x <- read.table("data/Source Dataset/UCI HAR Dataset/train/X_train.txt")
    train_y <- read.table("data/Source Dataset/UCI HAR Dataset/train/y_train.txt")
    train_subject <- read.table("data/Source Dataset/UCI HAR Dataset/train/subject_train.txt")
    test_x <- read.table("data/Source Dataset/UCI HAR Dataset/test/X_test.txt")
    test_y <- read.table("data/Source Dataset/UCI HAR Dataset/test/y_test.txt")
    test_subject <- read.table("data/Source Dataset/UCI HAR Dataset/test/subject_test.txt")
    merged_x <- rbind(train_x, test_x)
    merged_y <- rbind(train_y, test_y)
    merged_subject <- rbind(train_subject, test_subject)
    list(x=merged_x, y=merged_y, subject=merged_subject)
}

extractMeanAndStd = function(df) {
    # Extract measurements on the mean and standard deviation for each measurement. 
    features <- read.table("data/Source Dataset/UCI HAR Dataset/features.txt")
    means <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
    stds <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
    extracted <- df[, (means | stds)]
    colnames(extracted) <- features[(means | stds), 2]
    extracted
}

setActivities = function(df) {
    # Set descriptive activity names to name the activities in the data set
    colnames(df) <- "activity"
    df$activity[df$activity == 1] = "WALKING"
    df$activity[df$activity == 2] = "WALKING_UPSTAIRS"
    df$activity[df$activity == 3] = "WALKING_DOWNSTAIRS"
    df$activity[df$activity == 4] = "SITTING"
    df$activity[df$activity == 5] = "STANDING"
    df$activity[df$activity == 6] = "LAYING"
    df
}