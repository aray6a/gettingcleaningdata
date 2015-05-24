###Getting & Cleaning Data Project Code Book
####Author:  Alim Ray

This is a description of my Coursera Getting & Cleaning Project.

Experimental Design: The experimental design can be found here(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).  The data used can be found here(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip).

Raw Data:
From training and test sets of experiments:
tBodyAcc-mean()-X, tBodyAcc-mean()-Y, tBodyAcc-mean()-Z, tBodyAcc-std()-X, tBodyAcc-std()-Y, tBodyAcc-std()-Z, tGravityAcc-mean()-X, tGravityAcc-mean()-Y, tGravityAcc-mean()-Z, tGravityAcc-std()-X, tGravityAcc-std()-Y, tGravityAcc-std()-Z, tBodyAccJerk-mean()-X, tBodyAccJerk-mean()-Y, tBodyAccJerk-mean()-Z, tBodyAccJerk-std()-X, tBodyAccJerk-std()-Y, tBodyAccJerk-std()-Z, tBodyGyro-mean()-X, tBodyGyro-mean()-Y, tBodyGyro-mean()-Z, tBodyGyro-std()-X, tBodyGyro-std()-Y, tBodyGyro-std()-Z, tBodyGyroJerk-mean()-X, tBodyGyroJerk-mean()-Y, tBodyGyroJerk-mean()-Z, tBodyGyroJerk-std()-X, tBodyGyroJerk-std()-Y, tBodyGyroJerk-std()-Z, tBodyAccMag-mean(), tBodyAccMag-std(), tGravityAccMag-mean(), tGravityAccMag-std(), tBodyAccJerkMag-mean(), tBodyAccJerkMag-std(), tBodyGyroMag-mean(), tBodyGyroMag-std(), tBodyGyroJerkMag-mean(), tBodyGyroJerkMag-std(), fBodyAcc-mean()-X, fBodyAcc-mean()-Y, fBodyAcc-mean()-Z, fBodyAcc-std()-X, fBodyAcc-std()-Y, fBodyAcc-std()-Z, fBodyAccJerk-mean()-X, fBodyAccJerk-mean()-Y, fBodyAccJerk-mean()-Z, fBodyAccJerk-std()-X, fBodyAccJerk-std()-Y, fBodyAccJerk-std()-Z, fBodyGyro-mean()-X, fBodyGyro-mean()-Y, fBodyGyro-mean()-Z, fBodyGyro-std()-X, fBodyGyro-std()-Y, fBodyGyro-std()-Z, fBodyAccMag-mean(), fBodyAccMag-std(), fBodyBodyAccJerkMag-mean(), fBodyBodyAccJerkMag-std(), fBodyBodyGyroMag-mean(), fBodyBodyGyroMag-std(), fBodyBodyGyroJerkMag-mean(), fBodyBodyGyroJerkMag-std()

Process Data:  Below is description of the tidy data set.  The dimensions for all measures and magnitudes were moved into their own fields.  The 'Stat' (mean or standard deviation) was created as a field.  Lastly, type was added to differentiate between test data and training data.


Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	226578 obs. of  8 variables:
 $ ID     : chr  "1" "1" "1" "1" ...
 $ measure: Factor w/ 11 levels "fBodyAcc","fBodyAccJerk",..: 1 1 2 2 3 3 4 4 5 5 ...
 $ stat   : Factor w/ 2 levels "mean","std": 1 2 1 2 1 2 1 2 1 2 ...
 $ X      : num  -0.919 -0.948 -0.9 -0.924 NA ...
 $ Y      : num  -0.918 -0.925 -0.937 -0.943 NA ...
 $ Z      : num  -0.789 -0.636 -0.924 -0.948 NA ...
 $ mag    : num  -0.791 -0.711 NA NA -0.895 ...
 $ type   : Factor w/ 2 levels "test","train": 1 1 1 1 1 1 1 1 1 1 ...

Executing Analysis: Please place the zip file in the same directory as run_analysis.R.  From RStudio (or the R interactive base) set your working directory to the directory which contains run_analysis.R and the zip file.  Source "run_analysis.R" and execute testAndTrain().  This function will return the tidy data set.
