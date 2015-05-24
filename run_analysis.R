#run_analysis.R
#
#R script which unzips and tidies the UCI HAR Dataset.
#Getting and Cleaning Data Coursera Project
#author:  Alim Ray

library(tidyr)
library(dplyr)

#Load data from test or training set
loadAll <- function( type = 'test' ) {
  #Check to see if folder or zip file exists
  if ( !file.exists("UCI HAR Dataset") ) {
    if (!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
      print("Cannot find getdata-projectfiles-UCI HAR Dataset.zip file in working directory.")
      print("Please add this file to the working directory so the analysis can be executed.")
      return()
    }
    unzip("getdata-projectfiles-UCI HAR Dataset.zip")
  }
  uci <- read.table( paste( "UCI HAR Dataset//", type, "//X_", type, ".txt", sep = '' ) )
  featureColumns <- read.table("UCI HAR Dataset//features.txt")
  names(uci) <- featureColumns$V2
  uci
}

#Load data and filter for mean and standard deviation measures
loadMain <- function( type = 'test' ) {
  uci <- loadAll( type )
  uci <- uci[ names(uci)[ grepl( '(mean\\(|std\\()', names(uci) ) ] ]
  uci$ID <- rownames(uci)
  uci
}


#Reformat a section of dimension variables at a time
#i.e. Convert
#tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
# 5                 10                  20
#to
#               X     Y   Z
#tBodyAcc mean  5     10  20
#tBodyAcc std
#'tBodyAcc' is the section
rearrange <- function(dta, section = 'tBodyAcc' ) {
  #Get the correct variables for this section
  n = names( dta )
  shortNames = n[ grepl( paste(section, '-', sep = ''), n)]
  shortNames = shortNames[ order(shortNames)]
  shortNames = c("ID", shortNames)
  shortData = dta[ shortNames ]

  #Gather section variables and separate into measure and stat
  shortData = shortData %>% gather( measure, value, -ID)
  shortData <- shortData %>% separate(measure, c("measure", "stat", "dimension"), sep = "-" )
  shortData$stat <- gsub( "\\W", "", shortData$stat)

  #Spread dimensions
  shortData <- shortData %>% spread( dimension, value )
}

#Create tidy data set of all dimensional measurements
# i.e.
#                 X     Y   Z
#tBodyAcc mean
#tBodyAcc std
#tGravityACC mean
rearrangeAll <- function( dta ) {
  #Get measurements names from data
  dataNames <- names(dta)[ grepl( 'X|Y|Z', names(dta))]
  dNDF <- data.frame( dimmeas = dataNames, val = 1)
  dNDF <- dNDF %>% separate(dimmeas, c('meas', 'stat', 'dimm')) %>% select( meas ) %>% unique
  measurements <- dNDF$meas

  #Create tidy data frame that contains all dimensional measurements
  rearrangedDF <- NULL
  for ( m in measurements) {
    measurementDF <- rearrange( dta, m)
    rearrangedDF <- bind_rows( measurementDF, rearrangedDF)
  }

  rearrangedDF
}

#Reformat a section of dimension variables at a time
#i.e. Convert
#tBodyAccMag-mean()     tBodyAccMag-std()
# 5                       10
#to
#
#measure  stat  mag
#tBodyAcc mean  5
#tBodyAcc std   10
#'tBodyAcc' is the section
rearrangeMag <- function(dta, section = 'tBodyAccMag') {
  #Get the correct variables for this section
  n = names( dta )
  shortNames = n[ grepl( paste(section, '-', sep = ''), n)]
  shortNames = shortNames[ order(shortNames)]
  shortNames = c("ID", shortNames)
  shortData = dta[ shortNames ]

  #Gather section variables and separate into measure and stat
  shortData = shortData %>% gather( measure, mag, -ID)
  shortData <- shortData %>% separate(measure, c("measure", "stat"), sep = "-" )

  #Clean up stat and measure fields
  shortData$stat <- gsub( "\\W", "", shortData$stat)
  shortData$measure <- gsub( "Mag", "", shortData$measure)
  shortData
}

#Create tidy data set of all measurements
# i.e.
#                 mag
#tBodyAcc mean
#tBodyAcc std
#tGravityACC mean
rearrangeMagAll <- function(dta) {
  #Get measurements names from data
  dataNames <- names(dta)[ grepl( 'Mag', names(dta))]
  dNDF <- data.frame( dimmeas = dataNames, val = 1)
  dNDF <- dNDF %>% separate(dimmeas, c('meas', 'stat' ), sep = '-') %>% select( meas ) %>% unique
  measurements <- dNDF$meas

  #Create tidy data frame that contains all mag measurements
  rearrangedDF <- NULL
  for ( m in measurements) {
    measurementDF <- rearrangeMag( dta, m)
    rearrangedDF <- bind_rows( measurementDF, rearrangedDF)
  }

  rearrangedDF
}

mergeAll <- function( type = 'test') {
    data <-loadMain( type )
    xyz <- rearrangeAll( data )
    mag <- rearrangeMagAll( data )
    merged <- merge( xyz, mag, all = T )
}

testAndTrain <- function() {
  test <- mergeAll('test')
  test$type <- 'test'

  train <- mergeAll('train')
  train$type <- 'train'

  total <- bind_rows(test, train)
  total$type <- factor(total$type)
  total$stat <- factor(total$stat)
  total$measure <- factor(total$measure)
  total
}
