# This function is used to tidy the column names
getcolumnnames <- function(filepath,seperator=" "){
  features <- read.table(file = filepath ,sep = seperator )
  features$V2 <- gsub("\\(","",features$V2)
  features$V2 <- gsub("-","",features$V2)
  features$V2 <- gsub("\\)","",features$V2)
  features$V2 <- gsub(",","",features$V2)
  features$V2 <- tolower(features$V2)
  return (features$V2)
}

#This function is used to extract the activity & Subject Ids from relevant files
getids <- function(filepath){
  ids <- read.table(file = filepath ,sep = "\n" )
  return (ids$V1)
}

#This function is used for grabbing of dataset from file
getmaindataset <- function (filepath){  
  ds <- read.table(file = filepath ,header=FALSE)
  return(ds)
}

#This function is used to assign the columnnames to data set
setcolumnnames <- function (dataset,clmnnames){
  names(dataset) <- clmnnames
  return(dataset)
}

#This function is used to translate activity Id to activity name
getactvitynames <- function (filepath,activityids){
  ds1 <- read.table(file = filepath ,header=FALSE)
  actname <- as.character(ds1$V2)
  activityname <- character()
  for(i in 1:length(activityids)){
    indx <- activityids[i]
    aname <- actname[indx]
    activityname[i] <- aname
  }
  return(activityname)
}
#Main Program Area
#File Urls 
activitynamefileurl <- "./UCI HAR Dataset/activity_labels.txt"
testdsfileurl <- "./UCI HAR Dataset/test/X_test.txt"
cnamefileurl <- "./UCI HAR Dataset/features.txt"
traindsfileurl <- "./UCI HAR Dataset/train/X_train.txt"
testsubjectfileurl <- "./UCI HAR Dataset/test/subject_test.txt"
testactivityfileurl <-"./UCI HAR Dataset/test/y_test.txt"
trainsubjectfileurl <- "./UCI HAR Dataset/train/subject_train.txt"
trainactivityfileurl <-"./UCI HAR Dataset/train/y_train.txt"

#Get datasets without header
testset <- getmaindataset(testdsfileurl)
trainset <- getmaindataset(traindsfileurl)
# Get Column names
clmnnames <- getcolumnnames(cnamefileurl)
#assign column names to datasets
testset <- setcolumnnames(testset,clmnnames)
trainset <- setcolumnnames(trainset,clmnnames)
#Insert subject & activity ids columns to the datasets
testsubjectclmn <- getids(testsubjectfileurl)
testactivityclmn <- getids(testactivityfileurl)
trainsubjectclmn <- getids(trainsubjectfileurl)
trainactivityclmn <- getids(trainactivityfileurl)
testset$subject <- testsubjectclmn
testset$activity <- testactivityclmn
trainset$subject <- trainsubjectclmn
trainset$activity <- trainactivityclmn
#Join the datasets
ds <- rbind(trainset,testset)
#Extract the mean and standard deviation columns
meandeviationcolumns <- grep("mean\\(\\)|std\\(\\)",clmnnames)
dsmeandeviation <- ds[,meandeviationcolumns]
#add the activity names column to dataset
act <- ds$activity
activitynames <- getactvitynames(activitynamefileurl,act)
ds$activityname <- activitynames
# Remove the duplicate activity id 
ds$activity <- NULL
# use aggregate function to group and get the mean of the variables based on per subject per activity name and sort in the same order
library(plyr)
ds1 <- aggregate(. ~subject + activityname, ds, mean)
ds1 <- ds1[order(ds1$subject,ds1$activityname),]
write.table(ds1, file = "tidydata.txt",row.name=FALSE)
