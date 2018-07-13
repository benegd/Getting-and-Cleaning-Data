##Loading required packages
library(plyr)
library(dplyr)





##Importing the data

labels <- read.table("./UCI HAR Dataset/features.txt")
#labels for columns is taken from labels data
test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = labels[,2])
train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = labels[,2])
ytest <- read.table("./UCI HAR Dataset/test/Y_test.txt", col.names = "activity")
ytrain <- read.table("./UCI HAR Dataset/train/Y_train.txt", col.names = "activity")
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
yfactors <- read.table("./UCI HAR Dataset/activity_labels.txt")
#removing labels data
rm(labels)





##Adding activity type to datasets as factor

ytest <- sapply(ytest, function(x){yfactors[x,2]})
ytrain <- sapply(ytrain, function(x){yfactors[x,2]})
test$activity <- ytest
train$activity <- ytrain
#removing the ytest and ytrain variables as they are now in the dataset
rm(ytest, ytrain)





##Adding subjects to dataset as factor

test$subject <- unlist(subjecttest)
train$subject <- unlist(subjecttrain)
#removing subjecttrain and subjecttest as they are now in the dataset
rm(subjecttest, subjecttrain)





##Adding dataset variable
#Adding "train" and "test" factor variable to sets
#This allows the set to still be identified once the data is merged

test$dataset <- rep("test", nrow(test))
train$dataset <- rep("train", nrow(train))






##Merging the data

alldata <- rbind(train, test)
#removing train and test sets
rm(train, test)


##Changing subject to factor variable
alldata$subject <- factor(alldata$subject)
rm(yfactors)


##Cleaning up the variable names
#converting to lower case and removing the dots

colnames(alldata) <- gsub("\\.", "", tolower(colnames(alldata)))




##Extracting mean and sd columns
#using column names to filter data
#using "*mean[^F]" to match column names alternative "*mean*" also matches meanFreq

#extracting the columnumbers for the activity and subject fiels to keep them in the dataset
activitycolumnnumber <- match("activity", colnames(alldata))
subjectcolumnnumber <- match("subject", colnames(alldata))
#extracting the mean and std column numbers
columnsubset <- grep("*mean[^F]", colnames(alldata))
columnsubset <- c(grep("*std*", colnames(alldata)), columnsubset)
columnsubset <- c(activitycolumnnumber, subjectcolumnnumber, columnsubset)
datasubset <- alldata[,columnsubset]






##mean data set for each activity and subject

#function for extracting column means
columnmeans <- function(x) {
    x1 <- select(x, -c("activity", "subject"))
    colMeans(x1)
}
#split data by both activity and subject and calculating means
datasplit<- sapply(split(datasubset, list(datasubset$activity, datasubset$subject)), columnmeans)
rm(alldata, yfactors, activitycolumnnumber, columnsubset, subjectcolumnnumber)





## outputting datasplit
write.table(datasplit, file = "./output.txt", row.names = FALSE)

