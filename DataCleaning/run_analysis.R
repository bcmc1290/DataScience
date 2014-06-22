## Brent McPherson
## 20140621
## Syntax to merge cell phone data sets
##
##########################################################################################
##########################################################################################
## read in the data files

## read in testing/training x data
testx <- read.table("UCI_HAR_Dataset/test/X_test.txt")
trainx <- read.table("UCI_HAR_Dataset/train/X_train.txt")

## read in testing/training y data
testy <- read.table("UCI_HAR_Dataset/test/y_test.txt")
trainy <- read.table("UCI_HAR_Dataset/train/y_train.txt")

## read in the features list
features <- read.table("UCI_HAR_Dataset/features.txt")

## read in subject IDs
subidtest <- read.table("UCI_HAR_Dataset/test/subject_test.txt")
subidtrain <- read.table("UCI_HAR_Dataset/train/subject_train.txt")

## add rownames
testx <- cbind(subidtest, testx)
## testy <- cbind(subidtest, testy)

trainx <- cbind(subidtrain, trainx)
## trainy <- cbind(subidtrain, trainy)

## Why are we given folders w/ spaces in the path?
## In Linux that's a real pain... but I don't need them?

##
## 1. Merges the training and the test sets to create one data set.
##

## merge test/train x data
datx <- rbind(trainx, testx)

## merge test/train y data
daty <- rbind(trainy, testy)

##
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
##

vars <- as.character(features[, 2])

## rownames of test/train x match features
colnames(datx) <- c("ID", vars)

## find and combine indices for mean()/sd() variables
mnindex <- grep("*-mean[^Freq]()", colnames(datx))
sdindex <- grep("*-std()", colnames(datx))
idindex <- grep("ID", colnames(datx))
subindex <- sort(c(idindex, mnindex, sdindex))

## create mean()/sd() data set
dat <- datx[, subindex]

##
## 3. Uses descriptive activity names to name the activities in the data set
##

## convert daty to a factor
daty <- factor(unlist(daty), labels = c("WALKING", "WALKING_UPSTAIRS",
                             "WALKING_DOWNSTAIRS", "SITTING",
                             "STANDING", "LAYING"))

## give the variable a better name in the merge
Activity <- daty
dat <- cbind(dat, Activity)

##
## 4. Appropriately labels the data set with descriptive variable names.
##

library(reshape2)

datmelt <- melt(dat, id = c("Activity", "ID"), measure.vars = colnames(dat[, 1:66]))
datcast <- dcast(datmelt, Activity + ID ~ ..., mean)

write.table(datcast, "tidydata.csv", quote = FALSE, sep = ",")

##
## 5. Creates a second, independent tidy data set with the average of each variable
##    for each activity and each subject.
##

dat5melt <- melt(dat, id = c("Activity", "ID"), measure.vars = colnames(dat[, 1:66]))
dat5cast <- dcast(datmelt, Activity ~ ID, mean)

write.table(dat5cast, "avg_id_BY_task.csv", quote = FALSE, sep = ",")
