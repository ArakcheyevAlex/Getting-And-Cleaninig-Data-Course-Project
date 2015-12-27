if (!require("dplyr")) {
    install.packages("dplyr")
}
require(dplyr)

# Paths
uciHarDataDir <- "../UCI HAR Dataset/"
featureFileName <- paste(uciHarDataDir, "features.txt", sep = "")
activityLabelsFileName <- paste(uciHarDataDir, "activity_labels.txt", sep = "")

xTrainFileName <- paste(uciHarDataDir, "train/X_train.txt", sep = "")
yTrainFileName <- paste(uciHarDataDir, "train/y_train.txt", sep = "")
subjectTraneFileName <- paste(uciHarDataDir, "train/subject_train.txt", sep = "")

xTestFileName  <- paste(uciHarDataDir, "test/X_test.txt", sep = "")
yTestFileName  <- paste(uciHarDataDir, "test/y_test.txt", sep = "")
subjectTestFileName <- paste(uciHarDataDir, "test/subject_test.txt", sep = "")

outputFile <- paste(uciHarDataDir, "/sensorAvgByActSub.txt", sep = "")


# Loading raw data
features <- read.table(featureFileName, colClasses = c("character"))
activityLabels <- read.table(activityLabelsFileName, col.names = c("ActivityId", "Activity"))
xTrain <- read.table(xTrainFileName)
yTrain <- read.table(yTrainFileName)
subjectTrain <- read.table(subjectTraneFileName)
xTest <- read.table(xTestFileName)
yTest <- read.table(yTestFileName)
subjectTest <- read.table(subjectTestFileName)

# 1. Merges the training and the test sets to create one data set.

trainingSensorData <- cbind(cbind(xTrain, subjectTrain), yTrain)
testSensorData <- cbind(cbind(xTest, subjectTest), yTest)
sensorData <- rbind(trainingSensorData, testSensorData)

names(sensorData) <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

sensorDataMeanAndStd <- sensorData[,grepl("mean|std|Subject|ActivityId", names(sensorData))]

# 3. Uses descriptive activity names to name the activities in the data set

sensorDataMeanAndStd <- join(sensorDataMeanAndStd, activityLabels, by = "ActivityId", match = "first")
sensorDataMeanAndStd <- sensorDataMeanAndStd[,-1]

# 4. Appropriately labels the data set with descriptive names.

# Remove parentheses
names(sensorDataMeanAndStd) <- gsub('\\(|\\)',"",names(sensorDataMeanAndStd), perl = TRUE)
# Make syntactically valid names
names(sensorDataMeanAndStd) <- make.names(names(sensorDataMeanAndStd))
# Make clearer names
names(sensorDataMeanAndStd) <- gsub('^t',"Time.",names(sensorDataMeanAndStd))
names(sensorDataMeanAndStd) <- gsub('^f',"Frequency.",names(sensorDataMeanAndStd))
names(sensorDataMeanAndStd) <- gsub('\\.mean',".Mean",names(sensorDataMeanAndStd))
names(sensorDataMeanAndStd) <- gsub('\\.std',".StandardDeviation",names(sensorDataMeanAndStd))
names(sensorDataMeanAndStd) <- gsub('Freq\\.',"Frequency.",names(sensorDataMeanAndStd))
names(sensorDataMeanAndStd) <- gsub('Freq$',"Frequency",names(sensorDataMeanAndStd))

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sensorAverageByActivitySubject = ddply(sensorDataMeanAndStd, c("Subject","Activity"), numcolwise(mean))
write.table(sensorAverageByActivitySubject, file = outputFile, row.name=FALSE)
