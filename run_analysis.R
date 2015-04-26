##  This script does the following. 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

rm (list = ls())

if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")

wd <-getwd()
setwd(wd)

## Download File and Extract Data to UCI HAR Dataset
dataset_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataset_file_name <- "UCI_HAR_Dataset.zip"
dataset_extract_dir <- "UCI HAR Dataset"
if (!file.exists(dataset_file_name)) {
  download.file(dataset_url, file.path(wd, dataset_file_name),method = "curl")
}
if (!file.exists(dataset_extract_dir)) {
  unzip(dataset_file_name)
}


# Load: activity labels
actLabels <- read.table(file.path(dataset_extract_dir, "activity_labels.txt"))[,2]

# Load: data column names
features <- read.table(file.path(dataset_extract_dir, "features.txt"))[,2]

# Extract only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", features)

# Load and process X_test & y_test data.
dt_X_test <- read.table(file.path(dataset_extract_dir, "test", "X_test.txt"))
y_test <- read.table(file.path(dataset_extract_dir, "test", "y_test.txt"))
subject_test <- read.table(file.path(dataset_extract_dir, "test", "subject_test.txt"))

names(dt_X_test) = features

# Extracting only the measurements on the mean and standard deviation for each measurement.
dt_X_test = dt_X_test[,extract_features]

# Loading activity labels
y_test[,2] = actLabels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"

# Binding data
test_data <- cbind(as.data.table(subject_test), y_test, dt_X_test)

# Loading and process X_train & y_train data.
X_train <- read.table(file.path(dataset_extract_dir, "train", "X_train.txt"))
y_train <- read.table(file.path(dataset_extract_dir, "train", "y_train.txt"))

subject_train <- read.table(file.path(dataset_extract_dir, "train", "subject_train.txt"))

names(X_train) = features

# Extracting only the measurements on the mean and standard deviation for each measurement.
X_train = X_train[,extract_features]

# Loading activity data
y_train[,2] = actLabels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

# Binding data
train_data <- cbind(as.data.table(subject_train), y_train, X_train)

# Mergeing test and train data
data = rbind(test_data, train_data)

id_labels   = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(data), id_labels)
melt_data      = melt(data, id = id_labels, measure.vars = data_labels)

# Apply mean function
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "tidydata.txt", row.names = FALSE)