library(reshape2)

# Get and clean data

# Store zip file URL and name
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "getdata_dataset.zip"

# Downlaod zip file if it doesn't exist
if (!file.exists(zipfile)) {
    download.file(Url, zipfile, mode = "wb")
}

# Store dataset name
dataset <- "UCI HAR Dataset"
if (!file.exists(dataset)) {
    unzip(zipfile)
}

# Read in training data
X_train <- read.table(file.path(dataset, "train", "X_train.txt"))
y_train <- read.table(file.path(dataset, "train", "y_train.txt"))
subject_train <- read.table(file.path(dataset, "train", "subject_train.txt")) 

# Read in testing data
X_test <- read.table(file.path(dataset, "test", "X_test.txt"))
y_test <- read.table(file.path(dataset, "test", "y_test.txt"))
subject_test <- read.table(file.path(dataset, "test", "subject_test.txt"))

# Read in features and keep them character
features <- read.table(file.path(dataset, "features.txt"), as.is = TRUE)

# Read in activity labels and keep them character
activity_labels <- read.table(file.path(dataset, "activity_labels.txt"), as.is = TRUE)

# Step 1: Merges the training and the test sets to create one data set
X_all <- rbind(X_train, X_test)
y_all <- rbind(y_train, y_test)
subject_all <- rbind(subject_train, subject_test)
data_all <- cbind(subject_all, X_all, y_all)  # Merged data set

# Step 2: Extracts only the measurements on the mean and standard deviation 
# for each measurement
mean_std_id <- grep(".*mean.*|.*std.*", features[,2])
data_extract <- cbind(subject_all, X_all[, mean_std_id], y_all)
# Rename middle columns later
colnames(data_extract) <- c("subject", colnames(X_all[, mean_std_id]), "activity")

# Step 3: Uses descriptive activity names to name the activities in the data set
data_extract$activity <- factor(data_extract$activity, 
                                levels = activity_labels[, 1], 
                                labels = activity_labels[, 2])

# Step 4: Appropriately labels the data set with descriptive variable names
mean_std_feat <- features[mean_std_id, 2]
mean_std_feat <- gsub("[()]", "", mean_std_feat)  # Remove "()"
colnames(data_extract) <- c("subject", mean_std_feat, "activity")
data_extract$subject <- as.factor(data_extract$subject)

# Step 5: From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity 
melt <- melt(data = data_extract, id = c("subject", "activity"))
meansummary <- dcast(data = melt, subject + activity ~ variable, fun.aggregate = mean)

write.table(meansummary, "tidy_data.txt", row.names = FALSE, quote = FALSE)
