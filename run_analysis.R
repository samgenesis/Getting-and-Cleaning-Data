# Data source : https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# This R script does the following 

# 1. Merges the training and the test sets to create one data set.

X_train_temp1 <- read.table("train/X_train.txt")
X_tesxt_temp2 <- read.table("test/X_test.txt")
X <- rbind(X_train_temp1, X_tesxt_temp2)

subject_train_temp1 <- read.table("train/subject_train.txt")
subject_test_temp2 <- read.table("test/subject_test.txt")
S <- rbind(subject_train_temp1, subject_test_temp2)

y_train_temp1 <- read.table("train/y_train.txt")
y_test_temp2 <- read.table("test/y_test.txt")
Y <- rbind(y_train_temp1, y_test_temp2)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_good_feature <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_good_features]
names(X) <- features[indices_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)

# Write to file named "merged_clean_data.txt"

write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

unique_Subjects = unique(S)[,1]
number_of_Subjects = length(unique(S)[,1])
number_of_Activities = length(activities[,1])
number_of_Cols = dim(cleaned)[2]
result = cleaned[1:(number_of_Subjects*number_of_Activities), ]

row = 1
for (s in 1:number_of_Subjects) {

    for (a in 1:number_of_Activities) {
    
        result[row, 1] = unique_Subjects[s]
        result[row, 2] = activities[a, 2]
        cleaned_temp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:number_of_Cols] <- colMeans(cleaned_temp[, 3:number_of_Cols])
        row = row+1

    }

}

# Write to file named "data_set_with_the_averages.txt"

write.table(result, "data_set_with_the_averages.txt")
write.table(result, "data_set_with_the_averages_rownameFALSE.txt",row.names=FALSE)