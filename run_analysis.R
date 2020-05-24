library(dplyr)

# read train data
X_train &lt;- read.table(&quot;./UCI HAR Dataset/train/X_train.txt&quot;)
Y_train &lt;- read.table(&quot;./UCI HAR Dataset/train/Y_train.txt&quot;)
Sub_train &lt;- read.table(&quot;./UCI HAR Dataset/train/subject_train.txt&quot;)

# read test data
X_test &lt;- read.table("/UCI HAR Dataset/test/X_test.txt")
Y_test &lt;- read.table("./UCI HAR Dataset/test/Y_test.txt")
Sub_test &lt;- read.table("./UCI HAR Dataset/test/subject_test.txt&quot")

# read data description
variable_names &lt;- read.table("./UCI HAR Dataset/features.txt")

# read activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# 1. Merges the training and the test sets to create one data set.
X_total <- rbind(X_train, X_test)
Y_total <- rbind(Y_train, Y_test)
Sub_total <- rbind(Sub_train, Sub_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
X_total <- X_total[,selected_var[,1]]

# 3. Uses descriptive activity names to name the activities in the data set

colnames(Y_total) <-"activity"
Y_total$activitylabel <- factor(Y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- Y_total[,-1]

# 4. Appropriately labels the data set with descriptive variable names.
colnames(X_total) <- variable_names[selected_var[,1],2]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
colnames(Sub_total) <- "subject";
total <- cbind(X_total, activitylabel, Sub_total)
total_mean <- total %&gt;% group_by(activitylabel, subject) %&gt;% summarize_each(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)