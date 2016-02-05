# downloading file and creating directory

if(!file.exists("./datassign")){dir.create("./datassign")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./datassign/Dataset.zip")

# unpack the archive!

#####################

## reading the files & naming the columns
y_train <- read.table("./datassign/UCI HAR Dataset/train/y_train.txt")
names(y_train) <- "t_labels"

y_test <- read.table("./datassign/UCI HAR Dataset/test/y_test.txt")
colnames(y_test) <- "t_lables"

X_test <- read.table("./datassign/UCI HAR Dataset/test/X_test.txt")

subject_test <- read.table("./datassign/UCI HAR Dataset/test/subject_test.txt")
colnames(subject_test) <- "sub_id"


subject_train <-  read.table("./datassign/UCI HAR Dataset/train/subject_train.txt")
colnames(subject_train) <- "sub_id"

X_train <- read.table("./datassign/UCI HAR Dataset/train/X_train.txt")

features <- read.table("./datassign/UCI HAR Dataset/features.txt")
activity_labels <- read.table("./datassign/UCI HAR Dataset/activity_labels.txt")

X_test <- cbind(subject_test, y_test, X_test)
X_train <- cbind(subject_train, y_train, X_train)

names(X_test) <- names(X_train) 
X_data <- rbind(X_test, X_train)

zz <- vector(length = ncol(X_data)-2)
bb <- unfactor(features)


for(i in 1:(ncol(X_data)-2)) {
  zz[i] <- bb[i, 2]
}

yy <- as.character(zz)
names(X_data) <- c("sub_id", "t_labels", yy)


#   Extraction of the mean and standard deviation measurement columns

kk <- grep("mean", names(X_data))
ll <- grep("std", names(X_data))
ss <- c(kk, ll)
sss <- sort(ss)
X_data_xtc <- X_data[, c(1, 2, sss)]

###    substitution of activity label with activity name
rrrrr <- select(activity_labels, V2)
rtr <- unfactor(rrrrr)            #varhandle package

for(i in 1:nrow(X_data_xtc)) {
  
  ccc <- X_data_xtc$t_labels[i]
  X_data_xtc$t_labels[i] <- rtr[ccc, ]
}



# Adjusting the names of the columns
colNames  <- colnames(X_data_xtc)

for (i in 3:length(colNames))  
{ 
  colNames[i] <- gsub("\\()","",colNames[i]) 
  colNames[i] <- gsub("-std$","StdDev",colNames[i]) 
  colNames[i] <- gsub("-mean","Mean",colNames[i]) 
  colNames[i] <- gsub("^(t)","time",colNames[i]) 
  colNames[i] <- gsub("^(f)","freq",colNames[i]) 
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i]) 
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i]) 
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i]) 
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i]) 
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i]) 
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i]) 
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i]) 
} 
colnames(X_data_xtc) <- colNames

Tidy_d_set <- group_by(X_data_xtc, sub_id, t_labels) %>% 
  summarise_each(funs(mean))

###  creation of a second, independent tidy data set with the average 
###  of each variable for each activity and each subject

write.table(Tidy_d_set, file = "./Tidy_d_set.txt", row.names=FALSE)
