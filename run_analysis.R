if(!file.exists("./datassign")){dir.create("./datassign")}
 fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 download.file(fileUrl, destfile = "./datassign/Dataset.zip")
 for (i in dir(pattern="\.zip$")) {
   unzip(i)
 }
y_train <-        read.table("./datassign/UCI HAR Dataset/train/y_train.txt")
names(y_train) <- "t_labels"

y_test <-       read.table("./datassign/UCI HAR Dataset/test/y_test.txt")
names(y_test) <- "t_lables"

X_test <-       read.table("./datassign/UCI HAR Dataset/test/X_test.txt")

subject_test <- read.table("./datassign/UCI HAR Dataset/test/subject_test.txt")
names(subject_test) <- "sub_id"


subject_train <-  read.table("./datassign/UCI HAR Dataset/train/subject_train.txt")
names(subject_train) <- "sub_id"

X_train <- read.table("./datassign/UCI HAR Dataset/train/X_train.txt")

features <- read.table("./datassign/UCI HAR Dataset/features.txt")
activity_labels <- read.table("./datassign/UCI HAR Dataset/activity_labels.txt")

X_test <- cbind(subject_test, y_test, X_test)
X_train <- cbind(subject_train, y_train, X_train)

names(X_test) <- names(X_train) #without this command, the next one could not match names and execute
X_data <- rbind(X_test, X_train)
zz <- vector(length = 561)
bb <- unfactor(features)
gg <- simplify(bb)
for(i in 1:(ncol(X_data)-2)) {
  zz[i] <- gg[i, 2]
}
yy <- as.character(zz)
names(X_data) <- c("sub_id", "t_labels", yy)

kk <- grep("mean", names(X_data))
ll <- grep("std", names(X_data))
ss <- c(kk, ll)
sss <- sort(ss)
X_data_xtc <- X_data[, c(1, 2, sss)]

#activity_labels %>% map_if(is.factor, as.character) -> activity_labels

rrrrr <- select(activity_labels, V2)
rtr <- unfactor(rrrrr)            #varhandle package
simplify(X_data_xtc$t_labels)
simplify(rtr)
for(i in 1:nrow(X_data_xtc)) {
  
  ccc <- X_data_xtc$t_labels[i]
  X_data_xtc$t_labels[i] <- rtr[ccc, ]
}
X_data_xtc %>% gather(activities, count) %>% separate(activities, c(rtr$V2)) 
