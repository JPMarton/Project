

#   Getting & Cleaning Data Course - Course Project 

#   Create your working directory
getwd()
setwd("/Users/.../Project")

#   load required packages
require(data.table); require(reshape2)

#   location of the project data 
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"



#   downloads the zip file in the working directory and extracts the archives in the "UCI HAR DAtaset" folder
path <- getwd()
download.file(url, file.path(path, "Dataset.zip"))
if (!file.exists(path)) {
  dir.create(path)
}
extract <- file.path(path, "UCI HAR Dataset")
path <- extract
unzip("Dataset.zip")
unlink(file.path(path, "train", "Inertial Signals/*.txt"))
unlink(file.path(path, "test", "Inertial Signals/*.txt"))
(list.files(path, recursive = TRUE))
# [1] "activity_labels.txt"     "features_info.txt"      
# [3] "features.txt"            "README.txt"             
# [5] "test/subject_test.txt"   "test/X_test.txt"        
# [7] "test/y_test.txt"         "train/subject_train.txt"
# [9] "train/X_train.txt"       "train/y_train.txt"    



# loads the subject, activity, and measurement train and test sets with corresponding n=7352 and n=2947 observations and reads them into data frames
s.train.s <- read.table(file.path(path, "train", "subject_train.txt"))   # [s]ubject [train] [s]et;         > dim(s.train.s) [1] 7352    1
s.test.s <- read.table(file.path(path, "test", "subject_test.txt"))     # [s]ubject [test] [s]et;               > dim(s.test.s) [1] 2947    1
a.train.s <- read.table(file.path(path, "train", "Y_train.txt"))     # [a]ctivity [train] [s]et;             > dim(a.train.s) [1] 7352    1
a.test.s <- read.table(file.path(path, "test", "Y_test.txt"))        # [a]ctivity [test] [s]et;              > dim(a.test.s) [1] 2947    1
m.train.s <- read.table(file.path(path, "train", "X_train.txt"))     # [m]easuremnts [train] [s]et;          > dim(m.train.s) [1] 7352  561
m.test.s <- read.table(file.path(path, "test", "X_test.txt"))            # [m]easuremnts [test] [s]et;           > dim(m.test.s) [1] 2947  561



# 1. Merges the training and the test sets to create one data set for subjects, activities and measurements, each.
subjects <- rbind(s.train.s, s.test.s)      # subjects
setnames(subjects, "V1", "subject")
activities <- rbind(a.train.s, a.test.s)    # activities
setnames(activities, "V1", "activityNum")
measurements <- rbind(m.train.s, m.test.s)  # measurements
# features, "the colum names vector" is assigned as colnames after extraction from the archived "features.txt" source file
features <- read.table(file.path(path, "features.txt"))[,"V2"]
colnames(measurements) <- features

# subjects + activities -> factors
factors <- cbind(subjects, activities)
# factors + measurement -> complete / full data set
one.dset <- cbind(factors, measurements)    # [o]ne [d]ata[set]
# one data set [c.DT] was created as per step 1 of the assignment 
c.DT <- data.table(one.dset)   #  [c]omplete [D]ata [T]able
# > dim(c.DT) [1] 10299   563


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#   factor vnames is created to select the variables of interest [mean and std]; the reduced data set of s.DT (with 10299 observations and 68 variables, i.e. selected variables) is generated
vnames <- features[grepl("mean\\(\\)|std\\(\\)", features)]
keep <- c("subject", "activityNum", as.character(vnames))

s.DT <- c.DT[, keep, with = FALSE]  # [s]elected [D]ata [T]able
setnames(s.DT, names(s.DT), c("subject", "activityNum", as.character(vnames)))        
# > dim(s.DT) [1] 10299    68



# 3. Uses descriptive activity names to name the activities in the data set
#   the factor "activity" was extracted from the "activity_labels" source file and added as an additional variable merging the factor into the s.DT data table using the activity number "activityNum" represented in both data frames for merging
a.names <- read.table(file.path(path, "activity_labels.txt"))
setnames(a.names, names(a.names), c("activityNum", "activity"))
# the result is the new, updated s.DT (with 10299 observations and 69 variables). Information regarding the recorded activities is represented as a "numeric vector" - "activityNum" and a "charcter vector" - "activity" in the updated data set.
s.DT <- merge(s.DT, a.names, by = "activityNum", all.x = TRUE)
setkey(s.DT, subject, activityNum, activity)
#   > dim(s.DT) [1] 10299    69

# the columns of s.DT are reordered to represent subject, activity number,  activity (i.e. a descriptive activity name) and 66 measurement variables; DT - the "one data set" with 10299 observations and 69 variables is created. 
DT <- setcolorder(s.DT, c("subject", "activityNum", "activity", as.character(vnames)))
#   > dim(DT) [1] 10299    69



# 4. Appropriately labels the data set with descriptive variable names.
# a character vector v.L with length representing the 69 variable labels is generated
v.L <- colnames(DT)     # [v]ariable [L]abels
#   "activityNum" is renamed as "activity index"
v.L[2] <- "activity index"

#   Descriptive names are generated by substituting character strings of the variable names; the applied conventions are described in the data dictionary.  
for (i in 1:length(v.L)) 
{
  v.L[i] = gsub("\\()","",v.L[i])
  v.L[i] = gsub("\\-","\\,",v.L[i])
  v.L[i] = gsub("Y$", " Vertical Aspect", v.L[i])
  v.L[i] = gsub("X$", " Lateral Aspect", v.L[i])
  v.L[i] = gsub("Z$", " Antero-Posterior Aspect", v.L[i])
  v.L[i] = gsub("std"," StdDev",v.L[i])
  v.L[i] = gsub("mean"," Mean",v.L[i])
  v.L[i] = gsub("^(t)","Time domain: ",v.L[i])
  v.L[i] = gsub("^(f)","Frequency domain: ",v.L[i])
  v.L[i] = gsub("Mag"," Size",v.L[i])
  v.L[i] = gsub("Jerk"," Jolt",v.L[i])
  v.L[i] = gsub("Gyro"," Orientation",v.L[i])
  v.L[i] = gsub("Gravity"," Gravity component of",v.L[i])
  v.L[i] = gsub("Acc"," Acceleration",v.L[i])
  v.L[i] = sub("BodyBody"," Body",v.L[i])
  v.L[i] = gsub("Antero,Posterior","Antero-Posterior",v.L[i])
  v.L[i] = gsub("Time,domain","Time domain",v.L[i])
}
# The new descriptive column names are reassigned to the DT data set. 
setnames(DT, names(DT), v.L)



# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyDT <- aggregate(. ~ subject + activity, data=DT, FUN = "mean")
#   > dim(tidyDT) [1] 180  69

#   Export the second independent tidy dataset "tidyDT" as "tidy.txt" file using the write.table function
write.table(tidyDT, file.path(path, "tidy.txt"), sep = "\t", row.names = FALSE, quote = FALSE)


