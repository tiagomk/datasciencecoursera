# Checks if the required packages are installed:
# 1. dplyr
# 2. tidyr
# If they are not, then install them.
if ("dplyr" %in% rownames(installed.packages()) == FALSE)
  install.packages("dplyr")
if ("tidyr" %in% rownames(installed.packages()) == FALSE)
  install.packages("tidyr")

# These commands load the librarys dplyr and tidyr
# for data manipulation.
library(dplyr)
library(tidyr)

# This helper counts the number of times a character
# appears inside a string.
helper.countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}

# This helper function combines the data set of the 
# filename from both test and train, merging them
# in a total set.
helper.combinedatasets <- function(filename){
  
  # Sets the appropriate filenames for each data set
  testName <- paste(filename, 'test.txt', sep='_')
  trainName <- paste(filename, 'train.txt', sep='_')
  totalName <- paste(filename, 'total.txt', sep='_')
  
  # Sets the directory name of each data set
  dirTest <- 'UCI HAR Dataset/test'
  dirTrain <- 'UCI HAR Dataset/train'
  dirTotal <- 'UCI HAR Dataset/total'
  
  # Sets the full path of each file
  testPath <- paste(dirTest, testName, sep='/')
  trainPath <- paste(dirTrain, trainName, sep='/')
  totalPath <- paste(dirTotal, totalName, sep='/')
  
  # Counts the subdirectory level below 'root'
  subDirCount <- helper.countCharOccurrences('/', dirTotal) + 1
  
  # Reads the filename from the test data set
  setwd(dirTest)
  dataset1 <- data.frame(
    read.table(
      paste(
        filename,
        'test.txt',
        sep='_'
      )
    ), 
    stringsAsFactors=F
  )
  for (i in 1:subDirCount)
    setwd('..')
  
  # Reads the filename from the train data set
  setwd(dirTrain)
  dataset2 <- data.frame(
    read.table(
      paste(
        filename,
        'train.txt',
        sep='_'
      )
    ), 
    stringsAsFactors=F
  )
  for (i in 1:subDirCount)
    setwd('..')
  
  # Merges both test and train data sets
  dataset1 <- rbind(dataset1, dataset2, use.names=T, fill=F)
  
  # Writes the filename for the total data set
  setwd(dirTotal)
  write.table(
    dataset1, 
    file = paste(filename, 'total.txt', sep='_'), 
    sep=" ", 
    row.names=F, 
    col.names=F
  )
  for (i in 1:subDirCount)
    setwd('..')
}

# Creates the total data set directories (fires no warnings)
dir.create('UCI HAR Dataset/total', showWarnings = FALSE)
dir.create('UCI HAR Dataset/total/Inertial Signals', showWarnings = FALSE)

# Combines all data sets
helper.combinedatasets('subject')
helper.combinedatasets('X')
helper.combinedatasets('y')
helper.combinedatasets('Inertial Signals/body_acc_x')
helper.combinedatasets('Inertial Signals/body_acc_y')
helper.combinedatasets('Inertial Signals/body_acc_z')
helper.combinedatasets('Inertial Signals/body_gyro_x')
helper.combinedatasets('Inertial Signals/body_gyro_y')
helper.combinedatasets('Inertial Signals/body_gyro_z')
helper.combinedatasets('Inertial Signals/total_acc_x')
helper.combinedatasets('Inertial Signals/total_acc_y')
helper.combinedatasets('Inertial Signals/total_acc_z')

# Removes helper functions
rm(helper.countCharOccurrences)
rm(helper.combinedatasets)

# Goes to data directory
setwd('UCI HAR Dataset')

# Reads and sets dataframe for activity's labels
total.activities <- read.table('activity_labels.txt', col.names = c('id', 'name'))
total.activities <- tbl_df(total.activities)

# Reads and sets dataframe for features
total.features <- read.table('features.txt', col.names = c('id', 'name'))
total.features <- tbl_df(total.features)

# Goes to total directory
setwd('total')

# Reads and sets dataframe for total set of subjects
total.subject <- read.table('subject_total.txt', col.names = c('subject'))
total.subject <- tbl_df(total.subject)

# Reads and sets dataframe for total set of y
total.y <- read.table('y_total.txt', col.names = c('y'))
total.y <- tbl_df(total.y)

# Reads and sets dataframe for total set of X
total.x <- read.table('X_total.txt', col.names = total.features$name)
total.x <- tbl_df(total.x)

# Goes back to main directory
setwd('..');setwd('..')

# Merges and sets the dataframe for the total dataset
total.dataset <- cbind(total.subject, total.y, total.x)
total.dataset <- tbl_df(total.dataset)

# Removes already used dataframes (they where already merged)
rm(total.subject)
rm(total.y)
rm(total.x)

# Gets through regex only the features which have 'mean' or 'std'
# in its name
total.dataset <- 
  total.dataset %>% 
    select(subject, y, total.features$id[grep('([Mm]ean|[Ss]td)', total.features$name)] + 2)

# Removes dataframe with features (was already used)
rm(total.features)

# Merges the tota dataset with the activities dataframe
total.dataset <- merge(total.activities, total.dataset, by.x='id', by.y='y')

# Sets name for the second column as 'activity'
names = colnames(total.dataset)
names[2] = 'activity'
colnames(total.dataset) = names

# Dispose of unused objects
rm(names)
rm(total.activities)

# Selects and sets the total dataset in an arranged dataset
# (the total.dataset will remain in memory)
total.dataset <- total.dataset %>% select(2:length(colnames(total.dataset)))
total.dataset <- total.dataset %>% arrange(activity, subject)

# Groups the total data set by activity and subject
# and then summarises the data with the mean value
total.tidyset <- total.dataset %>% 
  group_by(activity, subject) %>% 
  summarise_each(funs(mean))

# Calculates the mean value for each row.
# adds a new column with the calculated activities mean
# and selects the activity, subject and activity.mean columns
total.tidyset <- total.tidyset %>% 
  rowwise() %>% 
  mutate(activity.mean = mean(tBodyAcc.mean...X:angle.Z.gravityMean.)) %>% 
  select(activity, subject, activity.mean)

# Displays the total tidy set
View(total.tidyset)
# Prints the total tidy set
print(total.tidyset)
# Saves tidy dataset to root directory
write.table(total.tidyset, 'tidyset.txt', row.name=FALSE)
