library(dplyr)
library(tidyr)

helper.countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}

helper.combinedatasets <- function(filename){
  
  testName <- paste(filename, 'test.txt', sep='_')
  trainName <- paste(filename, 'train.txt', sep='_')
  totalName <- paste(filename, 'total.txt', sep='_')
  
  dirTest <- 'UCI HAR Dataset/test'
  dirTrain <- 'UCI HAR Dataset/train'
  dirTotal <- 'UCI HAR Dataset/total'
  
  testPath <- paste(dirTest, testName, sep='/')
  trainPath <- paste(dirTrain, trainName, sep='/')
  totalPath <- paste(dirTotal, totalName, sep='/')
  
  subDirCount <- helper.countCharOccurrences('/', dirTotal) + 1
  
  setwd(dirTest)
  dataset1 <- data.frame(read.table(paste(filename, 'test.txt', sep='_')), stringsAsFactors=F)
  for (i in 1:subDirCount)
    setwd('..')
  
  setwd(dirTrain)
  dataset2 <- data.frame(read.table(paste(filename, 'train.txt', sep='_')), stringsAsFactors=F)
  for (i in 1:subDirCount)
    setwd('..')
  
  dataset1 <- rbind(dataset1, dataset2, use.names=T, fill=F)
  
  setwd(dirTotal)
  write.table(dataset1, file = paste(filename, 'total.txt', sep='_'), sep=" ", row.names=F, col.names=F)
  for (i in 1:subDirCount)
    setwd('..')
}
dir.create('UCI HAR Dataset/total', showWarnings = FALSE)
dir.create('UCI HAR Dataset/total/Inertial Signals', showWarnings = FALSE)

#helper.combinedatasets('subject')
#helper.combinedatasets('X')
#helper.combinedatasets('y')
#helper.combinedatasets('Inertial Signals/body_acc_x')
#helper.combinedatasets('Inertial Signals/body_acc_y')
#helper.combinedatasets('Inertial Signals/body_acc_z')
#helper.combinedatasets('Inertial Signals/body_gyro_x')
#helper.combinedatasets('Inertial Signals/body_gyro_y')
#helper.combinedatasets('Inertial Signals/body_gyro_z')
#helper.combinedatasets('Inertial Signals/total_acc_x')
#helper.combinedatasets('Inertial Signals/total_acc_y')
#helper.combinedatasets('Inertial Signals/total_acc_z')

rm(helper.countCharOccurrences)
rm(helper.combinedatasets)

setwd('UCI HAR Dataset')
total.activities <- read.table('activity_labels.txt', col.names = c('id', 'name'))
total.activities <- tbl_df(total.activities)

total.features <- read.table('features.txt', col.names = c('id', 'name'))
total.features <- tbl_df(total.features)

setwd('total')

total.subject <- read.table('subject_total.txt', col.names = c('subject'))
total.subject <- tbl_df(total.subject)

total.y <- read.table('y_total.txt', col.names = c('y'))
total.y <- tbl_df(total.y)

total.x <- read.table('X_total.txt', col.names = total.features$name)
total.x <- tbl_df(total.x)

setwd('..');setwd('..')

total.dataset <- cbind(total.subject, total.y, total.x)
total.dataset <- tbl_df(total.dataset)

rm(total.subject)
rm(total.y)
rm(total.x)

total.dataset <- 
  total.dataset %>% 
    select(subject, y, total.features$id[grep('([Mm]ean|[Ss]td)', total.features$name)] + 2)

rm(total.features)

total.dataset <- merge(total.activities, total.dataset, by.x='id', by.y='y')
names = colnames(total.dataset)
names[2] = 'activity'
colnames(total.dataset) = names

rm(names)
rm(total.activities)

total.dataset <- total.dataset %>% select(2:length(colnames(total.dataset)))
total.dataset <- total.dataset %>% arrange(activity, subject)

total.tidyset <- total.dataset %>% 
  group_by(activity, subject) %>% 
  summarise_each(funs(mean))

total.tidyset <- total.tidyset %>% 
  rowwise() %>% 
  mutate(activity.mean = mean(tBodyAcc.mean...X:angle.Z.gravityMean.)) %>% 
  select(activity, subject, activity.mean)

View(total.tidyset)