library(dplyr)
library(tidyr)

helper.getext <- function (dir){
  fileList = list.files(dir)
  
  setwd(dir)
  
  fileName = ''
  for (file in fileList){
    if (file_test('-f', file)){
      fileName = strsplit(file, '_')
      break
    }
  }
  
  setwd('..')
  
  return(fileName[[1]][length(fileName[[1]])])
}

helper.getfilenamearray <- function (fileName){
  nameArray <- strsplit(fileName, '_')[[1]]
  return(nameArray)
}

helper.getfilename <- function (fileArray, ext){
  for (str in fileArray){
    if (str == fileArray[1])
      fileName = str
    else if (str == fileArray[length(fileArray)])
      fileName = paste(fileName, ext, sep = '_')
    else
      fileName = paste(fileName, str, sep = '_')
  }
  return(fileName)
}

helper.combinedatasets <- function(dir1 = 'test', dir2 = 'train', outDir = 'total', subdir = ''){
  
  root = paste(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 'Pessoais/Data Scientist Specialization/Project 03/Execucao/UCI HAR Dataset', sep = '/')
  
  ext1 = helper.getext(dir1)
  ext2 = helper.getext(dir2)
  extOut = outDir
  
  if (subdir != '') {
    dir1 = paste(dir1, subdir, sep = '/')
    dir2 = paste(dir2, subdir, sep = '/')
    outDir = paste(outDir, subdir, sep = '/')
  }
  
  if (!file.exists(outDir))
    dir.create(outDir)
  
  fileList1 = list.files(dir1)
  for (fileObj in fileList1){
    if (file_test('-f', paste(dir1, fileObj, sep = '/'))){
      
      setwd(dir1)
      dataset1 <- data.frame(read.table(helper.getfilename(helper.getfilenamearray(fileObj), ext1), stringsAsFactors=F))
      setwd(root)
      
      setwd(dir2)
      dataset2 <- data.frame(read.table(helper.getfilename(helper.getfilenamearray(fileObj), ext2), stringsAsFactors=F))
      setwd(root)
      
      dataset1 <- rbind(dataset1, dataset2, use.names=T, fill=F)
      
      setwd(outDir)
      write.table(dataset1, file = helper.getfilename(helper.getfilenamearray(fileObj), paste(extOut, 'txt', sep = '.')), sep=" ", row.names=F, col.names=F)
      setwd(root)
      
    } else if (file_test('-d', paste(dir1, fileObj, sep = '/'))){
      
      if (subdir == '') subdir = fileObj
      else subdir = paste(subdir, fileObj, sep = '/')
      
      helper.combinedatasets(dir1, dir2, extOut, subdir)
      
    }
  }
}

setwd('UCI HAR Dataset')
helper.combinedatasets()

rm(helper.combinedatasets)
rm(helper.getext)
rm(helper.getfilename)
rm(helper.getfilenamearray)

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

total.tidyset <- total.dataset %>% 
  group_by(activity, subject) %>% 
  summarise_each(funs(mean))

total.tidyset <- total.tidyset %>% 
  rowwise() %>% 
  mutate(activity.mean = mean(tBodyAcc.mean...X:angle.Z.gravityMean.)) %>% 
  select(activity, subject, activity.mean)

View(total.tidyset)