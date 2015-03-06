getExt <- function (dir){
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
  
  fileName[[1]][length(fileName[[1]])]
}

#
# Purpose:
#
#   Gets the array containg the substrings of the file
#   name, which were separated by '_' character.
#
# Arguments:
#
#   fileName (String) - file name.
#
# Return:
#
#   nameArray (array<String>) - array containing file name
#     substrings.
#
getFileArray <- function (fileName){
  nameArray <- strsplit(fileName, '_')[[1]]
  nameArray
}

#
# Purpose:
#
#   Gets only the file name without dataset and extension
#     substring and applies the proper extension.
#
# Arguments:
#
#   fileArray (array<String>) - array containing file name
#     substrings.
#   ext (String) - name of the dataset and extension to apply
#     on file name.
#
# Return:
#
#   fileName (String) - name of the file with proper dataset
#     and extension names.
#
getFile <- function (fileArray, ext){
  for (str in fileArray){
    if (str == fileArray[1])
      fileName = str
    else if (str == fileArray[length(fileArray)])
      fileName = paste(fileName, ext, sep = '_')
    else
      fileName = paste(fileName, str, sep = '_')
  }
  fileName
}

#
# Purpose:
#
#   Merges two different dataset into a final dataset.
#
# Arguments:
#
#   dir1 (String) - name of directory containing the
#     first dataset.
#   dir2 (String) - name of directory containing the
#     second dataset.
#   outDir (String) - name of directory where to output
#     merged dataset.
#
# Return:
#
#   void.
#
mergeDatasets <- function(dir1 = 'test', dir2 = 'train', outDir = 'total', subdir = ''){
  
  root = paste(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 'Pessoais/Data Scientist Specialization/Project 03/Execucao/UCI HAR Dataset', sep = '/')
  
  ext1 = getExt(dir1)
  ext2 = getExt(dir2)
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
      dataset1 <- data.frame(read.table(getFile(getFileArray(fileObj), ext1), stringsAsFactors=F))
      setwd(root)
      
      setwd(dir2)
      dataset2 <- data.frame(read.table(getFile(getFileArray(fileObj), ext2), stringsAsFactors=F))
      setwd(root)
      
      dataset1 <- rbind(dataset1, dataset2, use.names=T, fill=F)
      
      setwd(outDir)
      write.csv2(dataset1, file = getFile(getFileArray(fileObj), paste(extOut, 'txt', sep = '.')), sep=" ", row.names=F)
      setwd(root)

    } else if (file_test('-d', paste(dir1, fileObj, sep = '/'))){
      
      if (subdir == '') subdir = fileObj
      else subdir = paste(subdir, fileObj, sep = '/')
      
      mergeDatasets(dir1, dir2, extOut, subdir)
      
    }
  }
}

run <- function(){

  #root = paste(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 'Pessoais/Data Scientist Specialization/Project 03/Execucao/UCI HAR Dataset', sep = '/')
  #setwd(root)
  #mergeDatasets()
  #setwd('..')
  
  library(dplyr)
  library(tidyr)
  
  total.subject <- read.table('UCI HAR Dataset/total/subject_total.txt', header=T)
  colnames(total.subject) <- c('subject')
  total.subject <- tbl_df(total.subject)
  
  #total.x <- read.table('UCI HAR Dataset/total/X_total.txt', header=T)
  #colnames(total.x) <- paste('x', 1:(ncol(total.x)-1), sep='_')
  #total.x <- tbl_df(total.x)
  
  total.y <- read.table('UCI HAR Dataset/total/y_total.txt', header=T)
  colnames(total.y) <- c('y')
  total.y <- tbl_df(total.y)
  
  total.is.bd_acc_x <- read.table('UCI HAR Dataset/total/Inertial Signals/body_acc_x_total.txt', header=T)
  colnames(total.is.bd_acc_x) <- paste('bodyAcc-X', 1:(ncol(total.is.bd_acc_x)-1), sep='_')
  total.is.bd_acc_x <- tbl_df(total.is.bd_acc_x)
  
  #df <- merge(total.subject, total.y, by='id')
  
  df <- cbind(total.subject, total.y)
  
  #nrows <- nrow(test.subject)
  #ncols <- ncol(test.is.bd_acc_x)
  #ntotal <- nrows * ncols
  
  #df <- data.frame(subject = numeric(ntotal), bd_acc_x = numeric(ntotal))
  #for (i in 1:nrows){
  #  for (j in 1:ncols){
  #    df$subject[(i - 1) * ncols + j] = test.subject[i,1]
  #    df$bd_acc_x[(i - 1) * ncols + j] = test.is.bd_acc_x[i,j]
  #  }
  #}
  df
}