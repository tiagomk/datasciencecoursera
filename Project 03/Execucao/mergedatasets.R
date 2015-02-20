#
# Purpose:
#
#   Gets the last substring containg the data set
#   and extension.
#
# Arguments:
#
#   dir (String) - directory containg the dataset.
#
# Return:
#
#   ext (String) - dataset name with file extension.
#
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
merge <- function(dir1 = 'test', dir2 = 'train', outDir = 'total', subdir = ''){
  
  root = paste(file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\"), 'Data Scientist Specialization/Project 03/Execucao/UCI HAR Dataset', sep = '/')
  
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
      dataset1 <- data.table(read.table(getFile(getFileArray(fileObj), ext1), stringsAsFactors=F))
      setwd(root)
      
      setwd(dir2)
      dataset2 <- data.table(read.table(getFile(getFileArray(fileObj), ext2), stringsAsFactors=F))
      setwd(root)
      
      dataset1 <- rbind(dataset1, dataset2, use.names=T, fill=F)
      
      setwd(outDir)
      write.csv2(dataset1, file = getFile(getFileArray(fileObj), paste(extOut, 'txt', sep = '.')))
      setwd(root)
      
    } else if (file_test('-d', paste(dir1, fileObj, sep = '/'))){
      
      if (subdir == '') subdir = fileObj
      else subdir = paste(subdir, fileObj, sep = '/')
      
      merge(dir1, dir2, extOut, subdir)
      
    }
  }    
}
