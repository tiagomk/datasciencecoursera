source("complete.R")

getfilepath <- function (directory, id){
  name = id
  if (id < 10)
    name = paste("00", id, sep = "")
  else if (10 <= id && id < 100)
    name = paste("0", id, sep = "")
  
  filename = paste(name, "csv", sep = ".")
  
  paste(directory, filename, sep = "/")
}

corr <- function(directory, threshold = 0) {
  aCorr = array()
  nobs = complete(directory)
  for (i in 1:nrow(nobs)){
    if (nobs$nobs[i] > threshold){
      data = read.table(getfilepath(directory, i), header = T, sep = ',')
      aCorr = rbind(aCorr, cor(data$sulfate, data$nitrate, use = 'pairwise.complete.obs'))
    }
  }
  aCorr
}