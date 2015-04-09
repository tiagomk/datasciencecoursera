getfilepath <- function (directory, id){
  name = id
  if (id < 10)
    name = paste("00", id, sep = "")
  else if (10 <= id && id < 100)
    name = paste("0", id, sep = "")
  
  filename = paste(name, "csv", sep = ".")
  
  paste(directory, filename, sep = "/")
}

pollutantmean <- function(directory, pollutant, id = 1:332) {

  oxide = array()
  for (i in id){
    data = read.table(getfilepath(directory, i), header = T, sep = ',')
    oxide = c(oxide, data[,c(pollutant)])
  }
  
  mean(oxide, na.rm = T)
  
}