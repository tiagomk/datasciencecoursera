getfilepath <- function (directory, id){
  name = id
  if (id < 10)
    name = paste("00", id, sep = "")
  else if (10 <= id && id < 100)
    name = paste("0", id, sep = "")
  
  filename = paste(name, "csv", sep = ".")
  
  paste(directory, filename, sep = "/")
}

complete <- function(directory, id = 1:332) {
  
  df <- data.frame(id=integer(length(id)), nobs=integer(length(id)))
  r = 1
  for (i in id){
    data = read.table(getfilepath(directory, i), header = T, sep = ',')
    df$id[r] = i
    df$nobs[r] = sum(!is.na(data$nitrate) & !is.na(data$sulfate))
    r <- r + 1
  }
  df
}