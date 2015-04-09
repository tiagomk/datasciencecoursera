best <- function(state, outcome) {
  data <- read.csv("dataset/outcome-of-care-measures.csv", colClasses = "character")
  
  nHospitals <- sum(data[,'State']==state)
  if (nHospitals == 0)
    stop('invalid state')
  hospitals <- data[data[,'State']==state,]
  
  jCol = numeric()
  if (outcome == 'heart attack')
    jCol = 11
  else if (outcome == 'heart failure')
    jCol = 17
  else if (outcome == 'pneumonia')
    jCol = 23
  else
    stop('invalid outcome')
  
  hospitals[,c(jCol)] <- as.numeric(hospitals[,c(jCol)])
  
  minValue = 99999
  for (r in 1:nrow(hospitals)){
    if (!is.na(hospitals[r, c(jCol)]) & hospitals[r, c(jCol)] < minValue)
      minValue = hospitals[r, c(jCol)]
  }
  
  bestHospitals = hospitals[hospitals[, c(jCol)] == minValue,]
  
  normalizedHospitals = bestHospitals[!is.na(bestHospitals[, c('Hospital.Name')]),]
  sort(normalizedHospitals[, c('Hospital.Name')])[1]
}