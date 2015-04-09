rankhospital <- function(state, outcome, num = "best") {
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
  
  orderedHospitals <- order(hospitals[,c(jCol)], hospitals[,'Hospital.Name'], na.last = NA)
  
  if (num == 'best') num = 1
  if (num == 'worst') num = length(orderedHospitals)
  if (num > length(orderedHospitals)) return(NA)
  
  hospitals[orderedHospitals,'Hospital.Name'][num]
}