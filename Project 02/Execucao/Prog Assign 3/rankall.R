#library(dplyr)

rankall <- function(outcome, num = "best") {
  data <- read.csv("dataset/outcome-of-care-measures.csv", colClasses = "character")
  
  jCol = numeric()
  if (outcome == 'heart attack')
    jCol = 11
  else if (outcome == 'heart failure')
    jCol = 17
  else if (outcome == 'pneumonia')
    jCol = 23
  else
    stop('invalid outcome')
  
  states = sort(unique(data[,'State']))
  
  rankedHospitals = matrix(ncol = 2, nrow = length(states))
  
  for (s in 1:length(states)){
    #hospitals <- tbl_df(data)
    hospitals <- data[data[,'State']==states[s],]
    hospitals[,c(jCol)] <- as.numeric(hospitals[,c(jCol)])
    return(hospitals)
    orderedHospitals <- order(hospitals[,c(jCol)], hospitals[,'Hospital.Name'])
    #orderedHospitals <- hospitals %>% filter(State == states[s]) %>% select(Hospital.Name, jCol)
    
    notFound <- F
    if (num == 'best') num <- 1
    if (num == 'worst')
      for (r in nrow(hospitals):1)
        #if (is.na(hospitals[orderedHospitals,c(jCol)][r])){
          num <- r
          break;
        #}
    if (num > nrow(hospitals)) notFound <- T
    
    if (notFound == T) rankedHospitals[s, 1] <- NA
    else
      rankedHospitals[s, 1] <- hospitals[orderedHospitals, 'Hospital.Name'][num]
    rankedHospitals[s, 2] <- states[s]
  }
  
  rankedHospitals = data.frame(rankedHospitals)
  colnames(rankedHospitals) <- c('hospital', 'state')

  return(rankedHospitals)
}
