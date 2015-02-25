#getwd()
#setwd("C:/Users/Gulsevi/Desktop/Coursera/Assignment3_RProg")
rankhospital <- function(state, outcome, num="best") {
  library(plyr)
  library (dplyr)
  ## Read outcome data
  myOutcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  necessaryVars <- select (myOutcomes, c(2,7,11,17,23))
  names (necessaryVars) <- c("Hospital Name","State","heart attack","heart failure","pneumonia") 
  possible_outcome <- names(necessaryVars[3:5])
  ## Check that state and outcome are valid
  check_term_state = paste("^",state, sep="")
  check_term_outcome = paste("^",outcome, sep="")
  
  is_include_state <- sum(as.logical(grep (check_term_state, as.vector(necessaryVars$State))))
  is_include_outcome <- sum(as.logical(grep (check_term_outcome, possible_outcome)))
 
  if (is_include_state == 0){stop('invalid state')}
  if (is_include_outcome == 0){stop('invalid outcome')}
  
  ## Return hospital name in that state with lowest 30-day death
  subset_state <- necessaryVars [which (necessaryVars$State == state),]
  subset_state <- subset_state [which (subset_state[outcome] != "Not Available"),]
  sortedFileAscending <- subset_state [order(as.numeric(subset_state [,outcome]),subset_state[["Hospital Name"]]),]
  
  if (num == "best"){
    hospital_name = sortedFileAscending [1,1]
  }else if (num == "worst"){
    hospital_name = sortedFileAscending [nrow(subset_state),1]
  }else {
    if (num > nrow(subset_state)){hospital_name = NA}
    else{hospital_name = sortedFileAscending[num,1]}
  }
    
  ## rate
  hospital_name
}

#rankhospital("TX", "heart failure", 4)
#rankhospital("MD", "heart attack", "worst")
#rankhospital("MN", "heart attack", 5000)




