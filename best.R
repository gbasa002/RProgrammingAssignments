#getwd()
#setwd("C:/Users/Gulsevi/Desktop/Coursera/Assignment3_RProg")
best <- function(state, outcome) {
  library(plyr)
  library (dplyr)
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  necessaryFile <- select (file, c(2,7,11,17,23))
  names (necessaryFile) <- c("Hospital Name","State","heart attack","heart failure","pneumonia")
  #names(necessaryFile)
  possible_outcome <- names(necessaryFile[3:5])
  ## Check that state and outcome are valid
  check_term_state = paste("^",state, sep="")
  check_term_outcome = paste("^",outcome, sep="")
  
  is_include_state <- sum(as.logical(grep (check_term_state, as.vector(necessaryFile$State))))
  is_include_outcome <- sum(as.logical(grep (check_term_outcome, possible_outcome)))
 
  #my_err_msg_state <- paste('Error in best','("',state, '")', ":invalid state", sep = "")
  #my_err_msg_outcome <- paste('Error in best','("',outcome, '")', ":invalid state", sep = "")
  
  if (is_include_state == 0){stop('invalid state')}
  if (is_include_outcome == 0){stop('invalid outcome')}
  
  ## Return hospital name in that state with lowest 30-day death
  subset_state <- necessaryFile [which (necessaryFile$State == state),]
  subset_state <- subset_state [which (subset_state[outcome] != "Not Available"),]
  sortedFile <- arrange(subset_state, as.numeric(subset_state [,outcome]))
  hospital_name = sortedFile[1,1]
  
  ## rate
  hospital_name
}
#best("TX", "heart attack")
#best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")
#best("BB", "heart attack")
#best("NY", "hert attack")

