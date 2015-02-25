rankall <- function(outcome, num = "best") {
  ## Read outcome data
  library(plyr)
  library (dplyr)
  ## Read outcome data
  myOutcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  necessaryVars <- select (myOutcomes, c(2,7,11,17,23))
  rm(myOutcomes)
  names (necessaryVars) <- c("Hospital Name","State","heart attack","heart failure","pneumonia") 
  possible_outcome <- names(necessaryVars[3:5])
  ## Check that state and outcome are valid 
  is_include_outcome <- sum(as.logical(grep (paste("^",outcome, sep=""), possible_outcome)))
  if (is_include_outcome == 0){stop('invalid outcome')}  
  ## For each state, find the hospital of the given rank
  subset_state <- data.frame (necessaryVars$State)
  subset_state <- data.frame(lapply (subset_state, unique))
  names(subset_state) <- "State"
  subset_state <- data.frame(subset_state[order(subset_state$State),])
  
  hospitals <- vector ()
  
  for (i in 1:nrow(subset_state)){
      state_based_subset <- necessaryVars[which (necessaryVars$State == subset_state[i,1]), ]
      state_based_subset <- state_based_subset [which (state_based_subset[outcome] != "Not Available"),]
      sorted_state_based <- state_based_subset [order(as.numeric(state_based_subset [,outcome]),state_based_subset[["Hospital Name"]]),]
     
      if (num == "best"){
        hospital_name = sorted_state_based [1,1]
      }else if (num == "worst"){
        hospital_name = sorted_state_based [nrow(sorted_state_based),1]
      }else {
        if (num > nrow(sorted_state_based)){hospital_name = NA}
        else{hospital_name = sorted_state_based[num,1]}
      }
      hospitals = c(hospitals, hospital_name)
  }
   
    final <- cbind (hospitals, subset_state)
    names (final) <- c("hospital", "state")
    row.names(final) <- final$state
    final
}

#head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia", "worst"), 3)
#tail(rankall("heart failure"), 10)



