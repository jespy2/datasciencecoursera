best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ##Reading in csv file and assigning column classes for efficiency
  outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##pulling the state abbreviations in column 7 for reference
  evalState <- unique(outcomefile[,7])
  ##confirming that "state" input is one of the abbreviations used in column 7.
  ##Populates outcomefile with data from requested state only.
  ##Else, stopping and printing error message
  if (! (state %in% evalState)) stop("invalid state")
  outcomefile <-subset(outcomefile, outcomefile[,7]==state)
  
  ##Setting x to hold the outcome requested.  
  x <- if(outcome=="heart attack") {outcomefile[,c(2,11)]}
  else if(outcome=="heart failure") {outcomefile[,c(2,17)]}
  else if(outcome=="pneumonia") {outcomefile[,c(2,23)]}
  ##if outcome something other than the three conditions expected, stops and prints error message
  else stop("invalid outcome")
  
  ##remove NAs and assign cleaned data to x
  suppressWarnings( x[,2]<-as.numeric(x[,2]) )
  x<-subset(x,!is.na(x[,2]))
  
  ##Sorts data in x by requested outcome, then by hospital name.  Then prints first hospital in set. 
  print(head(x[order(x[,2],x[,1]),1],1))
}