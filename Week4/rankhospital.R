rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  ##Reading in csv file
  outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available" )

  ##pulling the state abbreviations in column 7 for reference
  evalState <- unique(outcomefile[,7])
  ##confirming that "state" input is one of the abbreviations used in column 7.
  ##Populates outcomefile with data from requested state only.
  ##Else, stopping and printing error message
  if (! (state %in% evalState)) stop("invalid state")
  outcomefile <-subset(outcomefile, outcomefile[,7]==state)

  ##Setting outcome columns to numeric for sorting 
  outcomefile[,11]<-as.numeric(as.character(outcomefile[,11]))
  outcomefile[,17]<-as.numeric(as.character(outcomefile[,17]))
  outcomefile[,23]<-as.numeric(as.character(outcomefile[,23]))
  
  ##Setting x to hold the outcome requested.  
  x <- if(outcome=="heart attack") {outcomefile[,c(2,11)]}
  else if(outcome=="heart failure") {outcomefile[,c(2,17)]}
  else if(outcome=="pneumonia") {outcomefile[,c(2,23)]}
  ##if outcome something other than the three conditions expected, stops and prints error message
  else stop("invalid outcome")

  ##remove NAs and assign cleaned data to x
  y<-complete.cases(x)
  x<-x[y,]

  ##Sorts data in x by requested outcome, then by hospital name.   
  x<-x[order(x[,2],x[,1]),1]

  ##Evaluates x by requested num, then prints result
  x<- if(num=="best") {head(x,1)}
  else if(num=="worst") {tail(x,1)}
  else x[num]
  print(x)
}