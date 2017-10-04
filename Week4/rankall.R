rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  ##Confirms that input for outcome is valid
  condition <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% condition){
    stop("invalid outcome")
  }
  
  if (outcome=="heart attack"){
    outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")[,c(2,7,11)]
    names(outcomefile)[3] <- "illness"
  }
  
  if (outcome=="heart failure"){
    outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")[,c(2,7,17)]
    names(outcomefile)[3] <- "illness"
  }
  
  if (outcome=="pneumonia"){
    outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")[,c(2,7,23)]
    names(outcomefile)[3] <- "illness"
  }
  
 names(outcomefile)[1] <- "Hospital" 
 ##Setting outcome columns to numeric for sorting 
 outcomefile[,3]<-as.numeric(as.character(outcomefile[,3]))
 
 ##removing NAs
 x<-complete.cases(outcomefile)
 outcomefile<-outcomefile[x,]

 splittable = split(outcomefile, outcomefile$State)
 sorttable = lapply(splittable, function(y,num) {
  ##sort by illness then hospital
   y = y[order(y$illness, y$Hospital),]
   
   if (num == "best") {
     return(y$Hospital[1])
   }
   else if (num == "worst") {
     return(y$Hospital[nrow(y)])
   }
   else {
     return(y$Hospital[num])
   }
 }, num )

return (data.frame(hospital=unlist(sorttable), state=names(sorttable)))
  
}  
