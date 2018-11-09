



rankhospital <- function(state, outcome, num = "best"){
  # check parameters 
  validOutcomes <- c("heart attack", "heart failure", "pneumonia");
  validCols <- c(11, 17, 23);
  idx <- which(validOutcomes == outcome);
  outcomeCol <- validCols[idx[1]];
  if (is.na(outcomeCol)){
    stop("invalid outcome");
  }
 
  # get all states into stateVec
  data <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=",", colClasses = "character")
  stateVec <- unique(data$State);
  if (!(state %in% stateVec)){
    stop("invalid state");
  }
  
  # grep data belong to the special state, grep data that is NA
  stateData <- subset(data, data$State == state);
  suppressWarnings(stateData[,outcomeCol] <- as.numeric(stateData[,outcomeCol]));
  stateData <- subset(stateData, !is.na(stateData[,outcomeCol]));
  stateData <- data.frame(name=stateData$Hospital.Name, outcome=stateData[[outcomeCol]]);
  stateData <- stateData[order(stateData$outcome, stateData$name), ]
  
  #get hospital names , and sort by alphabet
  hospitalNames <- stateData$name; # TODO unique
  #hospitalNames <- sort(hospitalNames)
  if (length(hospitalNames) == 0){
    return (NA);
  }
  if ("best" == num){
    return( hospitalNames[1] );
  }else if ("worst" == num){
    return(hospitalNames[length(hospitalNames)]);
  }else{
    return(hospitalNames[num]);
  }
  
}


