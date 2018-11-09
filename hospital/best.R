



best <- function(state, outcome){
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
  stateData[,outcomeCol] <- as.numeric(stateData[,outcomeCol]);
  stateData <- subset(stateData, !is.na(stateData[,outcomeCol]));
  
  minValue = min(stateData[,outcomeCol]);
  #grep rows that outcome equals to minValue
  stateData <- subset(stateData, stateData[,outcomeCol] == minValue);
  
  #get hospital names , and sort by alphabet
  hospitalNames = stateData$Hospital.Name;
  sort(hospitalNames)[1]
  
}


