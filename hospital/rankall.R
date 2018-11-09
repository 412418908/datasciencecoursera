



rankall <- function(outcome, num = "best"){
  # check parameters 
  validOutcomes <- c("heart attack", "heart failure", "pneumonia");
  validCols <- c(11, 17, 23);
  idx <- which(validOutcomes == outcome);
  outcomeCol <- validCols[idx[1]];
  if (is.na(outcomeCol)){
    stop("invalid outcome");
  }
  
 
  # split data by state
  data <- read.csv("outcome-of-care-measures.csv", header=TRUE, sep=",", colClasses = "character");
  colnames <- names(data);
  hospitalCol <- which(colnames == "Hospital.Name");
  stateCol <- which(colnames == "State");
  data[, outcomeCol] <- suppressWarnings(as.numeric(data[, outcomeCol]));
  data <- subset(data, !is.na(data[, outcomeCol]));
  data <- data.frame(name=data[,hospitalCol], State=data[,stateCol], outcome=data[,outcomeCol]);
  stateData <- split(data, data$State);
  
  stateData <- lapply(stateData, function(item){
     item <- item[order(item$outcome, item$name), ];
     if ("best" == num){
       return(item[1,]);
     }else if ("worst" == num){
       return(item[nrow(item),]);
     }else{
       return(item[num,]);
     }     
  });
  
  # return a data frame with 2 column (hospital, state)
  vecState <- names(stateData);
  vecHospital <- vapply(stateData, function(item){as.character(item$name)}, character(1));
  data.frame(hospital=vecHospital, state=vecState);
  
}


