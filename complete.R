



complete <- function(directory, id=1:332){

  #  1) get filename 
  #  2) read file,  sum <-  sum ( !is.na(data$sulfate & data$nitrate) )
  #  3) append sum to sumVector
  #  4) generate dataframe with id and sumVector(id, nobs)
  #
  #
  sumVector <- numeric(length(id))
  i <- 1;
  for (id0 in id){
    filename = sprintf("%03d.csv", id0);
    data <- read.csv(file.path(directory, filename), header = TRUE, sep = ",");
    cases <- sum(!is.na(data$sulfate & data$nitrate));
    sumVector[i] <- cases;
    i <- i+1;
  }
  data.frame(id=id, nobs=sumVector);
}


