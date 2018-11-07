



corr <- function(directory, threshold=0){

  #  1) loop all file
  #    1.1) with file, check sum() large than threshold, is fullfill requirment,
  #    1.2) calculate cor( data$sulfate, data$nitrate)
  #    1.3) set cor -> corVector[i]
  # 2) return corVector
  #
 
  corVector = numeric();
  files = list.files(directory, pattern = "*.csv");
  files <- sort(files);
  i = 1;
  for (file in files){
    data <- read.csv(file.path(directory, file), header = TRUE, sep=",");
    caseSum = sum(!is.na(data$sulfate) & !is.na(data$nitrate));
    if (caseSum <= threshold){
      next;
    }
    corVector[i] <- cor(data$sulfate, data$nitrate, use="complete.obs");
    i <- i+1;
  }
  corVector
}


