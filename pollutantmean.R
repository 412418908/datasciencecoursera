



pollutantmean <- function(directory, pollutant, id=1:332){
  print(c("directory=", directory));
  files <- list.files(directory, pattern="*.csv");
  print(sprintf("files=%d", length(files)))
  files <- file.path(directory, files);
  tmplist <- lapply(files, read.csv, header=TRUE, sep=",")
  data <- do.call(rbind, tmplist)
  print(ncol(data))
  print(nrow(data))
  data <- data[which(!is.na(data[pollutant]) & is.element(data$ID,id) ),]
  
  mean(data[[pollutant]])
}


