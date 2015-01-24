#Look for variation in circadian patterns between platforms
circadian_variation <- function(data){
  dataset <- data[,j=.SD[sample(1:.N,100000),], by = "type"]
  dataset <- dataset[,j=list(edits = .N), by = c("type","hour")]
}