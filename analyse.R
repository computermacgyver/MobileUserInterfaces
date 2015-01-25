#Look for variation in circadian patterns between platforms
circadian_variation <- function(data){
  dataset <- data[,j=.SD[sample(1:.N,100000),], by = "type"]
  dataset <- dataset[,j=list(edits = .N), by = c("type","hour")]
  ggsave(filename = file.path(getwd(),"Paper","Figures","circadian_variation.png"),
         plot = ggplot(data = dataset, aes(hour, edits)) +
    geom_line(aes(group = type, colour = type)) +
    scale_x_discrete(breaks = seq(0,23,1)) +
    labs(title = "Wikimedia edits by hour, mobile versus desktop, 90 day sample",
         x = "Hour",
         y = "% of requests") +
    theme_bw())
  write.table(dataset, file = file.path(getwd(),"Paper","Datasets","circadian_variation.tsv"),
              quote = FALSE, sep = "\t", row.names = FALSE)
  return(data[,c("hour","day") := NULL,])
}

analyse <- function(data){
  data <- circadian_variation(data) %>%
    
}