#Look for variation in circadian patterns between platforms
circadian_variation <- function(data){
  dataset <- data[,j=.SD[sample(1:.N,100000),], by = "type"]
  dataset <- dataset[,j=list(edits = .N), by = c("type","hour")]
  ggsave(filename = file.path(getwd(),"Paper","Figures","circadian_variation.svg"),
         plot = ggplot(data = dataset, aes(hour, edits, color=as.factor(type))) +
    geom_line(aes(group = type, colour = type)) +
    scale_y_continuous("Percent of requests", labels = percent) +
    scale_x_continuous("Local hour of the day based on geocoded IP address") +
    scale_color_brewer("Traffic source", type = "qual", palette = 7) +
    theme_bw())
  write.table(dataset, file = file.path(getwd(),"Paper","Datasets","circadian_variation.tsv"),
              quote = FALSE, sep = "\t", row.names = FALSE)
  return(data[,c("hour","day") := NULL,])
}

geographic_distribution <- function(data){
  
}


session_distribution <- function(data){
  test <- data[, j = {
    by_user_events <- split(.SD$timestamp,.SD$username)
  }]
}

revert_rate <- function(data){
  boot_dataset <- data[data$is_new == TRUE & data$namespace == 0,]
  stat_fun <- function(data, indices){
    data <- data[indices]
    return(sum(data)/length(data))
  }
  mobile_results <- boot(data = boot_dataset$reverted[boot_dataset$type == "mobile"],
                         statistic = stat_fun,
                         R = 1000)$t
  desktop_results <- boot(data = boot_dataset$reverted[boot_dataset$type == "desktop"],
                          statistic = stat_fun,
                          R = 1000)$t
  results <- data.frame(revert_rate = c(mobile_results, desktop_results),
                        type = c(rep("mobile",1000),rep("desktop",1000)),
                        stringsAsFactors = FALSE)
  ggsave(filename = file.path(getwd(),"Paper","Figures","revert_rate.svg"),
    plot = ggplot(results, aes(type, revert_rate)) + 
      geom_boxplot() + 
      scale_y_continuous(breaks = seq(0,0.1,0.01), limits = c(0,0.1), labels = percent) + 
      theme_bw() +
      labs(title = "Revert rate of article contributions by new (<90 days since registration) contributors",
           x = "Contribution type",
           y = "Percentage of contributions reverted"))
  write.table(results, file = file.path(getwd(),"Paper","Datasets","revert_rate.tsv"),
              quote = FALSE, sep = "\t", row.names = FALSE)
  print(permTS(results$revert_rate[results$type == "mobile"],
         results$revert_rate[results$type == "desktop"],
         "greater"))
  return(data[,c("reverted","namespace") := NULL,])
}
connection_type <- function(data){
  
}
analyse <- function(data){
  data <- %<>% circadian_variation %>%
    geographic_distribution %>%
    session_distribution %>%
    revert_rate %>%
    connection_type
    
}