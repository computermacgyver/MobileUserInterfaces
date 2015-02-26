#Look for variation in circadian patterns between platforms. Previous research (Taha)
#has shown that there is a discernible pattern - previous unpublished research (Oliver)
#has shown that this pattern varies depending on platform. If we can replicate that
#it should go in.
circadian_variation <- function(data){
  dataset <- data[!data$day %in% c("Sat","Sun"),]
  dataset <- dataset[,j=.SD[sample(1:.N,100000),], by = "type"]
  dataset <- dataset[,j=list(edits = .N), by = c("type","hour")]
  dataset <- dataset[,j=list(edits = edits/sum(edits), hour = hour), by = "type"]
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

#Look for substantial deltas in the geographic distribution of mobile and
#desktop contributors. This involves both visual mapping (because graphs maketh
#the man) and a comparison with world bank numbers to calculate the actual
#connectivity rate of people in a given country.
geographic_distribution <- function(data){
  
}


session_distribution <- function(data){
  stat_fun <- function(data, indices){
    data <- data[indices]
    return(sum(data)/length(data))
  }
  sess_dataset <- data[data$is_new == TRUE,]
  mobile_sessions <- reconstruct_sessions(split(sess_dataset$timestamp[sess_dataset$type == "mobile"],
                                                sess_dataset$username[sess_dataset$type == "mobile"]))
  test <- data[, j = {
    by_user_events <- split(.SD$timestamp,.SD$username)
  }]
}

#Having calculated /whether/ each edit was reverted with is_reverted,
#we now calculate the probability of edits being reverted - specifically,
#content-namespace (namespace == 0) edits from contributors registered
#in the last 90 days (is_new == TRUE). Random resampling and bootstrapping
#generates a series of probabilities from sub-sampled of mobile or desktop
#contributions that fall into this category, and a permutation test then
#identifies if there is a statistically significant difference betwixt
#mobile and desktop newbs.
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
  data <- %<>% circadian_variation() %>%
    geographic_distribution %>%
    session_distribution %>%
    revert_rate %>%
    connection_type
    
}