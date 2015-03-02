#Look for variation in circadian patterns between platforms. Previous research (Taha)
#has shown that there is a discernible pattern - previous unpublished research (Oliver)
#has shown that this pattern varies depending on platform. If we can replicate that
#it should go in.
circadian_variation <- function(data){
  dataset <- data[,j=list(edits = .N), by = c("type","hour")]
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
  
  dataset <- data[data$is_new == TRUE, j=list(edits = .N), by = c("type","hour")]
  dataset <- dataset[,j=list(edits = edits/sum(edits), hour = hour), by = "type"]
  ggsave(filename = file.path(getwd(),"Paper","Figures","new_user_circadian_variation.svg"),
         plot = ggplot(data = dataset, aes(hour, edits, color=as.factor(type))) +
           geom_line(aes(group = type, colour = type)) +
           scale_y_continuous("Percent of requests", labels = percent) +
           scale_x_continuous("Local hour of the day based on geocoded IP address") +
           scale_color_brewer("Traffic source", type = "qual", palette = 7) +
           theme_bw())
  write.table(dataset, file = file.path(getwd(),"Paper","Datasets","new_user_circadian_variation.tsv"),
              quote = FALSE, sep = "\t", row.names = FALSE)
  return(data[,c("hour","day") := NULL,])
}

#Look for substantial deltas in the geographic distribution of mobile and
#desktop contributors. This involves both visual mapping (because graphs maketh
#the man) and a comparison with world bank numbers to calculate the actual
#connectivity rate of people in a given country.
geographic_distribution <- function(data){
  
  dt_geo_plot <- function(dt, title){
    suppressMessages({
    setnames(dt, 1:2, c("country","count"))
    cdm <- joinCountryData2Map(dt, joinCode="ISO2", nameJoinColumn="country", suggestForFailedCodes=TRUE)
    values <- as.data.frame(cdm[,c("count", "country")])
    names(values) <- c("count", "id")
    values <- unique(values)
    fortified_polygons <- fortify(cdm, region = "country")
    ggplot(values) + 
      geom_map(aes(fill = count, map_id = id),
               map = fortified_polygons) +
      expand_limits(x = fortified_polygons$long,
                    y = fortified_polygons$lat) +
      coord_equal() + 
      coord_map(projection="mollweide") +
      labs(title = title,
           x = "Longitude",
           y = "Latitude") +
      scale_fill_gradientn(colours=brewer.pal(9, "Blues")[3:8])
    })
  }
  
  svg(filename = file.path(getwd(),"Paper","Figures","edit_map.svg"), width = par("din")[1], height = par("din")[2])
  grid.arrange(dt_geo_plot(data[data$type == "desktop" , j=list(log10_edits = log10(.N)), by = "country_iso"],
                           "Desktop"),
               dt_geo_plot(data[data$type == "mobile" , j=list(log10_edits = log10(.N)), by = "country_iso"],
                           "Mobile"), ncol = 2)
  dev.off()
  
  svg(filename = file.path(getwd(),"Paper","Figures","editor_map.svg"), width = par("din")[1], height = par("din")[2])
  grid.arrange(dt_geo_plot(data[data$user_type %in% c("desktop","mixed") , j=list(editors = log10(length(unique(username)))), by = "country_iso"],
                           "Desktop & Mixed-Method"),
               dt_geo_plot(data[data$user_type %in% c("mobile","mixed"), j=list(editors = log10(length(unique(username))))
                                , by = "country_iso"],
                           "Mobile & Mixed-Method"), ncol = 2)
  dev.off()
  return(data)
}

#Look for variation in common session metrics (session length,
#number of events) between classes of edit. Use reconstructr,
#which implements the session reconstruction approach pioneered
#by Halfaker et al.
session_distribution <- function(data){
  
  metric_to_df <- function(desk_set, mob_set){
    return(rbind(data.frame(type = "desktop", value = unlist(desk_set), stringsAsFactors = FALSE),
                 data.frame(type = "mobile", value = unlist(mob_set), stringsAsFactors = FALSE)))
  }

  sess_dataset <- data[data$is_new == TRUE,]
  mobile_session_sample <- reconstruct_sessions(split(sess_dataset$timestamp[sess_dataset$type == "mobile"],
                                                      sess_dataset$username[sess_dataset$type == "mobile"]))
  desktop_session_sample <- reconstruct_sessions(split(sess_dataset$timestamp[sess_dataset$type == "desktop"],
                                                       sess_dataset$username[sess_dataset$type == "desktop"]))
  
  session_length <- metric_to_df(desk_set = session_length(desktop_session_sample),
                                 mob_set = session_length(mobile_session_sample))
  session_length <- session_length[session_length$value > -1,]
  event_count <- metric_to_df(desk_set = session_events(desktop_session_sample),
                              mob_set = session_events(mobile_session_sample))

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

world_bank_ranking <- function(wiki_data, wb_data){
  merged_set <- merge(x = wb_data, y = wiki_data, all.x = TRUE, by = "country_iso")
}
analyse <- function(data){
  data <- %<>% circadian_variation() %>%
    geographic_distribution %>%
    session_distribution %>%
    revert_rate %>%
    connection_type
  
  #Ranking
  world_bank_ranking(data, world_bank_retrieve())
}