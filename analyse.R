#Look for variation in circadian patterns between platforms. Previous research (Taha)
#has shown that there is a discernible pattern - previous unpublished research (Oliver)
#has shown that this pattern varies depending on platform. If we can replicate that
#it should go in.
circadian_variation <- function(data){
  dataset <- data[data$namespace ==0 , j=list(edits = .N), by = c("type","hour")]
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
  
  dataset <- data[data$namespace == 0 & data$is_new == TRUE, j=list(edits = .N), by = c("type","hour")]
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
    missingCountries <- unique(cdm$ISO_A2[!(cdm$ISO_A2 %in% x$country)])
    if(length(missingCountries) >= 1){
      dt <- rbind(dt, data.frame(country=missingCountries, count=0))
    }
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
  stat_fun <- function(data, indices){
    data <- data[indices]
    return(sum(data)/length(data))
  }
  sess_dataset <- data[data$is_new == TRUE,]
  mobile_session_sample <- reconstruct_sessions(split(sess_dataset$timestamp[sess_dataset$type == "mobile"],
                                                      sess_dataset$username[sess_dataset$type == "mobile"]))
  desktop_session_sample <- reconstruct_sessions(split(sess_dataset$timestamp[sess_dataset$type == "desktop"],
                                                       sess_dataset$username[sess_dataset$type == "desktop"]))
  
  session_length_mobile_results <- boot(data = unlist(session_length(mobile_session_sample)),
                                        statistic = stat_fun,
                                        R = 1000)$t
  session_length_desktop_results <- boot(data = unlist(session_length(desktop_session_sample)),
                                         statistic = stat_fun,
                                         R = 1000)$t
  results <- data.frame(session_length = c(session_length_mobile_results, session_length_desktop_results),
                        type = c(rep("mobile",1000),rep("desktop",1000)),
                        stringsAsFactors = FALSE)
  
  ggsave(filename = file.path(getwd(),"Paper","Figures","new_contributor_session_length.svg"),
         plot = ggplot(results, aes(type, session_length)) + 
           geom_boxplot() + 
           theme_bw() +
           labs(title = "",
                x = "Contribution type",
                y = "Average session length (seconds)"))
  write.table(results, file = file.path(getwd(),"Paper","Datasets","new_contributor_session_length.tsv"),
              quote = FALSE, sep = "\t", row.names = FALSE)
  
  session_edits_mobile_results <- boot(data = unlist(session_events(mobile_session_sample)),
                                       statistic = stat_fun,
                                       R = 1000)$t
  session_edits_desktop_results <- boot(data = unlist(session_events(desktop_session_sample)),
                                        statistic = stat_fun,
                                        R = 1000)$t
  results <- data.frame(session_events = c(session_edits_mobile_results, session_edits_desktop_results),
                        type = c(rep("mobile",1000),rep("desktop",1000)),
                        stringsAsFactors = FALSE)
  
  ggsave(filename = file.path(getwd(),"Paper","Figures","new_contributor_session_edits.svg"),
         plot = ggplot(results, aes(type, session_events)) + 
           geom_boxplot() + 
           theme_bw() +
           labs(title = "",
                x = "Contribution type",
                y = "Edits per session"))
  write.table(results, file = file.path(getwd(),"Paper","Datasets","new_contributor_edits_per_session.tsv"),
              quote = FALSE, sep = "\t", row.names = FALSE)
  return(data)
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
  ggsave(filename = file.path(getwd(),"Paper","Figures","new_contributor_revert_rate.svg"),
    plot = ggplot(results, aes(type, revert_rate)) + 
      geom_boxplot() + 
      scale_y_continuous(breaks = seq(0,0.1,0.01), limits = c(0,0.1), labels = percent) + 
      theme_bw() +
      labs(title = "",
           x = "Contribution type",
           y = "Percentage of contributions reverted"))
  write.table(results, file = file.path(getwd(),"Paper","Datasets","revert_rate.tsv"),
              quote = FALSE, sep = "\t", row.names = FALSE)
  return(data)
}

#Compares editors-by-country to World Bank data, gathered with
#world_bank_retrieve.R's functions, about mobile and desktop
#connection subscriptions in each country, both as a proportion
#of the population and as a total count of subscriptions
world_bank_ranking <- function(wiki_data, wb_data){
  wiki_data <- wiki_data[, j = list(editors = length(unique(username))), by = c("country_iso","user_type"),]
  merged_set <- merge(x = wb_data, y = wiki_data, all.x = TRUE, by = "country_iso")
  merged_set <- merged_set[!is.na(editors) & !is.na(broadband_population) & !is.na(mobile_population),]
  
  pop_rank_set <- merged_set[,j = {
    to_return <- data.table(desktop_penetration = (sum(editors[user_type %in% c("mixed","desktop")])/broadband_population[1])*1000000,
                            mobile_penetration = (sum(editors[user_type %in% c("mixed","mobile")])/mobile_population[1])*1000000)
    to_return
  }, by = "country_iso"]
  pop_rank_set$desktop_penetration_rank <- rank(pop_rank_set$desktop_penetration, na.last = "keep", ties.method = "min")
  pop_rank_set$mobile_penetration_rank <- rank(pop_rank_set$mobile_penetration, na.last = "keep", ties.method = "min")
  ggsave(filename = file.path(getwd(),"Paper","Figures","editor_versus_penetration_ranking.svg"),
         plot = ggplot(pop_rank_set, aes(x = desktop_penetration_rank, y = mobile_penetration_rank, label = country_iso)) + 
           geom_text() + 
           geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
           theme_bw() +
           labs(title = "",
                x = "Rank for desktop editors / fixed broadband connections",
                y = "Rank for mobile editors / mobile subscribers"))
  write.table(pop_rank_set, file = file.path(getwd(),"Paper","Datasets","editors_and_penetration.tsv"), row.names = FALSE,
              sep = "\t", quote = TRUE)
  
  editor_raw_rank <- merged_set[, j = list(mobile_editors = sum(editors[user_type %in% c("mobile","mixed")]),
                                           desktop_editors = sum(editors[user_type %in% c("desktop","mixed")])),
                                by = "country_iso"]
  editor_raw_rank$desktop_rank <- rank(editor_raw_rank$desktop_editors, na.last = "keep", ties.method = "min")
  editor_raw_rank$mobile_rank <- rank(editor_raw_rank$mobile_editors, na.last = "keep", ties.method = "min")
  
  ggsave(filename = file.path(getwd(),"Paper","Figures","editor_raw_ranking.svg"),
         plot = ggplot(editor_raw_rank, aes(x = desktop_rank, y = mobile_rank, label = country_iso)) + 
           geom_text() + 
           geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
           theme_bw() +
           labs(title = "",
                x = "Rank in desktop editors (raw)",
                y = "Rank in mobile editors (raw)"))
  write.table(editor_raw_rank[,c("country_iso","desktop_rank","mobile_rank"), with = FALSE], file = file.path(getwd(),"Paper","Datasets","editors_raw_rank.tsv"), row.names = FALSE,
              sep = "\t", quote = TRUE)
  
  pop_set <- merged_set[,j = list(mobile_population = unique(mobile_population), desktop_population = unique(broadband_population)),
                        by = "country_iso"]
  pop_set$mobile_rank <- rank(pop_set$mobile_population, na.last = "keep", ties.method = "min")
  pop_set$broadband_rank <- rank(pop_set$desktop_population, na.last = "keep", ties.method = "min")
  ggsave(filename = file.path(getwd(),"Paper","Figures","population_raw_ranking.svg"),
         plot = ggplot(pop_set, aes(x = broadband_rank, y = mobile_rank, label = country_iso)) + 
           geom_text() + 
           geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
           theme_bw() +
           labs(title = "",
                x = "Rank in Fixed Broadband Subscribers (raw)",
                y = "Rank in Mobile Subscribers (raw)"))
  
  pop_percentage_set <- merged_set[,j = list(mobile_proportion = unique(mobile_subscriptions),
                                             broadband_population = unique(broadband_subscriptions)),
                                   by = "country_iso"]
  pop_percentage_set$mobile_percentage_rank <- rank(pop_percentage_set$mobile_proportion, na.last = "keep", ties.method = "min")
  pop_percentage_set$broadband_percentage_rank <- rank(pop_percentage_set$broadband_population, na.last = "keep", ties.method = "min")
  
  ggsave(filename = file.path(getwd(),"Paper","Figures","population_percentage_ranking.svg"),
         plot = ggplot(pop_percentage_set, aes(x = broadband_rank, y = mobile_rank, label = country_iso)) + 
           geom_text() + 
           geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
           theme_bw() +
           labs(title = "",
                x = "Rank in Fixed Broadband Subscribers (%)",
                y = "Rank in Mobile Subscribers (%)"))
}


analyse <- function(data){
  wb_data <- world_bank_retrieve()
  data <- circadian_variation() %>%
    geographic_distribution %>%
    session_distribution %>%
    revert_rate %>%
    world_bank_ranking(wb_data)
  
}

