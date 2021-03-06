#Retrieves all edits or page creations from all public projects in the last 90 days,
#excluding actions by anonymous users or bots. Bots are either self-identified through the
#ug_group field in user_groups, or identified using a set of user-agent and user-name based
#heuristics found on config.R.
get_data <- function(){
  data <- global_query("SELECT cuc_ip AS ip_address,
                          cuc_timestamp AS timestamp,
                          cuc_user_text AS username,
                          cuc_agent AS user_agent,
                          cuc_namespace AS namespace,
                          ts_tags AS type,
                          cuc_page_id AS page,
                          rev_sha1 AS hash,
                          user_registration AS registration
                        FROM cu_changes LEFT JOIN tag_summary
                          ON cuc_this_oldid = ts_rev_id
                        INNER JOIN revision
                          ON cuc_this_oldid = rev_id
                        INNER JOIN user
                          ON cuc_user = user_id
                        WHERE cuc_type IN(0,1)
                          AND cuc_user > 0
                          AND cuc_user NOT IN
                          (
                            SELECT user_id
                            FROM user INNER JOIN user_groups
                              ON user_id = ug_user AND ug_group = 'bot'
                          );")
  data <- data[!data$username %in% bot_usernames & !grepl(x = data$user_agent, pattern = bot_agents,
                                                          ignore.case = TRUE, perl = TRUE, useBytes = TRUE)]
  data$timestamp <- as.character(to_posix(data$timestamp))
  data$type[grepl(x = data$type, pattern = "mobile", fixed = TRUE)] <- "mobile"
  data$type[!data$type == "mobile"] <- "desktop"
  data$type[is.na(data$type)] <- "desktop"
  return(data)
}

#Detects whether edits were reverted or not, using Halfaker et al.'s heuristic: whether
#there is a match between a preceding and following edit's SHA1 hashes on the same page,
#within 24 hours of each other.
is_reverted <- function(data){
  data <- data[,j = {
      sdc <- as.data.frame(.SD)
      timestamps <- sdc$timestamp
      sdc$timestamp <- as.numeric(as.POSIXlt(sdc$timestamp))
      out <- detect_reverts(df = sdc, ts_col = "timestamp", hash_col = "hash", page_col = "page",
                            is_av = TRUE)
      out$timestamp <- timestamps
      out
    }, by = "project"]
  return(data)
}

#Geolocates the IP addresses associated with edits, using the MaxMind geolocation binaries
#(accessed through rgeoip) to return the ISO 3166 country codes associated with the IP,
#along with tz-data compatible timezones that can be combined with the timestamp field
#in localise() to localise timestamps, extracting HOD/DOW. IP addresses without
#identifiable countries are excluded from further analysis.
geolocate <- function(data){
  data <- cbind(data,
                geolookup(data$ip_address, geo_city_path, c("country_iso", "timezone")),
                geolookup(data$ip_address, geo_con_path, c("connection_type")))
  data <- data[!data$country_iso == "Unknown" & !data$timezone == "Unknown",]
  return(data)
}

#Takes a geolocated set of edits and localises them using the timezone extracted from
#the IP address, returning the hour of the day and day of the week of the data's
#timestamp field (and the data itself, obviously.)
localise <- function(data){
  data$rn <- seq(1,nrow(data),1)
  pairs <- as.data.table(ddply(.data = data,
                               .variables = "timezone",
                               .fun = function(x){
                                 tses <- as.character(with_tz(as.POSIXlt(x$timestamp,"UTC"), x$timezone[1]))
                                 result <- data.frame(timestamps = tses,
                                                      rn = x$rn,
                                                      stringsAsFactors = FALSE)
                                 return(result)
                               }))
  localised_timestamps <- as.POSIXlt(pairs$timestamps, tz = "UTC")
  pairs$day <- as.character(wday(localised_timestamps, label = TRUE))
  pairs$hour <- hour(localised_timestamps)
  pairs <- pairs[,c("timezone","timestamps") :=NULL]
  data <- merge(data, pairs, by = "rn", all.x = TRUE)
  return(data)
}

#Categorises users, identifying (respectively) new users, and primarily-mobile or
#primarily-desktop users
categorise_users <- function(data){
  is_new <- logical(nrow(data))
  is_new[data$registration >= min(to_mw(data$timestamp))] <- TRUE
  data$is_new <- is_new
  data <- data[,j = {
    sdc <- copy(.SD)
    unique_types <- unique(type)
    if(length(unique_types) == 1){
      sdc$user_type <- unique_types[1]
    } else {
      sdc$user_type <- "mixed"
    }
    sdc
  }, by = "username"]
  return(data)
}

#Hash usernames, with a stable but unknown salt; this allows us to pass
#usernames forward as unique tokens, without actually passing usernames forward.
hash_users <- function(data){
  salt_val <- runif(1)
  data$username <- md5(paste0(data$username,salt_val))
  return(data)
}

#Strips unnecessary fields and converts timestamps to numeric values,
#allowing for session reconstruction during the "analysis" portion of the
#project.
clean <- function(data){
  data <- data[,c("ip_address","user_agent","page","registration","hash","timezone","rn") := NULL,]
  data$timestamp <- as.numeric(as.POSIXlt(data$timestamp))
  return(data)
}

#Binding function for the preceding retrieval elements
retrieve <- function(){
  data <- get_data() %>%
    is_reverted %>%
    geolocate %>%
    localise %>%
    categorise_users %>%
    hash_users %>%
    clean  
  return(data)
}
