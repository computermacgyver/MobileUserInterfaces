#Functions for retrieving world bank related data. This is largely based on
#a script by Scott Hale, my coauthor, and he should be considered the author.

#Grabs datasets from the world bank site, unzips them,
#retrieves the actual data as opposed to the associated meta-crap,
#and then filters the data to provide the last complete year (2013)
#and the actual meta-fields we care about (indicator name, country name,
#country code) under normalised file headings
download_and_unzip_world_bank <- function(url, field_name){
  file <- tempfile()
  dir <- tempdir()
  download.file(url, file)
  unzip(file, exdir = dir)
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  filtered_files <- files[!grepl(x = files, pattern = "Metadata")]
  data <- read.csv(filtered_files[1], skip = 1, stringsAsFactors = FALSE)
  file.remove(files)
  data <- data.frame(country_name = data$Country.Name, country_code = data$Country.Code,
                     value = data$X2013, stringsAsFactors = FALSE)
  names(data)[3] <- field_name
  return(data)
}

#Retrieves data from the worldbank website, using download_and_unzip_world_bank for the
#heavy lifting, and then merges it into a single data.frame that is amenable to further
#analysis.
get_data <- function(){
  mobile_and_broadband <- merge(download_and_unzip_world_bank("http://api.worldbank.org/v2/en/indicator/it.cel.sets.p2?downloadformat=csv",
                                                              "mobile_subscriptions"),
                                download_and_unzip_world_bank("http://api.worldbank.org/v2/en/indicator/it.net.bbnd.p2?downloadformat=csv",
                                                              "broadband_subscriptions"),
                                by = c("country_name","country_code"))
  internet_and_pop <- merge(download_and_unzip_world_bank("http://api.worldbank.org/v2/en/indicator/it.net.user.p2?downloadformat=csv",
                                                          "internet_subscriptions"),
                            download_and_unzip_world_bank("http://api.worldbank.org/v2/en/indicator/sp.pop.totl?downloadformat=csv",
                                                          "population"),
                            by = c("country_name","country_code"))
  results <- merge(mobile_and_broadband, internet_and_pop, by = c("country_name","country_code"))
  setnames(results, "country_code", "country_iso")
  return(results)
}

#Scrub the data and calculate second-level figures. This consists largely of handling
#cases where there are more mobile subscriptions than people, and calculating the actual
#number of people in a {connection_type, country} tuple using the population count.
clean_data <- function(data){
  data$mobile_subscriptions_capped <- ifelse(test = data$mobile_subscriptions > 100, 
                                             yes  = 100,
                                             no = data$mobile_subscriptions)
  data$internet_population <- (data$population * data$internet_subscriptions)
  data$broadband_population <- (data$population * data$broadband_subscriptions)
  data$mobile_population <- (data$population * data$mobile_subscriptions_capped)
  return(data)
}

#Grabs the ISO-2 country code and replaces the relevant ISO-3 with it.
#This is necessary because rgeoip outputs an ISO-2, and the World Bank an ISO-3,
#making merging...a pain.
get_iso2 <- function(data){
  
}
#Wrapper function for all of the above; retrieves raw data, parses and scrubs it,
#generates second-order data, writes it to file and then passes it on.
world_bank_retrieve <- function(){
  results <- get_data() %>%
    clean_data %>%
    get_iso2
  
  write.table(results, file = file.path(getwd(),"Paper", "Datasets", "world_bank_data.tsv"),
              row.names = FALSE, quote = TRUE, sep = "\t")
  return(as.data.table(results))
}