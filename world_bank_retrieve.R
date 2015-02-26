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
  names(data)[4] <- field_name
  return(data)
}

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
  return(results)
}