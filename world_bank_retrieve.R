#Grabs datasets from the world bank site, unzips them,
#retrieves the actual data as opposed to the associated meta-crap,
#and then filters the data to provide the last complete year (2013)
#and the actual meta-fields we care about (indicator name, country name,
#country code) under normalised file headings
download_and_unzip_world_bank <- function(url){
  file <- tempfile()
  dir <- tempdir()
  download.file(url, file)
  unzip(file, exdir = dir)
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  filtered_files <- files[!grepl(x = files, pattern = "Metadata")]
  data <- read.csv(filtered_files[1], skip = 1, stringsAsFactors = FALSE)
  file.remove(files)
  data <- data.frame(country_name = data$Country.Name, country_code = data$Country.Code,
                     indicator = data$Indicator.Name, value = data$X2013, stringsAsFactors = FALSE)
  return(data)
}

