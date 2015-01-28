#Load dependencies
source("config.R")
source("retrieve.R")
source("analyse.R")

#Create necessary directories
dir.create(file.path(getwd(),"Paper","Figures"), showWarnings = FALSE)
dir.create(file.path(getwd(),"Paper","Datasets"), showWarnings = FALSE)

#Run!
analyse(retrieve())