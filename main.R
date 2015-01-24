source("config.R")
source("retrieve.R")
source("analyse.R")

main <- function(){
  analyse(retrieve())
}