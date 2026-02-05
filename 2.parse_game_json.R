library(tidyverse)
library(jsonlite)

FILE_DIRECTORY <- "/Users/listeven/Documents/School/MASDS THESIS/game__20251101__813024.Rdata"

# --------------------------------------------------------------------------------------------- #

# Function to read a .RData file containing a cached raw API response
# (e.g., data pulled from the MLB API and saved in compressed RData format)

read_rdata_json <- function(path) {
  env <- new.env()
  obj_names <- load(path, envir = env) 
  
  if (length(obj_names) != 1) {
    stop("Multiple objects in .RData. Specify object_name. Found: ",
         paste(obj_names, collapse = ", "))
  }
  object_name <- obj_names[1]
  json_text <- env[[object_name]]
  
  if (!is.character(json_text) || length(json_text) != 1) {
    stop("Loaded object is not a single character string.")
  }
  
  fromJSON(json_text, simplifyVector = FALSE)
}

game_data <- read_rdata_json(FILE_DIRECTORY)
