library(tidyverse)
library(jsonlite)


# FILE_DIRECTORY <- "/Users/listeven/Documents/School/MASDS THESIS/sample/game__20251101__813024.RData"

# FILE_DIRECTORY <- "/Users/listeven/Documents/School/MASDS THESIS/MLBgames/game__20251031__813025.RData"

FILE_DIRECTORY <- "/Users/listeven/Documents/School/MASDS THESIS/MLBgames/game__20100302__276989.RData"


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
  
  # JSON may be stored as line-split character vector (older version); 
  # Collapse to single string
  if (length(json_text) > 1) {
    json_text <- paste(json_text, collapse = "")
  }
  
  # Output Error Message if JSON can't be collapsed
  if (length(json_text) != 1) {
    stop("Loaded object could not be converted to a single character string.")
  }
  
  fromJSON(json_text, simplifyVector = FALSE)
}

game_data <- read_rdata_json(FILE_DIRECTORY)
