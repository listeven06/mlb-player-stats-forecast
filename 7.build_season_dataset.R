library(tidyverse)
library(progress)

mlb_game_dir <- "/Users/listeven/Documents/School/MASDS THESIS/MLBgames"

# ---------------------------------------------------------------------------- #
# 1. Function: list_game_files
# 
# Purpose:
#   - Scan a directory for MLB game .RData files
#   - Extract game metadata (date + gameID) from filenames
#   - Return a clean index dataframe for filtering by season
#
# Expected file format: game__YYMMDD__gameID.RData
#
# ---------------------------------------------------------------------------- #
list_game_files <- function(game_dir, pattern = "^game__\\d{8}__\\d+\\.RData$") {
  
  # list all files in the directory matching the game naming pattern
  # keep the full file path with full.names = TRUE
  files <- list.files(game_dir, pattern = pattern, full.names = TRUE)
  
  tibble(file = files) %>%
    mutate(
      fname = basename(file),
      date_str = str_match(fname, "^game__(\\d{8})__")[,2],
      gameID   = str_match(fname, "__([0-9]+)\\.RData$")[,2],
      date     = ymd(date_str),
      gameID   = as.character(gameID)
    ) %>%
    arrange(date, gameID)
}

# ---------------------------------------------------------------------------- #
# 2. Function: filter_season_2010
#
# Purpose: 
#   - filter for 2010 mlb season window
#
# Season boundaries:
#   - first game: 2010-03-02 
#   - last game:  2010-11-01
#
# ---------------------------------------------------------------------------- #

filter_season_2010 <- function(file_index) {
  file_index %>%
    filter(date >= ymd("2010-03-02"), date <= ymd("2010-11-01")) %>%
    arrange(date, gameID)
}

# ---------------------------------------------------------------------------- #


process_one_game <- function(rdata_file_directory) {
  
  tryCatch({
    
    # 1. load and parse raw game JSON
    parsed_game_data <- read_rdata_json(rdata_file_directory)
    
    # 2. aggregate stats 
    bat_stats <- aggregate_batting(parsed_game_data)
    pitch_stats <- aggregate_pitching(parsed_game_data)
    field_stats <- aggregate_fielding(parsed_game_data)
    
    # 3. merge into one per-game dataframe
    merged_df <- merge_stats(
      bat_stats, 
      field_stats, 
      pitch_stats
    )
    
    # 4. return player-level stats for this game in merged df 
    merged_df
    
  }, error = function(e) {
    message("Error in file: ", basename(rdata_file_directory))
    message(e$message)
    return(NULL)
  })
}

# ---------------------------------------------------------------------------- #
# Function: build_season_panel
#
# Purpose:
#   - Process a set of game files (from an index) using process_one_game()
#   - Bind all player-game rows into a single season-level dataframe
#   - Sort for modeling: playerId -> date -> gameID
#
# ---------------------------------------------------------------------------- #

# build tibble with full filenames, file name, gameID, and date
gamesIndex <- list_game_files(mlb_game_dir)

# (TEMP) subset for 2010 season
subsetGamesIndex <- filter_season_2010(gamesIndex)


build_season_panel <- function(games_index, verbose = TRUE) {
  
  pb <- progress_bar$new(
    total = nrow(games_index),
    format = "Processing [:bar] :percent | Game :current/:total \n"
  )
  
  res <- games_index %>%
    mutate(
      df = map(file, ~{
        pb$tick()
        
        tryCatch(
          process_one_game(.x),
          error = function(e) {
            message("FAILED: ", basename(.x), " | ", e$message)
            NULL
          }
        )
      })
    )
  
  # Bind all rows
  season_df <- res %>%
    select(df) %>%
    unnest(df)
  
  # sort by playerID -> date -> gameID
  season_df <- season_df %>%
    arrange(playerID, date, gameID)
  
  # return season stats per player-game
  season_df
}

subset_df <- build_season_panel(
  games_index = subsetGamesIndex
)

