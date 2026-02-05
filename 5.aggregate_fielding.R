library(tidyverse)

# --------------------------------------------------------------------------------------------- #

# Function that aggregates fielder-level statistics for a single team (home or away) in one game.
# Takes a list of player objects from the MLB boxscore JSON and returns
# one row per player with fielding stats.

aggregate_fielding_team <- function(players, game_id, game_date, team_side) {
  
  imap_dfr(players, function(player, id) {
    
    field <- player$stats$fielding
    
    tibble(
      gameID               = game_id,
      playerID             = player$person$id,
      field_assists        = field$assists,
      field_putOuts        = field$putOuts,
      field_errors         = field$errors,
      field_chances        = field$chances,
      field_caughtStealing = field$caughtStealing,
      field_passedBall     = field$passedBall,
      field_stolenBases    = field$stolenBases,
      field_pickoffs       = field$pickoffs
    )
  })
}

# --------------------------------------------------------------------------------------------- #

# Function that aggregates fielder-level statistics for an entire game. 
# Extracts game metadata and applies team-level aggregation to both
# home and away pitchers, returning one row per player per game.

aggregate_fielding <- function(game_raw) {
  
  game_id   <- game_raw$gameData$game$pk
  game_date <- as.Date(game_raw$gameData$datetime$officialDate)
  
  home_players <- game_raw$liveData$boxscore$teams$home$players
  away_players <- game_raw$liveData$boxscore$teams$away$players
  
  bind_rows(
    aggregate_fielding_team(home_players, game_id, game_date, "H"),
    aggregate_fielding_team(away_players, game_id, game_date, "V")
  )
}

field_stats <- aggregate_fielding(game_data)

