library(tidyverse)

# --------------------------------------------------------------------------------------------- #

# Function that aggregates batter-level statistics for a single team (home or away) in one game.
# Takes a list of player objects from the MLB boxscore JSON and returns
# one row per player with batting stats.

aggregate_batting_team <- function(players, game_id, game_date, team_side) {
  
  imap_dfr(players, function(player, id) {
    
    bat <- player$stats$batting
    
    tibble(
      date                     = game_date,
      gameID                   = game_id,
      VorH                     = team_side,
      playerName               = player$person$fullName,
      playerID                 = player$person$id,
      jerseyNumber             = player$person$jerseyNumber,
      position                 = player$position$name,
      bat_flyOuts              = bat$flyOuts,
      bat_groundOuts           = bat$groundOuts,
      bat_airOuts              = bat$airOuts,
      bat_runs                 = bat$runs,
      bat_doubles              = bat$doubles,
      bat_triples              = bat$triples,
      bat_homeRuns             = bat$homeRuns,
      bat_strikeOuts           = bat$strikeOuts,
      bat_baseOnBalls          = bat$baseOnBalls,
      bat_intentionalWalks     = bat$intentionalWalks,
      bat_hits                 = bat$hits,
      bat_hitByPitch           = bat$hitByPitch,
      bat_atBats               = bat$atBats,
      bat_caughtStealing       = bat$caughtStealing,
      bat_stolenBases          = bat$stolenBases,
      bat_groundIntoDoublePlay = bat$groundIntoDoublePlay,
      bat_groundIntoTriplePlay = bat$groundIntoTriplePlay,
      bat_plateAppearances     = bat$plateAppearances,
      bat_totalBases           = bat$totalBases,
      bat_rbi                  = bat$rbi,
      bat_leftOnBase           = bat$leftOnBase,
      bat_sacBunts             = bat$sacBunts,
      bat_sacFlies             = bat$sacFlies,
      bat_catchersInterference = bat$catchersInterference,
      bat_pickoffs             = bat$pickoffs
    )
  })
}

# --------------------------------------------------------------------------------------------- #

# Function that aggregates batter-level statistics for an entire game. 
# Extracts game metadata and applies team-level aggregation to both
# home and away batters, returning one row per player per game.

aggregate_batting <- function(game_raw) {
  
  game_id   <- game_raw$gameData$game$pk
  game_date <- as.Date(game_raw$gameData$datetime$officialDate)
  
  home_players <- game_raw$liveData$boxscore$teams$home$players
  away_players <- game_raw$liveData$boxscore$teams$away$players
  
  bind_rows(
    aggregate_batting_team(home_players, game_id, game_date, "H"),
    aggregate_batting_team(away_players, game_id, game_date, "V")
  )
}

bat_stats <- aggregate_batting(game_data)
