library(tidyverse)

# --------------------------------------------------------------------------------------------- #

# Function that aggregates pitcher-level statistics for a single team (home or away) in one game.
# Takes a list of player objects from the MLB boxscore JSON and returns
# one row per player with pitching stats.

aggregate_pitching_team <- function(players, game_id, game_date, team_side) {
  
  imap_dfr(players, function(player, id) {
    
    pitch <- player$stats$pitching
    
    tibble(
      date                         = game_date,
      gameID                       = game_id,
      VorH                         = team_side,
      playerName                   = player$person$fullName,
      playerID                     = player$person$id,
      jerseyNumber                 = player$person$jerseyNumber,
      position                     = player$position$name,
      pitch_flyOuts                = pitch$flyOuts,
      pitch_groundOuts             = pitch$groundOuts,
      pitch_airOuts                = pitch$airOuts,
      pitch_runs                   = pitch$runs,
      pitch_doubles                = pitch$doubles,
      pitch_triples                = pitch$triples,
      pitch_homeRuns               = pitch$homeRuns,
      pitch_strikeOuts             = pitch$strikeOuts,
      pitch_baseOnBalls            = pitch$baseOnBalls,
      pitch_intentionalWalks       = pitch$intentionalWalks,
      pitch_hits                   = pitch$hits,
      pitch_hitByPitch             = pitch$hitByPitch,
      pitch_atBats                 = pitch$atBats,
      pitch_caughtStealing         = pitch$caughtStealing,
      pitch_stolenBases            = pitch$stolenBases,
      pitch_numberOfPitches        = pitch$numberOfPitches,
      pitch_saveOpportunities      = pitch$saveOpportunities,
      pitch_earnedRuns             = pitch$earnedRuns,
      pitch_battersFaced           = pitch$battersFaced,
      pitch_outs                   = pitch$outs,
      pitch_completeGames          = pitch$completeGames,
      pitch_shutouts               = pitch$shutouts,
      pitch_pitchesThrown          = pitch$pitchesThrown,
      pitch_balls                  = pitch$balls,
      pitch_strikes                = pitch$strikes,
      pitch_hitBatsmen             = pitch$hitBatsmen,
      pitch_balks                  = pitch$balks,
      pitch_wildPitches            = pitch$wildPitches,
      pitch_pickoffs               = pitch$pickoffs,
      pitch_rbi                    = pitch$rbi,
      pitch_inheritedRunners       = pitch$inheritedRunners,
      pitch_inheritedRunnersScored = pitch$inheritedRunnersScored,
      pitch_catchersInterference   = pitch$catchersInterference,
      pitch_sacBunts               = pitch$sacBunts,
      pitch_sacFlies               = pitch$sacFlies
    )
  })
}

# --------------------------------------------------------------------------------------------- #

# Function that aggregates pitcher-level statistics for an entire game. 
# Extracts game metadata and applies team-level aggregation to both
# home and away pitchers, returning one row per player per game.

aggregate_pitching <- function(game_raw) {
  
  game_id   <- game_raw$gameData$game$pk
  game_date <- as.Date(game_raw$gameData$datetime$officialDate)
  
  home_players <- game_raw$liveData$boxscore$teams$home$players
  away_players <- game_raw$liveData$boxscore$teams$away$players
  
  bind_rows(
    aggregate_pitching_team(home_players, game_id, game_date, "H"),
    aggregate_pitching_team(away_players, game_id, game_date, "V")
  )
}

pitch_stats <- aggregate_pitching(game_data)
