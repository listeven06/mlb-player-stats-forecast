library(tidyverse)

aggregate_batting <- function(game_raw) {
  
}


summary <- tibble(
  date = game_data$gameData$datetime$dateTime,
  gameID = game_data$gameData$game$pk,
)

home_players <- game_data$liveData$boxscore$teams$home$players

home_batting <- imap_dfr(home_players, function(player, id) {
  bat <- player$stats$batting
  
  tibble(
    playerName = player$person$fullName,
    playerID = player$person$id,
    jerseryNumber = player$person$jerseyNumber,
    position = player$position$name,
    bat_flyOuts = bat$flyOuts,
    bat_groundOuts = bat$groundOuts,
    bat_airOuts = bat$airOuts,
    bat_runs = bat$runs,
    bat_doubles = bat$doubles,
    bat_triples = bat$triples,
    bat_homeRuns = bat$homeRuns,
    bat_strikeOuts = bat$strikeOuts,
    bat_baseOnBalls = bat$baseOnBalls,
    bat_intentionalWalks = bat$intentionalWalks,
    bat_hits = bat$hits,
    bat_hitByPitch = bat$hitByPitch,
    19. bat_atBats
    20. bat_caughtStealing
    21. bat_stolenBases
    22. bat_groundIntoDoublePlay
    23. bat_groundIntoTriplePlay
    24. bat_plateAppearances
  )
  
})

home_batting
