library(tidyverse)

merge_stats <- function(d1, d2, d3) {
  key <- c("gameID", "playerID")
  player_game <- d1 %>%
    full_join(d2, by = key) %>%
    full_join(d3, by = key)
}

# player_game <- merge_stats(bat_stats, field_stats, pitch_stats)


# Export df to csv
# write.csv(player_game, "/Users/listeven/Documents/School/MASDS THESIS/example_aggregated_player_data.csv")
