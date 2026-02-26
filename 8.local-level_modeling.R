library(tidyverse)

# columns to exclude (metadata / identifiers)
exclude_cols <- c("date", "gameID", "VorH", "playerName", "playerID", "position")

# stat columns (everything else)
stat_cols <- setdiff(names(subset_df), exclude_cols)

results <- list()

# -----------------------------------------------------------------------------#
# Create a modeling-ready data set with a per-player time index
# -----------------------------------------------------------------------------#

# create a global time index per player (assumes subset_df is already ordered)
pg <- subset_df %>%
  group_by(playerID) %>%
  mutate(t_global = row_number()) %>%
  ungroup()





# -----------------------------------------------------------------------------#
fit_one_series <- function(df_one) {
  # df_one: One playerID x one stat 
  # contains:
  #   stat: stat name
  #   t: time index
  #   y: stat value at time t
  
  # If too few points, don't try to fit a model
  if (nrow(df_one) < 5) {
    return(tibble(
      playerID  = df_one$playerID[1],
      stat      = df_one$stat[1],
      n         = nrow(df_one),
      converged = FALSE,
      reason    = "too_few_observations"
    ))
  }
  
  # if all zeros, latent rate is basically ~0, dynamics are not identifiable
  
  
}




# -----------------------------------------------------------------------------#
# Loop over each statistical column (one stat at a time)
# -----------------------------------------------------------------------------#

for (s in stat_cols) {
  stat_data <- pg %>%
    select(playerID, t_global, y = all_of(s)) %>%
    
    # remove NA values.
    # NA represents "did not attempt", not zero.
    # after this step, only valid attempts remain.
    filter(!is.na(y)) %>% 
    
    # create a stat-specific time index
    group_by(playerID) %>%
    mutate(t = row_number(), stat = s) %>%
    ungroup()
  
  # split the data into a list of tibbles:
  # each element = one player's time series for stat s
  stat_result <- stat_data %>%
    group_by(playerID) %>%
    group_split() 
    
    # map_dfr runs fit_one_series(list[[i]]) for each (playerID, stat) tibble
    map_dfr(fit_one_series)
    
  results[[s]] <- stat_result
}

results_all <- dplyr::bind_rows(results, .id = "stat_from_list")
