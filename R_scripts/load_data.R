prepare_data <- function(file_path, observed = FALSE, min_eventsize = 1, use_rhom = FALSE, use_dhe = FALSE, normalize = TRUE, sqrt_transform = FALSE) {
  bgg_events <- read.csv(file_path)

  # Filter based on min_eventsize
  bgg_events <- bgg_events[bgg_events$team_size >= min_eventsize, ]
  if (observed) {
    bgg_events <- bgg_events[bgg_events$TYPE == "game", ]
  }
  # Calculate average_duration_at_risk
  bgg_events$tenure_avg <- bgg_events$TIME - bgg_events$tenure_avg

  actor_variables <- c(
    "team_size",
    "gender_avg",
    "gender_disparity",
    "tenure_avg",
    "tenure_disparity"
  )

  rhem_variables <- c(
    "team_closure",
    "team_exact_repetition",
    "team_sub_rep_1",
    "team_sub_rep_2",
    "team_sub_rep_3",
    "team_sub_rep_4"
  )

  rhom_variables <- NULL
  if (use_rhom) {
    # Clipping negative values to 0
    bgg_events$cum_performance_exact <- pmax(bgg_events$cum_performance_exact, 0)

    # For prior_success_exact
    bgg_events$prior_success_exact <- 0
    bgg_events[bgg_events$team_exact_repetition > 0, "prior_success_exact"] <-
      bgg_events[bgg_events$team_exact_repetition > 0, "cum_performance_exact"] /
        bgg_events[bgg_events$team_exact_repetition > 0, "team_exact_repetition"]

    # Clipping negative values to 0 for prior_success_X variables
    for (i in 1:4) {
      prior_col <- paste0("prior_success_", i)
      cum_col <- paste0("cum_performance_", i)
      sub_rep_col <- paste0("team_sub_rep_cum_", i)

      # Clip negative values to 0
      bgg_events[[cum_col]] <- pmax(bgg_events[[cum_col]], 0)

      bgg_events[[prior_col]] <- 0
      bgg_events[bgg_events[[sub_rep_col]] > 0, prior_col] <-
        bgg_events[bgg_events[[sub_rep_col]] > 0, cum_col] /
          bgg_events[bgg_events[[sub_rep_col]] > 0, sub_rep_col]
    }
    rhom_variables <- c(
      "prior_success_exact",
      "prior_success_1",
      "prior_success_2",
      "prior_success_3",
      "prior_success_4",
      "performance_disparity"
    )
  }

  dhe_variables <- NULL
  if (use_dhe) {
    dhe_variables <- c(
      "team_mechanic_closure",
      "portfolio_size_heterogeneity",
      "portfolio_size_avg",
      "team_mechanic_sub_rep_1_1",
      "team_mechanic_sub_rep_1_2",
      "team_mechanic_sub_rep_1_3",
      "team_mechanic_sub_rep_2_1",
      "team_mechanic_sub_rep_2_2",
      "team_mechanic_sub_rep_2_3",
      "team_mechanic_sub_rep_3_1",
      "team_mechanic_sub_rep_3_2",
      "team_mechanic_sub_rep_3_3",
      "mechanic_size",
      #'mechanic_closure',
      "mechanics_exact_repetition",
      "exact_repetition",
      "mechanic_sub_rep_1",
      "mechanic_sub_rep_2",
      "mechanic_sub_rep_3",
      "mechanic_sub_rep_4",
      "mechanic_sub_rep_5"
    )
  }

  ivs <- c(actor_variables, rhem_variables, rhom_variables, dhe_variables)

  # Transform gender_avg to range 0-1
  bgg_events$gender_avg <- (bgg_events$gender_avg + 1) / 2

  #Square root transformation
  if (sqrt_transform) {
    network_vars <- c(rhem_variables, rhom_variables, dhe_variables)
    bgg_events[, network_vars] <- sqrt(bgg_events[, network_vars])
  }

  # Normalize
  if (normalize) {
    bgg_events[, c(12:ncol(bgg_events))] <- scale(bgg_events[, c(12:ncol(bgg_events))])
  }

  return(list(bgg_events = bgg_events, ivs = ivs))
}
