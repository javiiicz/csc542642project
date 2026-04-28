# WC 2026 model comparison runner
#
# This script does three things:
# 1. Builds a recent national-team match dataset from Transfermarkt.
# 2. Trains the available head-to-head models in R.
# 3. Projects the World Cup bracket and compares the model outcomes side by side.
#
# The bracket projection here is intentionally deterministic at the group stage:
# it seeds each group by a current-feature strength score, then runs the
# knockout bracket through every model. That makes the model comparison
# repeatable. If you later have official group-stage results, you can swap in
# your own `slot_assignments` table and reuse the same simulation helpers.

source("wc2026_bracket_simulation.R")

MODEL_OUT_DIR <- file.path(OUT_DIR, "r_model_outputs")
dir.create(MODEL_OUT_DIR, recursive = TRUE, showWarnings = FALSE)

HISTORICAL_MATCHES_CSV <- file.path(OUT_DIR, "historical_matches_transfermarkt_recent.csv")
H2H_MATCHES_CSV <- file.path(OUT_DIR, "national_team_games_h2h_wc_teams_2018_2026.csv")
PROJECTED_GROUP_RESULTS_CSV <- file.path(MODEL_OUT_DIR, "projected_group_results.csv")
PROJECTED_SLOT_ASSIGNMENTS_CSV <- file.path(MODEL_OUT_DIR, "projected_slot_assignments.csv")
DETERMINISTIC_MATCHES_CSV <- file.path(MODEL_OUT_DIR, "deterministic_bracket_matches.csv")
DETERMINISTIC_TEAM_PROGRESS_CSV <- file.path(MODEL_OUT_DIR, "deterministic_team_progress.csv")
MODEL_COMPARISON_SUMMARY_CSV <- file.path(MODEL_OUT_DIR, "model_comparison_summary.csv")
MODEL_CHAMPION_PROBS_CSV <- file.path(MODEL_OUT_DIR, "model_champion_probabilities.csv")
MODEL_FINAL_POSITIONS_CSV <- file.path(MODEL_OUT_DIR, "model_final_position_probabilities.csv")
MODEL_SAMPLED_PROGRESS_CSV <- file.path(MODEL_OUT_DIR, "model_sampled_team_progress.csv")
MODEL_SAMPLED_MATCHES_CSV <- file.path(MODEL_OUT_DIR, "model_sampled_matches.csv")
MODEL_COMPARISON_RDS <- file.path(MODEL_OUT_DIR, "model_comparison_results.rds")

normalize_transfermarkt_team_name <- function(x) {
  x <- str_squish(as.character(x))
  aliases <- tibble::tribble(
    ~source, ~target,
    "South Korea", "Korea, South",
    "Korea Republic", "Korea, South",
    "Czech Republic", "Czech Republic",
    "Bosnia-Herzegovina", "Bosnia-Herzegovina",
    "Bosnia and Herzegovina", "Bosnia-Herzegovina",
    "Cape Verde", "Cape Verde",
    "Cabo Verde", "Cape Verde",
    "DR Congo", "DR Congo",
    "Democratic Republic of the Congo", "DR Congo",
    "Congo DR", "DR Congo",
    "United States", "United States",
    "Curacao", "Curacao",
    "Curaçao", "Curacao",
    "Cote d'Ivoire", "Cote d'Ivoire",
    "Ivory Coast", "Cote d'Ivoire",
    "Turkiye", "Turkiye",
    "Turkey", "Turkiye"
  )
  idx <- match(x, aliases$source)
  out <- x
  keep <- !is.na(idx)
  out[keep] <- aliases$target[idx[keep]]
  out
}

wc2026_tracked_team_names <- function() {
  c(
    normalize_fwc26_team_name(fwc26_group_assignments()$team_name),
    "Turkiye",
    "Curacao"
  ) |>
    unique()
}

zscore_safe <- function(x) {
  x <- safe_num(x)
  if (all(is.na(x))) return(rep(0, length(x)))
  x[is.na(x)] <- stats::median(x, na.rm = TRUE)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  as.numeric((x - mean(x, na.rm = TRUE)) / s)
}

build_historical_matches_from_transfermarkt <- function(
  out_path = HISTORICAL_MATCHES_CSV,
  start_date = as.Date("2021-01-01"),
  team_features = NULL
) {
  assert_feature_packages()

  games_path <- if (file.exists(H2H_MATCHES_CSV)) {
    H2H_MATCHES_CSV
  } else {
    "transfermarkt_data/games.csv"
  }

  if (!file.exists(games_path)) {
    stop("Missing match file: ", games_path, call. = FALSE)
  }

  if (is.null(team_features)) {
    if (file.exists(TEAM_CURRENT_FEATURES_CSV)) {
      team_features <- read_csv(TEAM_CURRENT_FEATURES_CSV, show_col_types = FALSE)
    } else {
      stop(
        "Missing team_current_features.csv. Run wc2026_head_to_head_models.R first.",
        call. = FALSE
      )
    }
  }

  tracked_team_names <- wc2026_tracked_team_names()
  team_features <- complete_fwc26_team_features(team_features, tracked_team_names)

  games <- read_csv(
    games_path,
    show_col_types = FALSE,
    col_select = c(
      date,
      competition_type,
      home_club_name,
      away_club_name,
      home_club_goals,
      away_club_goals
    )
  ) |>
    mutate(
      date = as.Date(date),
      home_club_name = normalize_transfermarkt_team_name(home_club_name),
      away_club_name = normalize_transfermarkt_team_name(away_club_name),
      home_club_goals = safe_num(home_club_goals),
      away_club_goals = safe_num(away_club_goals)
    ) |>
    filter(
      competition_type == "national_team_competition",
      date >= start_date,
      home_club_name %in% tracked_team_names,
      away_club_name %in% tracked_team_names,
      !is.na(home_club_goals),
      !is.na(away_club_goals),
      home_club_goals != away_club_goals
    ) |>
    transmute(
      team = home_club_name,
      opponent = away_club_name,
      team_goals = home_club_goals,
      opponent_goals = away_club_goals,
      match_date = date,
      neutral = 1L
    )

  write_csv(games, out_path)

  list(
    match_results = games,
    team_features = team_features
  )
}

project_group_results_from_features <- function(team_features) {
  assert_feature_packages()

  group_assignments <- fwc26_group_assignments() |>
    mutate(
      team_model = normalize_fwc26_team_name(team_name)
    )

  required_team_names <- unique(group_assignments$team_model)
  team_features <- complete_fwc26_team_features(team_features, required_team_names)

  score_cols <- c(
    "xg_per90_weighted",
    "np_xg_per90",
    "xa_per90_weighted",
    "shots_per90_weighted",
    "key_passes_per90_weighted",
    "attack_depth_index",
    "caps_mean",
    "market_value_mean"
  )

  missing <- setdiff(score_cols, names(team_features))
  if (length(missing) > 0) {
    stop(
      "team_features is missing required columns for projection: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  projection <- group_assignments |>
    left_join(
      team_features |>
        select(
          nation,
          all_of(score_cols),
          feature_source = any_of("feature_source")
        ),
      by = c("team_model" = "nation")
    ) |>
    mutate(
      across(all_of(score_cols), ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x))
    )

  projection <- projection |>
    mutate(
      xg_per90_weighted_z = zscore_safe(xg_per90_weighted),
      np_xg_per90_z = zscore_safe(np_xg_per90),
      xa_per90_weighted_z = zscore_safe(xa_per90_weighted),
      shots_per90_weighted_z = zscore_safe(shots_per90_weighted),
      key_passes_per90_weighted_z = zscore_safe(key_passes_per90_weighted),
      attack_depth_index_z = zscore_safe(attack_depth_index),
      caps_mean_z = zscore_safe(caps_mean),
      market_value_mean_z = zscore_safe(market_value_mean),
      strength_score =
        0.25 * xg_per90_weighted_z +
        0.20 * np_xg_per90_z +
        0.15 * xa_per90_weighted_z +
        0.10 * shots_per90_weighted_z +
        0.10 * key_passes_per90_weighted_z +
        0.10 * attack_depth_index_z +
        0.05 * caps_mean_z +
        0.05 * market_value_mean_z
    ) |>
    group_by(group) |>
    arrange(desc(strength_score), desc(xg_per90_weighted), desc(caps_mean), team_name, .by_group = TRUE) |>
    mutate(place = row_number()) |>
    ungroup() |>
    select(
      group,
      place,
      team_name,
      team_model,
      team_code,
      strength_score,
      all_of(score_cols),
      feature_source
    )

  projection
}

build_projected_slot_assignments <- function(group_results) {
  assert_feature_packages()

  third_slots <- fwc26_seed_map() |>
    filter(slot_kind == "third_place_pool") |>
    arrange(first_match) |>
    pull(slot)

  third_place <- group_results |>
    filter(place == 3) |>
    arrange(desc(strength_score), desc(xg_per90_weighted), desc(caps_mean), team_name)

  third_place_assignments <- third_place |>
    slice_head(n = length(third_slots)) |>
    mutate(slot = third_slots[seq_len(n())]) |>
    select(group, team_name, slot)

  slot_assignments <- build_fwc26_initial_slot_assignments(
    group_results = group_results,
    third_place_assignments = third_place_assignments
  ) |>
    filter(!is.na(slot))

  list(
    slot_assignments = slot_assignments,
    third_place_assignments = third_place_assignments
  )
}

extract_model_fits <- function(model_suite) {
  if (!is.null(model_suite$models) && length(model_suite$models) > 0) {
    fits <- purrr::map(model_suite$models, "fit")
    names(fits) <- purrr::map_chr(model_suite$models, "model_name")
    return(fits)
  }

  if (is.list(model_suite) && length(model_suite) > 0) {
    if (all(vapply(model_suite, inherits, logical(1), what = "workflow"))) {
      return(model_suite)
    }
    if (all(vapply(model_suite, function(x) !is.null(x$fit), logical(1)))) {
      fits <- purrr::map(model_suite, "fit")
      names(fits) <- names(model_suite)
      return(fits)
    }
  }

  stop(
    "Could not extract model fits from the supplied model suite object.",
    call. = FALSE
  )
}

fit_name_for_model <- function(model_name) {
  if (is.null(model_name) || !nzchar(model_name)) return("model")
  model_name
}

extract_match_export <- function(match_df) {
  assert_feature_packages()

  required_cols <- c("model_name", "sim_id", "match_number", "stage", "team_a", "team_b", "winner", "win_prob", "loss_prob")
  missing_cols <- setdiff(required_cols, names(match_df))
  if (length(missing_cols) > 0) {
    stop(
      "match_df is missing required columns for export: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  match_df |>
    mutate(
      winner_prob = if_else(
        winner == team_a,
        win_prob,
        loss_prob
      ),
      conf_prob = if_else(
        winner == team_a,
        win_prob,
        loss_prob
      ),
      round = pretty_fwc26_stage(as.character(stage)),
      team_1 = team_a,
      team_2 = team_b
    ) |>
    select(
      model_name, sim_id, match_number, round, team_1, team_2, winner, winner_prob, conf_prob
    ) |>
    arrange(model_name, sim_id, match_number)
}

summarise_deterministic_projection <- function(deterministic_team_progress) {
  deterministic_team_progress |>
    group_by(model_name) |>
    summarise(
      champion = team[final_position == "Champion"][1],
      runner_up = team[final_position == "Runner-up"][1],
      third_place = team[final_position == "Third place"][1],
      fourth_place = team[final_position == "Fourth place"][1],
      semifinalists = paste(sort(team[final_position %in% c("Champion", "Runner-up", "Third place", "Fourth place")]), collapse = ", "),
      .groups = "drop"
    )
}

simulate_model_suite_brackets <- function(
  model_suite,
  team_features,
  slot_assignments,
  n_sims = 200,
  decision_rule = c("sample", "most_probable"),
  seed = 642
) {
  assert_feature_packages()

  decision_rule <- match.arg(decision_rule)
  model_fits <- extract_model_fits(model_suite)

  deterministic_runs <- purrr::imap(model_fits, function(model_fit, model_name) {
    sim <- simulate_fwc26_bracket(
      model_fit = model_fit,
      team_features = team_features,
      slot_assignments = slot_assignments,
      decision_rule = "most_probable"
    )

    matches <- sim$matches |>
      mutate(model_name = fit_name_for_model(model_name), sim_id = 0L)

    progress <- sim$team_progress |>
      mutate(model_name = fit_name_for_model(model_name), sim_id = 0L)

    list(matches = matches, progress = progress)
  })

  deterministic_matches_tbl <- purrr::map_dfr(deterministic_runs, "matches")
  deterministic_progress_tbl <- purrr::map_dfr(deterministic_runs, "progress")
  deterministic_summary_tbl <- summarise_deterministic_projection(deterministic_progress_tbl)

  sampled_runs <- purrr::imap(model_fits, function(model_fit, model_name) {
    model_index <- match(model_name, names(model_fits))

    purrr::map(seq_len(n_sims), function(sim_id) {
      set.seed(seed + sim_id + 1000 * model_index)
      sim <- simulate_fwc26_bracket(
        model_fit = model_fit,
        team_features = team_features,
        slot_assignments = slot_assignments,
        decision_rule = decision_rule
      )

      list(
        matches = sim$matches |>
          mutate(model_name = fit_name_for_model(model_name), sim_id = sim_id) |>
          extract_match_export(),
        progress = sim$team_progress |>
          mutate(model_name = fit_name_for_model(model_name), sim_id = sim_id)
      )
    })
  })

  sampled_matches <- purrr::imap_dfr(sampled_runs, function(model_runs, model_name) {
    purrr::map_dfr(model_runs, "matches")
  })

  sampled_progress <- purrr::imap_dfr(sampled_runs, function(model_runs, model_name) {
    purrr::map_dfr(model_runs, "progress")
  })

  champion_probs <- sampled_progress |>
    filter(final_position == "Champion") |>
    count(model_name, team, name = "champion_count") |>
    group_by(model_name) |>
    mutate(champion_prob = champion_count / sum(champion_count)) |>
    ungroup() |>
    arrange(model_name, desc(champion_prob), team)

  final_position_probs <- sampled_progress |>
    count(model_name, team, final_position, name = "count") |>
    group_by(model_name, team) |>
    mutate(prob = count / sum(count)) |>
    ungroup() |>
    arrange(model_name, team, final_position)

  champion_rows <- deterministic_progress_tbl |>
    filter(final_position == "Champion") |>
    select(model_name, team, final_position, matches_played, wins, losses)

  summary_tbl <- model_suite$metrics |>
    mutate(model_name = as.character(model)) |>
    select(model_name, accuracy, roc_auc, mn_log_loss) |>
    left_join(deterministic_summary_tbl, by = "model_name") |>
    left_join(
      champion_probs |>
        group_by(model_name) |>
        slice_max(order_by = champion_prob, n = 1, with_ties = FALSE) |>
        ungroup() |>
        select(model_name, champion_count, champion_prob),
      by = "model_name"
    )

  list(
    deterministic_matches = deterministic_matches_tbl,
    deterministic_team_progress = deterministic_progress_tbl,
    sampled_matches = sampled_matches,
    deterministic_summary = deterministic_summary_tbl,
    sampled_progress = sampled_progress,
    champion_probs = champion_probs,
    final_position_probs = final_position_probs,
    summary = summary_tbl,
    champion_rows = champion_rows
  )
}

save_model_comparison_outputs <- function(results, out_dir = MODEL_OUT_DIR) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv(results$deterministic_matches, DETERMINISTIC_MATCHES_CSV)
  write_csv(results$deterministic_team_progress, DETERMINISTIC_TEAM_PROGRESS_CSV)
  write_csv(results$sampled_matches, MODEL_SAMPLED_MATCHES_CSV)
  write_csv(results$sampled_progress, MODEL_SAMPLED_PROGRESS_CSV)
  write_csv(results$summary, MODEL_COMPARISON_SUMMARY_CSV)
  write_csv(results$champion_probs, MODEL_CHAMPION_PROBS_CSV)
  write_csv(results$final_position_probs, MODEL_FINAL_POSITIONS_CSV)
  saveRDS(results, MODEL_COMPARISON_RDS)

  invisible(results)
}

main <- function(n_sims = 200, decision_rule = c("sample", "most_probable")) {
  assert_feature_packages()
  assert_training_packages()

  decision_rule <- match.arg(decision_rule)

  message("Loading current team features...")
  team_features <- read_csv(TEAM_CURRENT_FEATURES_CSV, show_col_types = FALSE)
  team_features <- complete_fwc26_team_features(team_features, wc2026_tracked_team_names())

  message("Building historical match dataset from Transfermarkt...")
  historical <- build_historical_matches_from_transfermarkt(
    out_path = HISTORICAL_MATCHES_CSV,
    team_features = team_features
  )

  message("Training model suite...")
  model_suite <- train_head_to_head_models(
    match_results_path = HISTORICAL_MATCHES_CSV,
    team_features = historical$team_features
  )

  message("Projecting group stage and bracket slots...")
  group_results <- project_group_results_from_features(team_features = historical$team_features)
  projected_slots <- build_projected_slot_assignments(group_results)

  write_csv(group_results, PROJECTED_GROUP_RESULTS_CSV)
  write_csv(projected_slots$slot_assignments, PROJECTED_SLOT_ASSIGNMENTS_CSV)

  message("Running bracket simulations for each fitted model...")
  results <- simulate_model_suite_brackets(
    model_suite = model_suite,
    team_features = historical$team_features,
    slot_assignments = projected_slots$slot_assignments,
    n_sims = n_sims,
    decision_rule = decision_rule
  )

  save_model_comparison_outputs(results, MODEL_OUT_DIR)

  print(results$summary)
  invisible(results)
}

if (identical(Sys.getenv("RUN_WC2026_MODEL_COMPARISON", unset = "0"), "1")) {
  main()
}
