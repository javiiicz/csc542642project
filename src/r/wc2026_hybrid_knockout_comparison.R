source("wc2026_model_comparison.R")

library(ggplot2)
library(scales)

HYBRID_KNOCKOUT_OUT_DIR <- file.path(MODEL_OUT_DIR, "hybrid_knockout_outputs")
HYBRID_KNOCKOUT_PLOTS_DIR <- file.path(HYBRID_KNOCKOUT_OUT_DIR, "plots")
HYBRID_KNOCKOUT_MODELS_DIR <- file.path(HYBRID_KNOCKOUT_OUT_DIR, "models")
HYBRID_KNOCKOUT_FULL_HISTORY_DIR <- file.path(HYBRID_KNOCKOUT_OUT_DIR, "full_history")

dir.create(HYBRID_KNOCKOUT_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(HYBRID_KNOCKOUT_PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(HYBRID_KNOCKOUT_MODELS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(HYBRID_KNOCKOUT_FULL_HISTORY_DIR, recursive = TRUE, showWarnings = FALSE)

SCORE_MODEL_RDS <- file.path(MODEL_OUT_DIR, "group_stage_outputs", "score_model.rds")
TRAINED_MODEL_SUITE_RDS <- file.path(MODEL_OUT_DIR, "trained_model_suite.rds")
FULL_HISTORY_MATCH_RESULTS_CSV <- file.path(MODEL_OUT_DIR, "group_stage_outputs", "historical_matches_combined.csv")
FULL_HISTORY_80_20_MATCH_RESULTS_CSV <- file.path(HYBRID_KNOCKOUT_OUT_DIR, "full_history_80_20", "historical_matches_combined_1672.csv")

build_full_history_80_20_match_history <- function(out_path = FULL_HISTORY_80_20_MATCH_RESULTS_CSV) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  base <- read_csv(FULL_HISTORY_MATCH_RESULTS_CSV, show_col_types = FALSE)
  base_core <- base |>
    transmute(
      team = normalize_transfermarkt_team_name(team),
      opponent = normalize_transfermarkt_team_name(opponent),
      team_goals = safe_num(team_goals),
      opponent_goals = safe_num(opponent_goals),
      match_date = as.Date(match_date),
      neutral = if_else(is.na(neutral), 1L, as.integer(neutral))
    )

  recent_tm <- read_csv(file.path(OUT_DIR, "historical_matches_transfermarkt_recent.csv"), show_col_types = FALSE)
  tm_new <- recent_tm |>
    transmute(
      team,
      opponent,
      team_goals = safe_num(team_goals),
      opponent_goals = safe_num(opponent_goals),
      match_date = as.Date(match_date),
      neutral = if_else(is.na(neutral), 1L, as.integer(neutral)),
      competition_id = "Transfermarkt recent",
      round = NA_character_,
      source = "transfermarkt_recent"
    ) |>
    anti_join(base_core, by = c("team", "opponent", "team_goals", "opponent_goals", "match_date", "neutral"))

  recent_h2h <- read_csv(H2H_MATCHES_CSV, show_col_types = FALSE) |>
    standardize_match_results()
  h2h_new <- recent_h2h |>
    transmute(
      team = normalize_transfermarkt_team_name(team),
      opponent = normalize_transfermarkt_team_name(opponent),
      team_goals = safe_num(team_goals),
      opponent_goals = safe_num(opponent_goals),
      match_date = as.Date(match_date),
      neutral = if_else(is.na(neutral), 1L, as.integer(neutral)),
      competition_id = if ("competition_type" %in% names(recent_h2h)) as.character(competition_type) else "h2h_recent",
      round = if ("stage" %in% names(recent_h2h)) as.character(stage) else NA_character_,
      source = "h2h_recent"
    ) |>
    anti_join(base_core, by = c("team", "opponent", "team_goals", "opponent_goals", "match_date", "neutral"))

  combined <- bind_rows(base, tm_new, h2h_new) |>
    distinct()

  write_csv(combined, out_path)
  combined
}

hybrid_paths <- function(output_root) {
  list(
    output_root = output_root,
    plots_dir = file.path(output_root, "plots"),
    models_dir = file.path(output_root, "models"),
    training_dir = file.path(output_root, "winner_training")
  )
}

save_plot_to <- function(plot, path, width, height) {
  ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

score_model_label <- function(score_model) {
  if (!is.null(score_model$metrics) &&
      "model" %in% names(score_model$metrics) &&
      length(score_model$metrics$model) > 0 &&
      !is.null(score_model$metrics$model[[1]])) {
    return(as.character(score_model$metrics$model[[1]]))
  }
  if (!is.null(score_model$model_name)) {
    return(as.character(score_model$model_name))
  }
  "score_model"
}

score_model_metadata <- function(score_model) {
  metrics <- if (!is.null(score_model$metrics)) as_tibble(score_model$metrics) else tibble()
  model_name <- score_model_label(score_model)

  if (!nrow(metrics)) {
    return(tibble(
      model_name = model_name,
      split_type = if (!is.null(score_model$split_type)) as.character(score_model$split_type) else NA_character_,
      rmse = NA_real_,
      mae = NA_real_,
      mean_actual_goals = NA_real_,
      mean_pred_goals = NA_real_,
      fallback_lambda = if (!is.null(score_model$fallback_lambda)) as.numeric(score_model$fallback_lambda) else NA_real_
    ))
  }

  metrics |>
    mutate(
      model_name = model_name,
      split_type = if (!is.null(score_model$split_type)) as.character(score_model$split_type) else NA_character_,
      fallback_lambda = if (!is.null(score_model$fallback_lambda)) as.numeric(score_model$fallback_lambda) else NA_real_
    ) |>
    select(model_name, split_type, rmse, mae, mean_actual_goals, mean_pred_goals, fallback_lambda)
}

named_model_objects <- function(model_suite) {
  if (!is.list(model_suite) || length(model_suite) == 0) {
    stop("trained_model_suite.rds did not contain a usable model list.", call. = FALSE)
  }

  if (!is.null(model_suite$models) && length(model_suite$models) > 0) {
    model_list <- model_suite$models
  } else {
    model_list <- model_suite
  }

  model_names <- purrr::map_chr(model_list, function(x) {
    if (!is.null(x$model_name)) return(as.character(x$model_name))
    if (!is.null(x$metrics) && "model" %in% names(x$metrics) && length(x$metrics$model) > 0) {
      return(as.character(x$metrics$model[[1]]))
    }
    "model"
  })
  names(model_list) <- model_names
  model_list
}

named_model_fits <- function(model_suite) {
  purrr::map(named_model_objects(model_suite), "fit")
}

model_resolution_breakdown <- function(matches) {
  matches |>
    count(stage, resolution, name = "n") |>
    group_by(stage) |>
    mutate(prob = n / sum(n)) |>
    ungroup() |>
    arrange(stage, desc(n))
}

run_one_hybrid_model <- function(
  score_model,
  winner_model = NULL,
  winner_model_name = NULL,
  history_scope = "recent_form",
  team_features,
  slot_assignments,
  n_sims = 100,
  decision_rule = c("sample", "most_probable"),
  seed = 642,
  winner_adjust_scale = 0.75
) {
  decision_rule <- match.arg(decision_rule)
  score_model_name <- score_model_label(score_model)
  history_scope <- as.character(history_scope)

  if (is.null(winner_model_name) || !nzchar(winner_model_name)) {
    if (is.null(winner_model)) {
      winner_model_name <- "score_only"
    } else if (!is.null(winner_model$model_name)) {
      winner_model_name <- as.character(winner_model$model_name)
    } else if (!is.null(winner_model$metrics) &&
               "model" %in% names(winner_model$metrics) &&
               length(winner_model$metrics$model) > 0 &&
               !is.null(winner_model$metrics$model[[1]])) {
      winner_model_name <- as.character(winner_model$metrics$model[[1]])
    } else {
      winner_model_name <- "winner_model"
    }
  }

  run_label <- if (is.null(winner_model)) "score_only" else winner_model_name
  winner_model_fit <- if (is.null(winner_model)) NULL else winner_model$fit

  deterministic <- simulate_fwc26_score_bracket(
    score_model = score_model,
    team_features = team_features,
    slot_assignments = slot_assignments,
    decision_rule = "most_probable",
    winner_model_fit = winner_model_fit,
    winner_model_name = winner_model_name,
    winner_adjust_scale = winner_adjust_scale
  )

  sampled_runs <- purrr::map(seq_len(n_sims), function(sim_id) {
    set.seed(seed + sim_id)
    sim <- simulate_fwc26_score_bracket(
      score_model = score_model,
      team_features = team_features,
      slot_assignments = slot_assignments,
      decision_rule = decision_rule,
      winner_model_fit = winner_model_fit,
      winner_model_name = winner_model_name,
      winner_adjust_scale = winner_adjust_scale
    )
    list(
      matches = sim$matches |>
        mutate(
          model_name = .env$run_label,
          score_model_name = .env$score_model_name,
          winner_model_name = .env$winner_model_name,
          history_scope = .env$history_scope,
          sim_id = sim_id
        ),
      progress = sim$team_progress |>
        mutate(
          model_name = .env$run_label,
          score_model_name = .env$score_model_name,
          winner_model_name = .env$winner_model_name,
          history_scope = .env$history_scope,
          sim_id = sim_id
        )
    )
  })

  deterministic_matches <- deterministic$matches |>
    mutate(
      model_name = .env$run_label,
      score_model_name = .env$score_model_name,
      winner_model_name = .env$winner_model_name,
      history_scope = .env$history_scope,
      sim_id = 0L
    )
  deterministic_progress <- deterministic$team_progress |>
    mutate(
      model_name = .env$run_label,
      score_model_name = .env$score_model_name,
      winner_model_name = .env$winner_model_name,
      history_scope = .env$history_scope,
      sim_id = 0L
    )

  sampled_matches <- bind_rows(purrr::map(sampled_runs, "matches"))
  sampled_progress <- bind_rows(purrr::map(sampled_runs, "progress"))

  champion_probs <- sampled_progress |>
    filter(final_position == "Champion") |>
    count(model_name, team, name = "champion_count") |>
    group_by(model_name) |>
    mutate(champion_prob = champion_count / sum(champion_count)) |>
    ungroup() |>
    arrange(model_name, desc(champion_prob), team) |>
    mutate(
      score_model_name = .env$score_model_name,
      winner_model_name = .env$winner_model_name,
      history_scope = .env$history_scope
    )

  final_position_probs <- sampled_progress |>
    count(model_name, team, final_position, name = "count") |>
    group_by(model_name, team) |>
    mutate(prob = count / sum(count)) |>
    ungroup() |>
    arrange(model_name, team, final_position) |>
    mutate(
      score_model_name = .env$score_model_name,
      winner_model_name = .env$winner_model_name,
      history_scope = .env$history_scope
    )

  resolution_tbl <- sampled_matches |>
    model_resolution_breakdown()

  accuracy_tbl <- if (!is.null(winner_model) &&
                      !is.null(winner_model$metrics) &&
                      "model" %in% names(winner_model$metrics)) {
    as_tibble(winner_model$metrics) |>
      mutate(model_name = .env$run_label) |>
      select(model_name, accuracy, roc_auc, mn_log_loss)
  } else {
    tibble(
      model_name = run_label,
      accuracy = NA_real_,
      roc_auc = NA_real_,
      mn_log_loss = NA_real_
    )
  }

  summary_tbl <- score_model_metadata(score_model) |>
    rename(score_model_name = model_name) |>
    mutate(
      model_name = .env$run_label,
      winner_model_name = .env$winner_model_name,
      history_scope = .env$history_scope,
      hybrid_mode = if (is.null(.env$winner_model)) "score_only" else "hybrid_score_plus_winner",
      deterministic_champion = deterministic_progress$team[deterministic_progress$final_position == "Champion"][1],
      deterministic_runner_up = deterministic_progress$team[deterministic_progress$final_position == "Runner-up"][1],
      sampled_champion = champion_probs$team[champion_probs$champion_prob == max(champion_probs$champion_prob, na.rm = TRUE)][1],
      sampled_champion_prob = max(champion_probs$champion_prob, na.rm = TRUE)
    ) |>
    left_join(accuracy_tbl, by = "model_name") |>
    select(
      model_name, score_model_name, winner_model_name, history_scope, hybrid_mode, accuracy, roc_auc, mn_log_loss,
      split_type, rmse, mae, mean_actual_goals, mean_pred_goals, fallback_lambda,
      deterministic_champion, deterministic_runner_up, sampled_champion, sampled_champion_prob
    )

  list(
    summary = summary_tbl,
    deterministic_matches = deterministic_matches,
    deterministic_progress = deterministic_progress,
    sampled_matches = sampled_matches,
    sampled_progress = sampled_progress,
    champion_probs = champion_probs,
    final_position_probs = final_position_probs,
    resolution_tbl = resolution_tbl
  )
}

save_hybrid_model_outputs <- function(model_name, result, output_root = HYBRID_KNOCKOUT_OUT_DIR) {
  paths <- hybrid_paths(output_root)
  dir.create(paths$output_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$models_dir, recursive = TRUE, showWarnings = FALSE)

  model_dir <- file.path(paths$models_dir, model_name)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

  plot_subtitle <- if (!is.null(result$summary$hybrid_mode) &&
                       length(result$summary$hybrid_mode) > 0 &&
                       identical(result$summary$hybrid_mode[[1]], "score_only")) {
    "Score-only knockout sims"
  } else {
    "Hybrid score + winner model knockout sims"
  }

  write_csv(result$deterministic_matches, file.path(model_dir, "deterministic_matches.csv"))
  write_csv(result$deterministic_progress, file.path(model_dir, "deterministic_team_progress.csv"))
  write_csv(result$sampled_matches, file.path(model_dir, "sampled_matches.csv"))
  write_csv(result$sampled_progress, file.path(model_dir, "sampled_team_progress.csv"))
  write_csv(result$champion_probs, file.path(model_dir, "champion_probabilities.csv"))
  write_csv(result$final_position_probs, file.path(model_dir, "final_position_probabilities.csv"))
  write_csv(result$resolution_tbl, file.path(model_dir, "resolution_breakdown.csv"))
  write_csv(result$summary, file.path(model_dir, "model_profile.csv"))

  champion_plot <- result$champion_probs |>
    slice_max(champion_prob, n = min(10, nrow(result$champion_probs)), with_ties = FALSE) |>
    mutate(team = factor(team, levels = rev(team))) |>
    ggplot(aes(x = champion_prob, y = team)) +
    geom_col(fill = "#4c78a8", width = 0.7) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = paste0("Champion probabilities from ", model_name),
      subtitle = plot_subtitle,
      x = "Champion probability",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )

  top_heatmap_teams <- result$champion_probs |>
    slice_max(champion_prob, n = min(10, nrow(result$champion_probs)), with_ties = FALSE) |>
    pull(team) |>
    as.character()

  final_heatmap <- result$final_position_probs |>
    filter(team %in% top_heatmap_teams) |>
    mutate(
      team = factor(team, levels = rev(top_heatmap_teams)),
      final_position = factor(
        final_position,
        levels = c(
          "Champion",
          "Runner-up",
          "Third place",
          "Fourth place",
          "Semi-final",
          "Quarter-final",
          "Round of 16",
          "Round of 32"
        )
      )
    ) |>
    ggplot(aes(x = final_position, y = team, fill = prob)) +
    geom_tile(color = "white", linewidth = 0.25) +
    geom_text(aes(label = percent(prob, accuracy = 1)), size = 3) +
    scale_fill_gradient(low = "#f7fbff", high = "#08306b", labels = percent_format(accuracy = 1)) +
    labs(
      title = paste0("Final-position probabilities from ", model_name),
      subtitle = plot_subtitle,
      x = NULL,
      y = NULL,
      fill = "Prob"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )

  resolution_plot <- result$resolution_tbl |>
    mutate(
      stage = factor(stage, levels = unique(stage)),
      resolution = factor(resolution, levels = c("regular", "extra_time", "penalties"))
    ) |>
    ggplot(aes(x = stage, y = prob, fill = resolution)) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c(regular = "#4c78a8", extra_time = "#f58518", penalties = "#54a24b")) +
    labs(
      title = paste0("Resolution mix for ", model_name),
      subtitle = plot_subtitle,
      x = NULL,
      y = "Share of matches",
      fill = "Resolution"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )

  save_plot_to(champion_plot, file.path(model_dir, "champion_probabilities.png"), 10, 7)
  save_plot_to(final_heatmap, file.path(model_dir, "final_position_heatmap.png"), 12, 10)
  save_plot_to(resolution_plot, file.path(model_dir, "resolution_breakdown.png"), 11, 7)
}

load_or_train_winner_models <- function(
  match_results_path,
  suite_rds_path = NULL,
  training_output_dir = NULL,
  team_features = NULL,
  force_train = FALSE,
  split_mode = c("auto", "latest_season", "time_split", "random_split"),
  split_prop = 0.8
) {
  split_mode <- match.arg(split_mode)

  if (!force_train && !is.null(suite_rds_path) && file.exists(suite_rds_path)) {
    return(named_model_objects(readRDS(suite_rds_path)))
  }

  if (is.null(training_output_dir)) {
    training_output_dir <- file.path(MODEL_OUT_DIR, "winner_training")
  }

  train_head_to_head_models(
    match_results_path = match_results_path,
    team_features = team_features,
    output_dir = training_output_dir,
    split_mode = split_mode,
    split_prop = split_prop
  ) |>
    named_model_objects()
}

run_hybrid_knockout_variant <- function(
  variant_label,
  output_root,
  score_model,
  team_features,
  slot_assignments,
  projected_group_results = NULL,
  projected_slot_assignments = NULL,
  match_results_path = NULL,
  suite_rds_path = NULL,
  training_output_dir = NULL,
  n_sims = 100,
  decision_rule = c("sample", "most_probable"),
  seed = 642,
  split_mode = c("auto", "latest_season", "time_split", "random_split"),
  split_prop = 0.8
) {
  assert_feature_packages()

  decision_rule <- match.arg(decision_rule)
  split_mode <- match.arg(split_mode)
  paths <- hybrid_paths(output_root)
  dir.create(paths$output_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$models_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$plots_dir, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(projected_group_results)) {
    write_csv(projected_group_results, file.path(paths$output_root, "projected_group_results.csv"))
  }
  if (!is.null(projected_slot_assignments)) {
    write_csv(projected_slot_assignments, file.path(paths$output_root, "projected_slot_assignments.csv"))
  }

  winner_models <- load_or_train_winner_models(
    match_results_path = match_results_path,
    suite_rds_path = suite_rds_path,
    training_output_dir = training_output_dir,
    team_features = team_features,
    force_train = identical(variant_label, "full_history") || identical(variant_label, "full_history_80_20"),
    split_mode = split_mode,
    split_prop = split_prop
  )

  write_csv(score_model_metadata(score_model), file.path(paths$output_root, "score_model_metadata.csv"))

  run_specs <- c(list(score_only = NULL), winner_models)

  combined_results <- purrr::imap(run_specs, function(winner_model, display_name) {
    message("Running hybrid bracket for: ", display_name, " [", variant_label, "]")
    result <- run_one_hybrid_model(
      score_model = score_model,
      winner_model = winner_model,
      winner_model_name = display_name,
      history_scope = variant_label,
      team_features = team_features,
      slot_assignments = slot_assignments,
      n_sims = n_sims,
      decision_rule = decision_rule,
      seed = seed
    )

    save_hybrid_model_outputs(display_name, result, output_root = paths$output_root)
    result$summary
  })

  summary_tbl <- bind_rows(combined_results)
  write_csv(summary_tbl, file.path(paths$output_root, "hybrid_model_summary.csv"))

  summary_tbl
}

run_hybrid_knockout_comparison <- function(n_sims = 100, decision_rule = c("sample", "most_probable"), seed = 642) {
  assert_feature_packages()

  decision_rule <- match.arg(decision_rule)

  if (!file.exists(SCORE_MODEL_RDS)) {
    stop("Missing score model RDS: ", SCORE_MODEL_RDS, call. = FALSE)
  }
  if (!file.exists(TRAINED_MODEL_SUITE_RDS)) {
    stop("Missing trained model suite RDS: ", TRAINED_MODEL_SUITE_RDS, call. = FALSE)
  }
  if (!file.exists(FULL_HISTORY_MATCH_RESULTS_CSV)) {
    stop("Missing full-history match CSV: ", FULL_HISTORY_MATCH_RESULTS_CSV, call. = FALSE)
  }

  message("Loading team features...")
  team_features <- read_csv(TEAM_CURRENT_FEATURES_CSV, show_col_types = FALSE)
  team_features <- complete_fwc26_team_features(team_features, wc2026_tracked_team_names())

  message("Loading score model and winner models...")
  score_model <- readRDS(SCORE_MODEL_RDS)
  recent_suite <- readRDS(TRAINED_MODEL_SUITE_RDS)

  message("Projecting group stage and knockout slots...")
  group_results <- project_group_results_from_features(team_features = team_features)
  projected_slots <- build_projected_slot_assignments(group_results)

  write_csv(group_results, file.path(HYBRID_KNOCKOUT_OUT_DIR, "projected_group_results.csv"))
  write_csv(projected_slots$slot_assignments, file.path(HYBRID_KNOCKOUT_OUT_DIR, "projected_slot_assignments.csv"))

  recent_summary <- run_hybrid_knockout_variant(
    variant_label = "recent_form",
    output_root = HYBRID_KNOCKOUT_OUT_DIR,
    score_model = score_model,
    team_features = team_features,
    slot_assignments = projected_slots$slot_assignments,
    projected_group_results = group_results,
    projected_slot_assignments = projected_slots$slot_assignments,
    match_results_path = MATCH_RESULTS_CSV,
    suite_rds_path = TRAINED_MODEL_SUITE_RDS,
    training_output_dir = MODEL_OUT_DIR,
    n_sims = n_sims,
    decision_rule = decision_rule,
    seed = seed
  )

  full_history_summary <- run_hybrid_knockout_variant(
    variant_label = "full_history",
    output_root = HYBRID_KNOCKOUT_FULL_HISTORY_DIR,
    score_model = score_model,
    team_features = team_features,
    slot_assignments = projected_slots$slot_assignments,
    projected_group_results = group_results,
    projected_slot_assignments = projected_slots$slot_assignments,
    match_results_path = FULL_HISTORY_MATCH_RESULTS_CSV,
    suite_rds_path = file.path(HYBRID_KNOCKOUT_FULL_HISTORY_DIR, "winner_training", "trained_model_suite.rds"),
    training_output_dir = file.path(HYBRID_KNOCKOUT_FULL_HISTORY_DIR, "winner_training"),
    n_sims = n_sims,
    decision_rule = decision_rule,
    seed = seed
  )

  build_full_history_80_20_match_history()

  full_history_80_20_summary <- run_hybrid_knockout_variant(
    variant_label = "full_history_80_20",
    output_root = file.path(HYBRID_KNOCKOUT_OUT_DIR, "full_history_80_20"),
    score_model = score_model,
    team_features = team_features,
    slot_assignments = projected_slots$slot_assignments,
    projected_group_results = group_results,
    projected_slot_assignments = projected_slots$slot_assignments,
    match_results_path = FULL_HISTORY_80_20_MATCH_RESULTS_CSV,
    suite_rds_path = file.path(HYBRID_KNOCKOUT_OUT_DIR, "full_history_80_20", "winner_training", "trained_model_suite.rds"),
    training_output_dir = file.path(HYBRID_KNOCKOUT_OUT_DIR, "full_history_80_20", "winner_training"),
    n_sims = n_sims,
    decision_rule = decision_rule,
    seed = seed,
    split_mode = "time_split",
    split_prop = 0.8
  )

  summary_tbl <- bind_rows(recent_summary, full_history_summary, full_history_80_20_summary)
  write_csv(summary_tbl, file.path(HYBRID_KNOCKOUT_OUT_DIR, "hybrid_model_summary_recent_vs_full.csv"))
  write_csv(summary_tbl, file.path(HYBRID_KNOCKOUT_OUT_DIR, "hybrid_model_summary_recent_vs_full_80_20.csv"))

  message("Hybrid knockout outputs written to: ", HYBRID_KNOCKOUT_OUT_DIR)

  invisible(summary_tbl)
}

main <- function(n_sims = 100, decision_rule = c("sample", "most_probable")) {
  decision_rule <- match.arg(decision_rule)
  run_hybrid_knockout_comparison(n_sims = n_sims, decision_rule = decision_rule)
}

if (identical(Sys.getenv("RUN_WC2026_HYBRID_KNOCKOUT", unset = "0"), "1")) {
  main()
}
