source("wc2026_model_comparison.R")

library(ggplot2)
library(scales)

SCORE_MODEL_RDS <- file.path(MODEL_OUT_DIR, "group_stage_outputs", "score_model.rds")
SCORE_KNOCKOUT_OUT_DIR <- file.path(MODEL_OUT_DIR, "score_knockout_outputs")
SCORE_KNOCKOUT_PLOTS_DIR <- file.path(SCORE_KNOCKOUT_OUT_DIR, "plots")

dir.create(SCORE_KNOCKOUT_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SCORE_KNOCKOUT_PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)

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
  "score_poisson_glm"
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

resolution_breakdown <- function(matches) {
  matches |>
    count(stage, resolution, name = "n") |>
    group_by(stage) |>
    mutate(prob = n / sum(n)) |>
    ungroup() |>
    arrange(stage, desc(n))
}

run_score_bracket <- function(n_sims = 200, decision_rule = c("sample", "most_probable"), seed = 642) {
  assert_feature_packages()

  decision_rule <- match.arg(decision_rule)
  score_model_name <- NULL

  if (!file.exists(SCORE_MODEL_RDS)) {
    stop(
      "Missing score model RDS: ",
      SCORE_MODEL_RDS,
      ". Run wc2026_group_stage_expected_points.R first.",
      call. = FALSE
    )
  }

  message("Loading current team features...")
  team_features <- read_csv(TEAM_CURRENT_FEATURES_CSV, show_col_types = FALSE)
  team_features <- complete_fwc26_team_features(team_features, wc2026_tracked_team_names())

  message("Loading score model...")
  score_model <- readRDS(SCORE_MODEL_RDS)
  score_model_name <- score_model_label(score_model)
  model_meta <- score_model_metadata(score_model)

  write_csv(model_meta, file.path(SCORE_KNOCKOUT_OUT_DIR, "score_model_metadata.csv"))

  message("Projecting group stage and knockout slots...")
  group_results <- project_group_results_from_features(team_features = team_features)
  projected_slots <- build_projected_slot_assignments(group_results)
  write_csv(group_results, file.path(SCORE_KNOCKOUT_OUT_DIR, "projected_group_results.csv"))
  write_csv(projected_slots$slot_assignments, file.path(SCORE_KNOCKOUT_OUT_DIR, "projected_slot_assignments.csv"))

  message("Running deterministic score-based bracket...")
  deterministic <- simulate_fwc26_score_bracket(
    score_model = score_model,
    team_features = team_features,
    slot_assignments = projected_slots$slot_assignments,
    decision_rule = "most_probable"
  )
  deterministic_matches <- deterministic$matches |>
    mutate(model_name = score_model_name, sim_id = 0L)
  deterministic_progress <- deterministic$team_progress |>
    mutate(model_name = score_model_name, sim_id = 0L)

  message("Running sampled score-based brackets...")
  sampled_runs <- purrr::map(seq_len(n_sims), function(sim_id) {
    set.seed(seed + sim_id)
    sim <- simulate_fwc26_score_bracket(
      score_model = score_model,
      team_features = team_features,
      slot_assignments = projected_slots$slot_assignments,
      decision_rule = decision_rule
    )
    list(
      matches = sim$matches |>
        mutate(model_name = score_model_name, sim_id = sim_id),
      progress = sim$team_progress |>
        mutate(model_name = score_model_name, sim_id = sim_id)
    )
  })

  sampled_matches <- bind_rows(purrr::map(sampled_runs, "matches"))
  sampled_progress <- bind_rows(purrr::map(sampled_runs, "progress"))

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

  resolution_tbl <- sampled_matches |>
    resolution_breakdown()

  summary_tbl <- model_meta |>
    mutate(
      best_deterministic_champion = deterministic_progress$team[deterministic_progress$final_position == "Champion"][1],
      best_deterministic_runner_up = deterministic_progress$team[deterministic_progress$final_position == "Runner-up"][1],
      sampled_champion = champion_probs$team[champion_probs$champion_prob == max(champion_probs$champion_prob, na.rm = TRUE)][1],
      sampled_champion_prob = max(champion_probs$champion_prob, na.rm = TRUE)
    )

  write_csv(deterministic_matches, file.path(SCORE_KNOCKOUT_OUT_DIR, "deterministic_matches.csv"))
  write_csv(deterministic_progress, file.path(SCORE_KNOCKOUT_OUT_DIR, "deterministic_team_progress.csv"))
  write_csv(sampled_matches, file.path(SCORE_KNOCKOUT_OUT_DIR, "sampled_matches.csv"))
  write_csv(sampled_progress, file.path(SCORE_KNOCKOUT_OUT_DIR, "sampled_team_progress.csv"))
  write_csv(champion_probs, file.path(SCORE_KNOCKOUT_OUT_DIR, "champion_probabilities.csv"))
  write_csv(final_position_probs, file.path(SCORE_KNOCKOUT_OUT_DIR, "final_position_probabilities.csv"))
  write_csv(resolution_tbl, file.path(SCORE_KNOCKOUT_OUT_DIR, "resolution_breakdown.csv"))
  write_csv(summary_tbl, file.path(SCORE_KNOCKOUT_OUT_DIR, "score_bracket_summary.csv"))

  champion_plot <- champion_probs |>
    slice_max(champion_prob, n = min(10, nrow(champion_probs)), with_ties = FALSE) |>
    mutate(team = factor(team, levels = rev(team))) |>
    ggplot(aes(x = champion_prob, y = team)) +
    geom_col(fill = "#4c78a8", width = 0.7) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = paste0("Champion probabilities from ", score_model_name),
      subtitle = paste0("Score model knockout sims = ", n_sims),
      x = "Champion probability",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )

  top_heatmap_teams <- champion_probs |>
    slice_max(champion_prob, n = min(10, nrow(champion_probs)), with_ties = FALSE) |>
    pull(team) |>
    as.character()

  final_heatmap <- final_position_probs |>
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
      title = paste0("Final-position probabilities from ", score_model_name),
      subtitle = "Score model knockout sims",
      x = NULL,
      y = NULL,
      fill = "Prob"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )

  resolution_plot <- resolution_tbl |>
    mutate(
      stage = factor(stage, levels = unique(stage)),
      resolution = factor(resolution, levels = c("regular", "extra_time", "penalties"))
    ) |>
    ggplot(aes(x = stage, y = prob, fill = resolution)) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c(regular = "#4c78a8", extra_time = "#f58518", penalties = "#54a24b")) +
    labs(
      title = "How score-model knockout matches are resolved",
      subtitle = paste0("Sampled sims = ", n_sims),
      x = NULL,
      y = "Share of matches",
      fill = "Resolution"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )

  save_plot_to(champion_plot, file.path(SCORE_KNOCKOUT_PLOTS_DIR, "champion_probabilities.png"), 10, 7)
  save_plot_to(final_heatmap, file.path(SCORE_KNOCKOUT_PLOTS_DIR, "final_position_heatmap.png"), 12, 10)
  save_plot_to(resolution_plot, file.path(SCORE_KNOCKOUT_PLOTS_DIR, "resolution_breakdown.png"), 11, 7)

  message("Score knockout outputs written to: ", SCORE_KNOCKOUT_OUT_DIR)

  invisible(list(
    model_name = score_model_name,
    deterministic_matches = deterministic_matches,
    deterministic_progress = deterministic_progress,
    sampled_matches = sampled_matches,
    sampled_progress = sampled_progress,
    champion_probs = champion_probs,
    final_position_probs = final_position_probs,
    resolution_tbl = resolution_tbl,
    summary = summary_tbl
  ))
}

main <- function(n_sims = 200, decision_rule = c("sample", "most_probable")) {
  decision_rule <- match.arg(decision_rule)
  run_score_bracket(n_sims = n_sims, decision_rule = decision_rule)
}

if (identical(Sys.getenv("RUN_WC2026_SCORE_KNOCKOUT", unset = "0"), "1")) {
  main()
}
