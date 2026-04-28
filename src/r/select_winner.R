library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)

MODEL_OUT_DIR <- file.path("wc2026_outputs", "r_model_outputs")
SELECT_WINNER_OUT_DIR <- file.path(MODEL_OUT_DIR, "select_winner_outputs")
SELECT_WINNER_PLOTS_DIR <- file.path(SELECT_WINNER_OUT_DIR, "plots")
SELECT_WINNER_MODELS_DIR <- file.path(SELECT_WINNER_OUT_DIR, "models")
dir.create(SELECT_WINNER_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SELECT_WINNER_PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SELECT_WINNER_MODELS_DIR, recursive = TRUE, showWarnings = FALSE)

safe_log_prob <- function(x, epsilon = 1e-12) {
  log(pmax(dplyr::coalesce(x, epsilon), epsilon))
}

check_columns <- function(df, required, label) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

save_plot <- function(plot, filename, width, height) {
  ggsave(
    filename = file.path(SELECT_WINNER_PLOTS_DIR, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
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

slugify_model_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "model")
}

runs <- read_csv(file.path(MODEL_OUT_DIR, "model_sampled_matches.csv"), show_col_types = FALSE)
model_summary <- read_csv(file.path(MODEL_OUT_DIR, "model_comparison_summary.csv"), show_col_types = FALSE)
champion_probs <- read_csv(file.path(MODEL_OUT_DIR, "model_champion_probabilities.csv"), show_col_types = FALSE)
final_position_probs <- read_csv(file.path(MODEL_OUT_DIR, "model_final_position_probabilities.csv"), show_col_types = FALSE)

check_columns(
  runs,
  c("model_name", "sim_id", "match_number", "round", "winner", "winner_prob", "team_1", "team_2"),
  "model_sampled_matches.csv"
)
check_columns(
  model_summary,
  c("model_name", "accuracy", "roc_auc", "mn_log_loss"),
  "model_comparison_summary.csv"
)
check_columns(
  champion_probs,
  c("model_name", "team", "champion_prob"),
  "model_champion_probabilities.csv"
)
check_columns(
  final_position_probs,
  c("model_name", "team", "final_position", "prob"),
  "model_final_position_probabilities.csv"
)

model_order <- model_summary |>
  arrange(mn_log_loss, desc(roc_auc), desc(accuracy)) |>
  pull(model_name)

best_model_summary <- model_summary |>
  arrange(mn_log_loss, desc(roc_auc), desc(accuracy)) |>
  slice(1)

best_model_name <- best_model_summary$model_name[[1]]

model_summary_plot <- model_summary |>
  mutate(
    model_name = factor(model_name, levels = model_order),
    selected_model = model_name == best_model_name,
    metric_label = paste0(
      "AUC: ", number(roc_auc, accuracy = 0.001),
      "\nAcc: ", number(accuracy, accuracy = 0.001)
    )
  )

runs_plot <- runs |>
  mutate(model_name = factor(model_name, levels = model_order))

run_scores <- runs_plot |>
  group_by(model_name, sim_id) |>
  summarise(
    run_log_score = sum(safe_log_prob(winner_prob)),
    mean_winner_prob = mean(winner_prob, na.rm = TRUE),
    min_winner_prob = min(winner_prob, na.rm = TRUE),
    champion = winner[match_number == max(match_number)][1],
    .groups = "drop"
  ) |>
  arrange(desc(run_log_score), model_name, sim_id)

best_runs_by_model <- run_scores |>
  group_by(model_name) |>
  slice_max(run_log_score, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(model_name = factor(as.character(model_name), levels = model_order))

selected_run <- best_runs_by_model |>
  filter(as.character(model_name) == best_model_name) |>
  slice(1)

selected_run_matches <- runs_plot |>
  filter(as.character(model_name) == best_model_name, sim_id == selected_run$sim_id[[1]]) |>
  arrange(match_number) |>
  mutate(
    round = factor(
      round,
      levels = c(
        "Round of 32",
        "Round of 16",
        "Quarter-final",
        "Semi-final",
        "Bronze final",
        "Final"
      )
    ),
    match_label = factor(
      paste0(match_number, " | ", team_1, " vs ", team_2),
      levels = paste0(match_number, " | ", team_1, " vs ", team_2)
    )
  )

selected_champion_probs <- champion_probs |>
  filter(model_name == best_model_name) |>
  slice_max(champion_prob, n = 10, with_ties = FALSE) |>
  arrange(desc(champion_prob)) |>
  mutate(team = factor(team, levels = rev(team)))

top_heatmap_teams <- selected_champion_probs |>
  pull(team) |>
  as.character()

selected_final_position_probs <- final_position_probs |>
  filter(model_name == best_model_name, team %in% top_heatmap_teams) |>
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
  )

selection_summary <- best_model_summary |>
  mutate(
    best_run_sim_id = selected_run$sim_id[[1]],
    best_run_log_score = selected_run$run_log_score[[1]],
    best_run_mean_winner_prob = selected_run$mean_winner_prob[[1]],
    best_run_min_winner_prob = selected_run$min_winner_prob[[1]],
    best_run_champion = selected_run$champion[[1]]
  )

run_scores_out <- run_scores |>
  mutate(model_name = as.character(model_name))

best_runs_out <- best_runs_by_model |>
  mutate(model_name = as.character(model_name))

selected_run_matches_out <- selected_run_matches |>
  mutate(model_name = as.character(model_name)) |>
  select(model_name, sim_id, match_number, round, team_1, team_2, winner, winner_prob, conf_prob)

selected_champion_probs_out <- selected_champion_probs |>
  mutate(model_name = as.character(model_name))

selected_final_position_probs_out <- selected_final_position_probs |>
  mutate(model_name = as.character(model_name))

write_csv(run_scores_out, file.path(SELECT_WINNER_OUT_DIR, "run_scores.csv"))
write_csv(best_runs_out, file.path(SELECT_WINNER_OUT_DIR, "best_runs_by_model.csv"))
write_csv(selection_summary, file.path(SELECT_WINNER_OUT_DIR, "selection_summary.csv"))
write_csv(selected_run_matches_out, file.path(SELECT_WINNER_OUT_DIR, "selected_run_matches.csv"))
write_csv(selected_champion_probs_out, file.path(SELECT_WINNER_OUT_DIR, "selected_model_champion_probabilities.csv"))
write_csv(selected_final_position_probs_out, file.path(SELECT_WINNER_OUT_DIR, "selected_model_final_position_probabilities.csv"))

model_quality_plot <- ggplot(
  model_summary_plot,
  aes(x = model_name, y = mn_log_loss, fill = selected_model)
) +
  geom_col(width = 0.75) +
  geom_text(aes(label = metric_label), vjust = -0.35, size = 3.2) +
  scale_fill_manual(values = c(`TRUE` = "#d1495b", `FALSE` = "#4c78a8"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Model quality ranking",
    subtitle = paste0("Best model by log loss: ", best_model_name),
    x = NULL,
    y = "Mean log loss"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

run_score_plot <- ggplot(
  run_scores,
  aes(x = model_name, y = run_log_score)
) +
  geom_boxplot(aes(fill = model_name), alpha = 0.35, width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.12, size = 0.8, color = "grey35") +
  geom_point(
    data = best_runs_by_model,
    aes(x = model_name, y = run_log_score),
    inherit.aes = FALSE,
    color = "#d1495b",
    size = 3
  ) +
  geom_point(
    data = selection_summary,
    aes(x = factor(model_name, levels = model_order), y = best_run_log_score),
    inherit.aes = FALSE,
    color = "black",
    fill = "#ffd166",
    shape = 21,
    size = 4,
    stroke = 1.1
  ) +
  scale_fill_manual(values = setNames(rep("#4c78a8", length(model_order)), model_order), guide = "none") +
  labs(
    title = "Simulation run scores by model",
    subtitle = "Higher log score means the bracket path was more likely under the model",
    x = NULL,
    y = "Run log score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

champion_plot <- ggplot(
  selected_champion_probs,
  aes(x = team, y = champion_prob)
) +
  geom_col(fill = "#4c78a8", width = 0.7) +
  geom_text(aes(label = percent(champion_prob, accuracy = 0.1)), hjust = -0.08, size = 3.2) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0("Top champion probabilities: ", best_model_name),
    subtitle = "Based on the sampled bracket simulations",
    x = NULL,
    y = "Champion probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

best_run_plot <- ggplot(
  selected_run_matches,
  aes(x = winner_prob, y = match_label)
) +
  geom_col(fill = "#4c78a8", width = 0.7) +
  facet_wrap(~round, scales = "free_y", ncol = 1) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = paste0("Best run bracket confidence: ", best_model_name, " / sim ", selected_run$sim_id[[1]]),
    subtitle = "Each bar is the winner probability for the chosen side of the match",
    x = "Winner probability",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold")
  )

final_position_plot <- ggplot(
  selected_final_position_probs,
  aes(x = final_position, y = team, fill = prob)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = percent(prob, accuracy = 1)), size = 3) +
  scale_fill_gradient(
    low = "#f7fbff",
    high = "#08306b",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = paste0("Final position distribution: ", best_model_name),
    subtitle = "Top champion-probability teams from the selected model",
    x = NULL,
    y = NULL,
    fill = "Probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

save_plot(model_quality_plot, "model_quality_ranking.png", width = 8, height = 5)
save_plot(run_score_plot, "run_score_distribution.png", width = 8, height = 5)
save_plot(champion_plot, "selected_model_champion_probabilities.png", width = 8, height = 6)
save_plot(best_run_plot, "selected_best_run_bracket_confidence.png", width = 10, height = 13)
save_plot(final_position_plot, "selected_model_final_position_heatmap.png", width = 8, height = 6.5)

model_profiles <- list()

for (model_nm in model_order) {
  model_slug <- slugify_model_name(model_nm)
  model_dir <- file.path(SELECT_WINNER_MODELS_DIR, model_slug)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

  model_summary_row <- model_summary |>
    filter(model_name == model_nm)

  model_run_scores <- run_scores |>
    filter(as.character(model_name) == model_nm)

  model_best_run <- best_runs_by_model |>
    filter(as.character(model_name) == model_nm)

  model_champion_probs <- champion_probs |>
    filter(model_name == model_nm) |>
    arrange(desc(champion_prob), team)

  model_final_position_probs <- final_position_probs |>
    filter(model_name == model_nm) |>
    arrange(team, final_position)

  model_best_run_matches <- runs_plot |>
    filter(as.character(model_name) == model_nm, sim_id == model_best_run$sim_id[[1]]) |>
    arrange(match_number)

  model_profile <- model_summary_row |>
    mutate(
      n_sims = n_distinct(model_run_scores$sim_id),
      best_run_sim_id = model_best_run$sim_id[[1]],
      best_run_log_score = model_best_run$run_log_score[[1]],
      best_run_mean_winner_prob = model_best_run$mean_winner_prob[[1]],
      best_run_min_winner_prob = model_best_run$min_winner_prob[[1]],
      best_run_champion = model_best_run$champion[[1]],
      top_champion = model_champion_probs$team[[1]],
      top_champion_prob = model_champion_probs$champion_prob[[1]]
    ) |>
    mutate(model_name = as.character(model_name))

  model_profiles[[model_nm]] <- model_profile

  write_csv(model_profile, file.path(model_dir, "model_profile.csv"))
  write_csv(model_run_scores, file.path(model_dir, "run_scores.csv"))
  write_csv(model_champion_probs, file.path(model_dir, "champion_probabilities.csv"))
  write_csv(model_final_position_probs, file.path(model_dir, "final_position_probabilities.csv"))
  write_csv(
    model_best_run_matches |>
      mutate(model_name = as.character(model_name)) |>
      select(model_name, sim_id, match_number, round, team_1, team_2, winner, winner_prob, conf_prob),
    file.path(model_dir, "best_run_matches.csv")
  )

  if (nrow(model_champion_probs) > 0) {
    model_champion_plot <- ggplot(
      model_champion_probs |> slice_head(n = 10),
      aes(x = team, y = champion_prob)
    ) +
      geom_col(fill = "#4c78a8", width = 0.7) +
      geom_text(aes(label = percent(champion_prob, accuracy = 0.1)), hjust = -0.08, size = 3.2) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = paste0("Champion probabilities: ", model_nm),
        subtitle = "Top teams under this model",
        x = NULL,
        y = "Champion probability"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank()
      )

    save_plot_to(model_champion_plot, file.path(model_dir, "champion_probabilities.png"), width = 8, height = 6)
  }

  if (nrow(model_final_position_probs) > 0) {
    final_heatmap_teams <- model_champion_probs |>
      slice_head(n = 10) |>
      pull(team) |>
      as.character()

    model_final_heatmap <- model_final_position_probs |>
      filter(team %in% final_heatmap_teams) |>
      mutate(
        team = factor(team, levels = rev(final_heatmap_teams)),
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
      geom_tile(color = "white", linewidth = 0.3) +
      geom_text(aes(label = percent(prob, accuracy = 1)), size = 3) +
      scale_fill_gradient(
        low = "#f7fbff",
        high = "#08306b",
        labels = percent_format(accuracy = 1)
      ) +
      labs(
        title = paste0("Final position distribution: ", model_nm),
        subtitle = "Top champion-probability teams under this model",
        x = NULL,
        y = NULL,
        fill = "Probability"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank()
      )

    save_plot_to(model_final_heatmap, file.path(model_dir, "final_position_heatmap.png"), width = 8, height = 6.5)
  }

  if (nrow(model_run_scores) > 0) {
    best_model_run <- model_best_run$run_log_score[[1]]
    model_run_score_plot <- ggplot(
      model_run_scores,
      aes(x = run_log_score)
    ) +
      geom_histogram(bins = 20, fill = "#4c78a8", color = "white", alpha = 0.85) +
      geom_vline(xintercept = best_model_run, color = "#d1495b", linewidth = 1) +
      labs(
        title = paste0("Run score distribution: ", model_nm),
        subtitle = paste0("Best run = sim ", model_best_run$sim_id[[1]], " (red line)"),
        x = "Run log score",
        y = "Number of simulations"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank()
      )

    save_plot_to(model_run_score_plot, file.path(model_dir, "run_score_distribution.png"), width = 8, height = 5)
  }

  if (nrow(model_best_run_matches) > 0) {
    model_best_run_plot <- model_best_run_matches |>
      mutate(
        round = factor(
          round,
          levels = c(
            "Round of 32",
            "Round of 16",
            "Quarter-final",
            "Semi-final",
            "Bronze final",
            "Final"
          )
        ),
        match_label = factor(
          paste0(match_number, " | ", team_1, " vs ", team_2),
          levels = paste0(match_number, " | ", team_1, " vs ", team_2)
        )
      ) |>
      ggplot(aes(x = winner_prob, y = match_label)) +
      geom_col(fill = "#4c78a8", width = 0.7) +
      facet_wrap(~round, scales = "free_y", ncol = 1) +
      scale_x_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, 1),
        expand = expansion(mult = c(0, 0.03))
      ) +
      labs(
        title = paste0("Best run bracket confidence: ", model_nm, " / sim ", model_best_run$sim_id[[1]]),
        subtitle = "Winner probability for each match in the best run",
        x = "Winner probability",
        y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold")
      )

    save_plot_to(model_best_run_plot, file.path(model_dir, "best_run_bracket_confidence.png"), width = 10, height = 13)
  }
}

model_profiles <- bind_rows(model_profiles) |>
  arrange(mn_log_loss, desc(roc_auc), desc(accuracy))
write_csv(model_profiles, file.path(SELECT_WINNER_OUT_DIR, "model_profiles.csv"))

message("Best model: ", best_model_name)
message("Best run: sim ", selected_run$sim_id[[1]], " with log score ", round(selected_run$run_log_score[[1]], 3))
message("Outputs written to: ", SELECT_WINNER_OUT_DIR)
