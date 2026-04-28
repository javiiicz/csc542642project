source("wc2026_model_comparison.R")

GROUP_STAGE_OUT_DIR <- file.path(MODEL_OUT_DIR, "group_stage_outputs")
GROUP_STAGE_PLOTS_DIR <- file.path(GROUP_STAGE_OUT_DIR, "plots")
dir.create(GROUP_STAGE_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(GROUP_STAGE_PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)

WORLDFOOTBALLR_COMPETITIONS <- c(
  "FIFA World Cup",
  "FIFA World Cup Qualification — AFC",
  "FIFA World Cup Qualification — CAF",
  "FIFA World Cup Qualification — CONCACAF",
  "FIFA World Cup Qualification — CONMEBOL",
  "FIFA World Cup Qualification — OFC",
  "FIFA World Cup Qualification — UEFA",
  "CONCACAF Gold Cup",
  "CONMEBOL Copa América",
  "Africa Cup of Nations",
  "AFC Asian Cup",
  "UEFA European Football Championship",
  "UEFA European Football Championship Qualifying",
  "International Friendlies (M)"
)

clean_worldfootballr_team_name <- function(x) {
  x <- str_squish(as.character(x))
  x <- gsub("\\s+[a-z]{2}$", "", x)
  x <- gsub("^[a-z]{2}\\s+", "", x)
  x <- normalize_transfermarkt_team_name(x)
  normalize_fwc26_team_name(x)
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

load_worldfootballr_history <- function(competition_names = WORLDFOOTBALLR_COMPETITIONS) {
  if (!requireNamespace("worldfootballR", quietly = TRUE)) {
    warning(
      "worldfootballR is not installed. Using the existing H2H file only.",
      call. = FALSE
    )
    return(tibble())
  }

  tracked <- wc2026_tracked_team_names()

  out <- purrr::map_dfr(competition_names, function(comp) {
    tryCatch(
      {
        worldfootballR::load_match_comp_results(comp_name = comp) |>
          as_tibble() |>
          mutate(source_competition = comp)
      },
      error = function(e) {
        warning("Skipping worldfootballR competition `", comp, "`: ", conditionMessage(e), call. = FALSE)
        tibble()
      }
    )
  })

  if (nrow(out) == 0) {
    return(tibble())
  }

  out <- out |>
    rename_if_present("Competition_Name", "competition_id") |>
    rename_if_present("Gender", "gender") |>
    rename_if_present("Country", "country") |>
    rename_if_present("Season_End_Year", "season_end_year") |>
    rename_if_present("Round", "round") |>
    rename_if_present("Wk", "week") |>
    rename_if_present("Date", "match_date") |>
    rename_if_present("Time", "match_time") |>
    rename_if_present("Home", "home_team") |>
    rename_if_present("HomeGoals", "home_goals") |>
    rename_if_present("Away", "away_team") |>
    rename_if_present("AwayGoals", "away_goals") |>
    rename_if_present("Venue", "venue") |>
    rename_if_present("MatchURL", "match_url")

  required <- c("home_team", "away_team", "home_goals", "away_goals", "match_date")
  missing <- setdiff(required, names(out))
  if (length(missing) > 0) {
    stop(
      "worldfootballR output is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out |>
    mutate(
      home_team = clean_worldfootballr_team_name(home_team),
      away_team = clean_worldfootballr_team_name(away_team),
      home_goals = safe_num(home_goals),
      away_goals = safe_num(away_goals),
      match_date = as.Date(match_date),
      competition_id = if ("competition_id" %in% names(out)) as.character(competition_id) else "worldfootballR",
      round = if ("round" %in% names(out)) as.character(round) else NA_character_,
      source = "worldfootballR"
    ) |>
    filter(
      home_team %in% tracked,
      away_team %in% tracked,
      !is.na(home_goals),
      !is.na(away_goals)
    ) |>
    transmute(
      team = home_team,
      opponent = away_team,
      team_goals = home_goals,
      opponent_goals = away_goals,
      match_date,
      neutral = 1L,
      competition_id,
      round,
      source
    )
}

load_h2h_history <- function() {
  if (!file.exists(H2H_MATCHES_CSV)) {
    warning("Missing H2H file: ", H2H_MATCHES_CSV, call. = FALSE)
    return(tibble())
  }

  tracked <- wc2026_tracked_team_names()

  match_std <- read_csv(H2H_MATCHES_CSV, show_col_types = FALSE) |>
    standardize_match_results()

  competition_id <- if ("competition_type" %in% names(match_std)) {
    as.character(match_std$competition_type)
  } else {
    rep("h2h", nrow(match_std))
  }

  round_val <- if ("stage" %in% names(match_std)) {
    as.character(match_std$stage)
  } else {
    rep(NA_character_, nrow(match_std))
  }

  match_std |>
    transmute(
      team = normalize_transfermarkt_team_name(team),
      opponent = normalize_transfermarkt_team_name(opponent),
      team_goals = safe_num(team_goals),
      opponent_goals = safe_num(opponent_goals),
      match_date = as.Date(match_date),
      neutral = if_else(is.na(neutral), 1L, as.integer(neutral)),
      competition_id = competition_id,
      round = round_val,
      source = "h2h"
    ) |>
    filter(
      team %in% tracked,
      opponent %in% tracked,
      !is.na(team_goals),
      !is.na(opponent_goals)
    )
}

build_combined_score_history <- function(out_path = file.path(OUT_DIR, "historical_matches_worldfootballr_h2h.csv")) {
  wf <- load_worldfootballr_history()
  h2h <- load_h2h_history()

  combined <- bind_rows(wf, h2h) |>
    mutate(source_rank = if_else(source == "h2h", 1L, 2L)) |>
    arrange(source_rank, match_date, competition_id, team, opponent) |>
    distinct(team, opponent, team_goals, opponent_goals, match_date, competition_id, round, .keep_all = TRUE) |>
    select(-source_rank) |>
    arrange(match_date, competition_id, team, opponent)

  write_csv(combined, out_path)
  combined
}

build_group_stage_fixtures <- function() {
  fixture_template <- tibble::tribble(
    ~matchday, ~team_a_seed, ~team_b_seed,
    1L, 1L, 2L,
    1L, 3L, 4L,
    2L, 1L, 3L,
    2L, 2L, 4L,
    3L, 1L, 4L,
    3L, 2L, 3L
  )

  group_teams <- fwc26_group_assignments() |>
    mutate(team_model = normalize_fwc26_team_name(team_name)) |>
    group_by(group) |>
    arrange(team_code, .by_group = TRUE) |>
    mutate(seed_in_group = row_number()) |>
    ungroup() |>
    select(group, team_code, team_name, team_model, seed_in_group)

  group_split <- split(group_teams, group_teams$group)

  purrr::imap_dfr(group_split, function(group_df, group_name) {
    group_df <- group_df |> arrange(seed_in_group)
    fixture_template |>
      mutate(
        group = group_name,
        team_1 = group_df$team_model[team_a_seed],
        team_2 = group_df$team_model[team_b_seed],
        team_1_name = group_df$team_name[team_a_seed],
        team_2_name = group_df$team_name[team_b_seed]
      ) |>
      select(group, matchday, team_1, team_2, team_1_name, team_2_name)
  })
}

build_score_recipe <- function(train_df) {
  recipe(team_goals ~ ., data = train_df) |>
    step_rm(
      any_of(c(
        "team", "opponent", "winner", "match_date", "season",
        "opponent_goals", "team_goal_diff", "season_num",
        "competition_id", "round", "source", "matchday",
        "game_id", "home_club_id", "away_club_id",
        "home_club_position", "away_club_position",
        "attendance"
      ))
    ) |>
    step_rm(matches("^season_num")) |>
    step_rm(matches("^feature_source")) |>
    step_rm(matches("^team_name")) |>
    step_rm(matches("^competition_type")) |>
    step_rm(matches("^competition_id")) |>
    step_rm(matches("^round")) |>
    step_rm(matches("^stage")) |>
    step_rm(matches("^slot")) |>
    step_rm(matches("^group")) |>
    step_rm(matches("^place")) |>
    step_rm(matches("^team_model")) |>
    step_rm(matches("^team_code")) |>
    step_rm(matches("^first_match")) |>
    step_rm(matches("^winner_slot")) |>
    step_rm(matches("^loser_slot")) |>
    step_rm(matches("^decision_rule")) |>
    step_rm(matches("^sim_id")) |>
    step_rm(matches("^model_name")) |>
    step_rm(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_impute_median(all_numeric_predictors()) |>
    step_lincomb(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors())
}

train_score_model <- function(history_df, team_features) {
  assert_training_packages()

  history_feat <- make_match_features(history_df, team_features, include_raw_metadata = TRUE) |>
    drop_na(team_goals)

  split <- choose_train_test_split(history_feat)
  train_df <- split$train
  test_df <- split$test

  if (is_optional_engine_available("poissonreg")) {
    score_spec <- poisson_reg(mode = "regression") |>
      set_engine("glm")
    model_name <- "score_poisson_glm"
  } else {
    warning(
      "poissonreg is not installed; falling back to linear_reg for the score model.",
      call. = FALSE
    )
    score_spec <- linear_reg(mode = "regression") |>
      set_engine("glm")
    model_name <- "score_linear_glm"
  }

  wf <- workflow() |>
    add_recipe(build_score_recipe(train_df)) |>
    add_model(score_spec)

  fit_obj <- fit(wf, data = train_df)
  pred <- predict(fit_obj, new_data = test_df, type = "numeric")

  pred_df <- bind_cols(
    test_df |> select(team, opponent, season, match_date, team_goals, opponent_goals, competition_id, round),
    pred
  )

  metrics_df <- tibble(
    model = model_name,
    rmse = rmse_vec(truth = pred_df$team_goals, estimate = pred_df$.pred),
    mae = mae_vec(truth = pred_df$team_goals, estimate = pred_df$.pred),
    mean_actual_goals = mean(pred_df$team_goals, na.rm = TRUE),
    mean_pred_goals = mean(pred_df$.pred, na.rm = TRUE)
  )

  list(
    split_type = split$split_type,
    train = train_df,
    test = test_df,
    fit = fit_obj,
    predictions = pred_df,
    metrics = metrics_df,
    fallback_lambda = mean(train_df$team_goals, na.rm = TRUE)
  )
}

predict_goal_lambda <- function(score_fit, team_a, team_b, team_features, fallback_lambda = 1.25) {
  fixture <- tibble(
    team = team_a,
    opponent = team_b,
    season = if ("latest_season" %in% names(team_features)) as.character(team_features$latest_season[[1]]) else NA_character_,
    neutral = 1L,
    competition_id = "group_stage",
    round = "Group stage",
    source = "projection"
  )

  feat <- make_match_features(fixture, team_features, include_raw_metadata = TRUE)
  pred <- tryCatch(
    predict(score_fit, new_data = feat, type = "numeric"),
    error = function(e) tibble(.pred = NA_real_)
  )
  lambda <- safe_num(pred$.pred[[1]])
  if (length(lambda) == 0 || all(is.na(lambda)) || !is.finite(lambda) || lambda < 0.05) {
    lambda <- fallback_lambda
  }
  lambda
}

simulate_group_fixture <- function(
  score_fit,
  team_a,
  team_b,
  team_features,
  fallback_lambda = 1.25,
  lambda_a = NULL,
  lambda_b = NULL
) {
  if (is.null(lambda_a)) {
    lambda_a <- predict_goal_lambda(score_fit, team_a, team_b, team_features, fallback_lambda = fallback_lambda)
  }
  if (is.null(lambda_b)) {
    lambda_b <- predict_goal_lambda(score_fit, team_b, team_a, team_features, fallback_lambda = fallback_lambda)
  }

  goals_a <- rpois(1, lambda_a)
  goals_b <- rpois(1, lambda_b)

  result_a <- case_when(
    goals_a > goals_b ~ "win",
    goals_a < goals_b ~ "loss",
    TRUE ~ "draw"
  )

  points_a <- case_when(
    result_a == "win" ~ 3L,
    result_a == "draw" ~ 1L,
    TRUE ~ 0L
  )
  points_b <- case_when(
    result_a == "loss" ~ 3L,
    result_a == "draw" ~ 1L,
    TRUE ~ 0L
  )
  winner <- case_when(
    goals_a > goals_b ~ team_a,
    goals_b > goals_a ~ team_b,
    TRUE ~ "Draw"
  )

  tibble(
    team_a = team_a,
    team_b = team_b,
    goals_a = goals_a,
    goals_b = goals_b,
    result_a = result_a,
    points_a = points_a,
    points_b = points_b,
    winner = winner,
    goal_diff_a = goals_a - goals_b,
    goal_diff_b = goals_b - goals_a,
    goals_for_a = goals_a,
    goals_for_b = goals_b
  )
}

simulate_group_stage_points <- function(score_fit, team_features, n_sims = 1000, seed = 642, fallback_lambda = 1.25) {
  fixtures <- build_group_stage_fixtures() |>
    mutate(
      lambda_a = purrr::map2_dbl(team_1, team_2, ~ predict_goal_lambda(score_fit, .x, .y, team_features, fallback_lambda = fallback_lambda)),
      lambda_b = purrr::map2_dbl(team_2, team_1, ~ predict_goal_lambda(score_fit, .x, .y, team_features, fallback_lambda = fallback_lambda))
    )
  simulate_one_group_stage <- function(sim_id) {
    set.seed(seed + sim_id)

    fixture_results <- fixtures |>
      mutate(sim_id = sim_id) |>
      rowwise() |>
      mutate(sim_result = list(simulate_group_fixture(
        score_fit = score_fit,
        team_a = team_1,
        team_b = team_2,
        team_features = team_features,
        fallback_lambda = fallback_lambda,
        lambda_a = lambda_a,
        lambda_b = lambda_b
      ))) |>
      ungroup() |>
      unnest(sim_result)

    match_results <- fixture_results |>
      transmute(
        sim_id,
        group,
        matchday,
        team_1,
        team_2,
        goals_1 = goals_a,
        goals_2 = goals_b,
        winner,
        lambda_1 = lambda_a,
        lambda_2 = lambda_b,
        points_1 = points_a,
        points_2 = points_b
      )

    team_rows <- bind_rows(
      fixture_results |>
        transmute(
          sim_id,
          group,
          matchday,
          team = team_1,
          opponent = team_2,
          goals_for = goals_a,
          goals_against = goals_b,
          goal_diff = goal_diff_a,
          points = points_a,
          result = result_a
        ),
      fixture_results |>
        transmute(
          sim_id,
          group,
          matchday,
          team = team_2,
          opponent = team_1,
          goals_for = goals_b,
          goals_against = goals_a,
          goal_diff = goal_diff_b,
          points = points_b,
          result = case_when(
            result_a == "win" ~ "loss",
            result_a == "loss" ~ "win",
            TRUE ~ "draw"
          )
        )
    )

    standings <- team_rows |>
      group_by(sim_id, group, team) |>
      summarise(
        points = sum(points, na.rm = TRUE),
        goal_diff = sum(goal_diff, na.rm = TRUE),
        goals_for = sum(goals_for, na.rm = TRUE),
        goals_against = sum(goals_against, na.rm = TRUE),
        wins = sum(result == "win", na.rm = TRUE),
        draws = sum(result == "draw", na.rm = TRUE),
        losses = sum(result == "loss", na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(sim_id, group) |>
      arrange(desc(points), desc(goal_diff), desc(goals_for), team, .by_group = TRUE) |>
      mutate(place = row_number()) |>
      ungroup() |>
      mutate(sim_result = "standings")

    list(
      match_results = match_results,
      standings = standings
    )
  }

  sim_list <- purrr::map(seq_len(n_sims), simulate_one_group_stage)
  match_results <- bind_rows(purrr::map(sim_list, "match_results"))
  sim_results <- bind_rows(purrr::map(sim_list, "standings"))

  expected_points <- sim_results |>
    group_by(group, team) |>
    summarise(
      expected_points = mean(points, na.rm = TRUE),
      expected_goal_diff = mean(goal_diff, na.rm = TRUE),
      expected_goals_for = mean(goals_for, na.rm = TRUE),
      expected_goals_against = mean(goals_against, na.rm = TRUE),
      win_prob = mean(wins / 3, na.rm = TRUE),
      draw_prob = mean(draws / 3, na.rm = TRUE),
      loss_prob = mean(losses / 3, na.rm = TRUE),
      mean_place = mean(place, na.rm = TRUE),
      top_two_prob = mean(place <= 2, na.rm = TRUE),
      group_win_prob = mean(place == 1, na.rm = TRUE),
      third_place_prob = mean(place == 3, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(group, desc(expected_points), desc(expected_goal_diff), desc(expected_goals_for), team)

  list(
    fixtures = fixtures,
    match_results = match_results,
    simulations = sim_results,
    expected_points = expected_points
  )
}

main <- function(n_sims = 1000, seed = 642) {
  assert_feature_packages()
  assert_training_packages()

  message("Loading team features...")
  team_features <- read_csv(TEAM_CURRENT_FEATURES_CSV, show_col_types = FALSE)
  team_features <- complete_fwc26_team_features(team_features, wc2026_tracked_team_names())

  message("Building combined score history...")
  history <- build_combined_score_history()

  if (nrow(history) == 0) {
    stop("No historical matches available after combining worldfootballR and H2H data.", call. = FALSE)
  }

  write_csv(history, file.path(GROUP_STAGE_OUT_DIR, "historical_matches_combined.csv"))

  message("Training score model...")
  score_model <- train_score_model(history, team_features)
  score_model_summary <- score_model$metrics |>
    mutate(split_type = score_model$split_type)
  write_csv(score_model_summary, file.path(GROUP_STAGE_OUT_DIR, "score_model_metrics.csv"))
  saveRDS(score_model, file.path(GROUP_STAGE_OUT_DIR, "score_model.rds"))

  message("Simulating group stage points...")
  group_sim <- simulate_group_stage_points(
    score_fit = score_model$fit,
    team_features = team_features,
    n_sims = n_sims,
    seed = seed,
    fallback_lambda = score_model$fallback_lambda
  )

  write_csv(group_sim$fixtures, file.path(GROUP_STAGE_OUT_DIR, "group_stage_fixtures.csv"))
  write_csv(group_sim$match_results, file.path(GROUP_STAGE_OUT_DIR, "group_stage_match_results.csv"))
  write_csv(group_sim$simulations, file.path(GROUP_STAGE_OUT_DIR, "group_stage_simulations.csv"))
  write_csv(group_sim$expected_points, file.path(GROUP_STAGE_OUT_DIR, "group_stage_expected_points.csv"))

  expected_points_plot <- group_sim$expected_points |>
    mutate(
      group = factor(group, levels = sort(unique(group))),
      team = factor(team, levels = rev(unique(team)))
    ) |>
    ggplot(aes(x = group, y = team, fill = expected_points)) +
    geom_tile(color = "white", linewidth = 0.25) +
    geom_text(aes(label = round(expected_points, 1)), size = 3) +
    scale_fill_gradient(
      low = "#f7fbff",
      high = "#08306b"
    ) +
    labs(
      title = "Expected group-stage points",
      subtitle = paste0("Score model trained on worldfootballR + H2H data; sims = ", n_sims),
      x = NULL,
      y = NULL,
      fill = "Pts"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )

  top_two_plot <- group_sim$expected_points |>
    mutate(
      group = factor(group, levels = sort(unique(group))),
      team = factor(team, levels = rev(unique(team)))
    ) |>
    ggplot(aes(x = top_two_prob, y = team)) +
    geom_col(fill = "#4c78a8", width = 0.7) +
    facet_wrap(~group, scales = "free_y", ncol = 3) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Probability of finishing top two in each group",
      subtitle = "Based on score simulations",
      x = "Top-two probability",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold")
    )

  save_plot_to(expected_points_plot, file.path(GROUP_STAGE_PLOTS_DIR, "group_stage_expected_points_heatmap.png"), 12, 10)
  save_plot_to(top_two_plot, file.path(GROUP_STAGE_PLOTS_DIR, "group_stage_top_two_probabilities.png"), 12, 10)

  message("Group-stage outputs written to: ", GROUP_STAGE_OUT_DIR)
  invisible(group_sim)
}

if (identical(Sys.getenv("RUN_WC2026_GROUP_STAGE_POINTS", unset = "0"), "1")) {
  main()
}
