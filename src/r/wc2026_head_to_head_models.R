# WC 2026 head-to-head modeling pipeline
#
# What this script does:
# 1. Loads the current player-level dataset from `wc2026_outputs/understat_tm_merged_specific_nation_filtered.csv`.
# 2. Collapses it into nation-season features built around the xG story from the notebook:
#    - total xG / non-penalty xG
#    - xG per 90 / xA per 90 / shots per 90 / key passes per 90
#    - xG concentration among top players
#    - attacking depth and position mix
#    - caps, market value, and age proxies
# 3. Creates a current-form nation table for 2026 simulation.
# 4. Provides model-training helpers for historical match data.
# 5. Fits the current project model suite in R:
#    - random forest (`ranger`, if installed)
#    - gradient boosting (`xgboost`, if installed)
#
# Expected match-results input for training:
# - Either a winner column:
#     team, opponent, winner, season, match_date (optional), neutral (optional)
# - Or score columns:
#     team, opponent, team_goals, opponent_goals, season, match_date (optional), neutral (optional)
#
# For knockout brackets, the cleanest target is `winner`.
#
# Suggested install:
# install.packages(c("dplyr", "readr", "stringr", "tibble", "tidyr", "lubridate", "purrr", "tidymodels"))

load_optional_package <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    return(TRUE)
  }
  warning("Package not installed: ", pkg, call. = FALSE)
  FALSE
}

have_tidymodels <- load_optional_package("tidymodels")
have_dplyr <- load_optional_package("dplyr")
have_stringr <- load_optional_package("stringr")
have_tibble <- load_optional_package("tibble")
have_tidyr <- load_optional_package("tidyr")
have_readr <- load_optional_package("readr")
have_lubridate <- load_optional_package("lubridate")
have_purrr <- load_optional_package("purrr")

assert_feature_packages <- function() {
  required <- c("dplyr", "readr", "stringr", "lubridate", "purrr", "tibble", "tidyr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Feature-engineering functions need these packages installed: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

assert_training_packages <- function() {
  required <- c("tidymodels", "readr", "lubridate", "purrr", "tidyr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Training/model functions need these packages installed: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

set.seed(642)

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------

BASE_DIR <- getwd()
OUT_DIR <- file.path(BASE_DIR, "wc2026_outputs")
MODEL_OUT_DIR <- file.path(OUT_DIR, "r_model_outputs")
dir.create(MODEL_OUT_DIR, recursive = TRUE, showWarnings = FALSE)

PLAYER_CSV <- file.path(OUT_DIR, "understat_tm_merged_specific_nation_filtered.csv")
TEAM_SEASON_FEATURES_CSV <- file.path(OUT_DIR, "team_season_features.csv")
TEAM_CURRENT_FEATURES_CSV <- file.path(OUT_DIR, "team_current_features.csv")
MATCH_RESULTS_CSV <- file.path(OUT_DIR, "national_team_games_h2h_wc_teams_2018_2026.csv")

# Use current date by default; override by exporting WC2026_REFERENCE_DATE if needed.
REFERENCE_DATE <- as.Date(Sys.getenv("WC2026_REFERENCE_DATE", unset = as.character(Sys.Date())))

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------

safe_num <- function(x) suppressWarnings(as.numeric(x))

pct <- function(num, den) {
  den <- suppressWarnings(as.numeric(den))
  num <- suppressWarnings(as.numeric(num))
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

weighted_mean_na <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

top_n_sum <- function(x, n = 3) {
  x <- replace_na(safe_num(x), 0)
  if (!length(x)) return(NA_real_)
  sum(head(sort(x, decreasing = TRUE), n))
}

top_n_share <- function(x, n = 3) {
  x <- replace_na(safe_num(x), 0)
  total <- sum(x)
  if (is.na(total) || total <= 0) return(NA_real_)
  sum(head(sort(x, decreasing = TRUE), n)) / total
}

hhi_share <- function(x) {
  x <- replace_na(safe_num(x), 0)
  total <- sum(x)
  if (is.na(total) || total <= 0) return(NA_real_)
  shares <- x / total
  sum(shares^2)
}

normalize_position_bucket <- function(pos) {
  pos <- str_squish(as.character(pos))
  pos_lower <- str_to_lower(pos)
  case_when(
    str_detect(pos_lower, "goalkeeper") ~ "Goalkeeper",
    str_detect(pos_lower, "defender") ~ "Defender",
    str_detect(pos_lower, "midfield") ~ "Midfield",
    str_detect(pos_lower, "attack|forward") ~ "Attack",
    TRUE ~ NA_character_
  )
}

derive_season_code <- function(match_date) {
  match_date <- as.Date(match_date)
  year_num <- year(match_date)
  month_num <- month(match_date)
  start_year <- ifelse(month_num >= 8, year_num, year_num - 1)
  sprintf("%02d%02d", start_year %% 100, (start_year + 1) %% 100)
}

rename_if_present <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) {
    names(df)[names(df) == old] <- new
  }
  df
}

fwc26_team_name_aliases <- function() {
  tibble(
    schedule_name = c(
      "Korea Republic",
      "Czechia",
      "Bosnia and Herzegovina",
      "Cabo Verde",
      "Congo DR"
    ),
    model_name = c(
      "Korea, South",
      "Czech Republic",
      "Bosnia-Herzegovina",
      "Cape Verde",
      "DR Congo"
    )
  )
}

normalize_fwc26_team_name <- function(x) {
  x <- str_squish(as.character(x))
  aliases <- fwc26_team_name_aliases()
  idx <- match(x, aliases$schedule_name)
  out <- x
  keep <- !is.na(idx)
  out[keep] <- aliases$model_name[idx[keep]]
  out
}

is_optional_engine_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

# ------------------------------------------------------------------------------
# Load and clean player data
# ------------------------------------------------------------------------------

load_player_data <- function(path = PLAYER_CSV, reference_date = REFERENCE_DATE) {
  assert_feature_packages()

  if (!file.exists(path)) {
    stop("Player CSV not found: ", path, call. = FALSE)
  }

  df <- read_csv(path, show_col_types = FALSE)

  required_cols <- c(
    "nation", "player", "season", "minutes", "xg", "np_xg", "xa",
    "shots", "key_passes", "xgchain", "xgbuildup", "goals"
  )
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(
      "Player CSV is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  numeric_cols <- c(
    "season", "minutes", "matches", "goals", "xg", "np_goals", "np_xg",
    "assists", "xa", "shots", "key_passes", "yellow_cards", "red_cards",
    "xgchain", "xgbuildup", "npg", "xg_plus_xa", "npg_plus_xa", "xg_per90",
    "xa_per90", "xgchain_per90", "xgbuildup_per90", "npg_per90", "goals_per90",
    "shots_per90", "key_passes_per90", "assists_per90", "sb_minutes", "sb_goals",
    "sb_xg", "sb_np_xg", "sb_shots", "sb_np_shots", "sb_assists", "sb_xa",
    "sb_key_passes", "sb_progressive_passes", "sb_progressive_carries",
    "tm_last_season", "tm_current_club_id", "tm_height_in_cm",
    "tm_international_caps", "tm_international_goals", "tm_current_national_team_id",
    "tm_market_value_in_eur", "tm_highest_market_value_in_eur", "tm_match_score"
  )

  df <- df |>
    mutate(
      across(any_of(numeric_cols), safe_num),
      player = str_squish(as.character(player)),
      nation = str_squish(as.character(nation)),
      season = str_squish(as.character(season)),
      tm_position = str_squish(as.character(tm_position)),
      tm_sub_position = str_squish(as.character(tm_sub_position)),
      tm_position_bucket = normalize_position_bucket(tm_position),
      tm_date_of_birth = suppressWarnings(as.Date(tm_date_of_birth)),
      age_years = if_else(
        !is.na(tm_date_of_birth),
        time_length(interval(tm_date_of_birth, reference_date), "years"),
        NA_real_
      ),
      log_market_value = log1p(tm_market_value_in_eur),
      log_caps = log1p(tm_international_caps),
      log_international_goals = log1p(tm_international_goals)
    ) |>
    arrange(nation, player, desc(minutes)) |>
    distinct(nation, player, season, .keep_all = TRUE)

  df
}

# ------------------------------------------------------------------------------
# Feature engineering
# ------------------------------------------------------------------------------

make_team_season_features <- function(player_df) {
  assert_feature_packages()

  if (!all(c("nation", "season") %in% names(player_df))) {
    stop("player_df must contain nation and season columns.", call. = FALSE)
  }

  player_df <- player_df |>
    mutate(
      minutes = replace_na(minutes, 0),
      tm_position_bucket = replace_na(tm_position_bucket, "Unknown")
    )

  out <- player_df |>
    group_by(nation, season) |>
    summarise(
      players_n = n_distinct(player),
      rows_n = n(),
      minutes_total = sum(minutes, na.rm = TRUE),
      matches_total = sum(matches, na.rm = TRUE),
      goals_total = sum(goals, na.rm = TRUE),
      xg_total = sum(xg, na.rm = TRUE),
      np_goals_total = sum(np_goals, na.rm = TRUE),
      np_xg_total = sum(np_xg, na.rm = TRUE),
      assists_total = sum(assists, na.rm = TRUE),
      xa_total = sum(xa, na.rm = TRUE),
      shots_total = sum(shots, na.rm = TRUE),
      key_passes_total = sum(key_passes, na.rm = TRUE),
      xgchain_total = sum(xgchain, na.rm = TRUE),
      xgbuildup_total = sum(xgbuildup, na.rm = TRUE),
      xg_plus_xa_total = sum(xg_plus_xa, na.rm = TRUE),
      npg_plus_xa_total = sum(npg_plus_xa, na.rm = TRUE),
      yellow_cards_total = sum(yellow_cards, na.rm = TRUE),
      red_cards_total = sum(red_cards, na.rm = TRUE),
      market_value_total = sum(tm_market_value_in_eur, na.rm = TRUE),
      market_value_top5_sum = top_n_sum(tm_market_value_in_eur, 5),
      market_value_top3_sum = top_n_sum(tm_market_value_in_eur, 3),
      market_value_top1 = top_n_sum(tm_market_value_in_eur, 1),
      market_value_hhi = hhi_share(tm_market_value_in_eur),
      caps_total = sum(tm_international_caps, na.rm = TRUE),
      international_goals_total = sum(tm_international_goals, na.rm = TRUE),
      top1_xg = top_n_sum(xg, 1),
      top3_xg = top_n_sum(xg, 3),
      top5_xg = top_n_sum(xg, 5),
      top1_np_xg = top_n_sum(np_xg, 1),
      top3_np_xg = top_n_sum(np_xg, 3),
      top5_np_xg = top_n_sum(np_xg, 5),
      top1_xg_share = top_n_share(xg, 1),
      top3_xg_share = top_n_share(xg, 3),
      top5_xg_share = top_n_share(xg, 5),
      top1_np_xg_share = top_n_share(np_xg, 1),
      top3_np_xg_share = top_n_share(np_xg, 3),
      top5_np_xg_share = top_n_share(np_xg, 5),
      xg_hhi = hhi_share(xg),
      np_xg_hhi = hhi_share(np_xg),
      players_over_1000_min = sum(minutes >= 1000, na.rm = TRUE),
      players_over_500_min = sum(minutes >= 500, na.rm = TRUE),
      players_over_250_min = sum(minutes >= 250, na.rm = TRUE),
      players_xg_gt_1 = sum(xg >= 1, na.rm = TRUE),
      players_xg_gt_3 = sum(xg >= 3, na.rm = TRUE),
      players_xg_gt_5 = sum(xg >= 5, na.rm = TRUE),
      goalkeeper_minutes = sum(minutes[tm_position_bucket == "Goalkeeper"], na.rm = TRUE),
      defender_minutes = sum(minutes[tm_position_bucket == "Defender"], na.rm = TRUE),
      midfield_minutes = sum(minutes[tm_position_bucket == "Midfield"], na.rm = TRUE),
      attack_minutes = sum(minutes[tm_position_bucket == "Attack"], na.rm = TRUE),
      age_mean = weighted_mean_na(age_years, minutes),
      age_sd = sd(age_years, na.rm = TRUE),
      age_min = suppressWarnings(min(age_years, na.rm = TRUE)),
      age_max = suppressWarnings(max(age_years, na.rm = TRUE)),
      market_value_mean = weighted_mean_na(tm_market_value_in_eur, minutes),
      market_value_sd = sd(tm_market_value_in_eur, na.rm = TRUE),
      caps_mean = weighted_mean_na(tm_international_caps, minutes),
      caps_sd = sd(tm_international_caps, na.rm = TRUE),
      international_goals_mean = weighted_mean_na(tm_international_goals, minutes),
      international_goals_sd = sd(tm_international_goals, na.rm = TRUE),
      xg_per90_weighted = weighted_mean_na(xg_per90, minutes),
      xa_per90_weighted = weighted_mean_na(xa_per90, minutes),
      shots_per90_weighted = weighted_mean_na(shots_per90, minutes),
      key_passes_per90_weighted = weighted_mean_na(key_passes_per90, minutes),
      xgchain_per90_weighted = weighted_mean_na(xgchain_per90, minutes),
      xgbuildup_per90_weighted = weighted_mean_na(xgbuildup_per90, minutes),
      goals_per90_weighted = weighted_mean_na(goals_per90, minutes),
      assists_per90_weighted = weighted_mean_na(assists_per90, minutes),
      .groups = "drop"
    ) |>
    mutate(
      season_num = safe_num(season),
      minutes_per_player = pct(minutes_total, players_n),
      goals_minus_xg = goals_total - xg_total,
      goals_minus_np_xg = goals_total - np_xg_total,
      xg_per90 = pct(xg_total, minutes_total) * 90,
      np_xg_per90 = pct(np_xg_total, minutes_total) * 90,
      xa_per90 = pct(xa_total, minutes_total) * 90,
      shots_per90 = pct(shots_total, minutes_total) * 90,
      key_passes_per90 = pct(key_passes_total, minutes_total) * 90,
      xgchain_per90 = pct(xgchain_total, minutes_total) * 90,
      xgbuildup_per90 = pct(xgbuildup_total, minutes_total) * 90,
      goals_per90 = pct(goals_total, minutes_total) * 90,
      assists_per90 = pct(assists_total, minutes_total) * 90,
      xg_per_shot = pct(xg_total, shots_total),
      np_xg_per_shot = pct(np_xg_total, shots_total),
      attack_minutes_share = pct(attack_minutes, minutes_total),
      midfield_minutes_share = pct(midfield_minutes, minutes_total),
      defender_minutes_share = pct(defender_minutes, minutes_total),
      goalkeeper_minutes_share = pct(goalkeeper_minutes, minutes_total),
      attack_depth_index = pct(players_over_500_min, players_n)
    ) |>
    arrange(desc(xg_per90), desc(xg_total))

  out
}

make_current_team_features <- function(team_season_features, method = c("weighted", "latest"), decay = 0.75) {
  assert_feature_packages()

  method <- match.arg(method)

  if (!"season_num" %in% names(team_season_features)) {
    team_season_features <- team_season_features |>
      mutate(season_num = safe_num(season))
  }

  if (method == "latest") {
    return(
      team_season_features |>
        group_by(nation) |>
        slice_max(order_by = season_num, n = 1, with_ties = FALSE) |>
        ungroup()
    )
  }

  max_season <- max(team_season_features$season_num, na.rm = TRUE)
  weighted <- team_season_features |>
    mutate(season_weight = decay^(max_season - season_num))

  numeric_cols <- names(weighted)[vapply(weighted, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, c("season_num", "season_weight"))

  weighted |>
    group_by(nation) |>
    summarise(
      seasons_covered = n_distinct(season),
      latest_season = max(season_num, na.rm = TRUE),
      across(all_of(numeric_cols), ~ weighted_mean_na(.x, season_weight)),
      .groups = "drop"
    )
}

write_feature_tables <- function(player_df = NULL, reference_date = REFERENCE_DATE) {
  if (is.null(player_df)) {
    player_df <- load_player_data(reference_date = reference_date)
  }

  team_season_features <- make_team_season_features(player_df)
  team_current_features <- make_current_team_features(team_season_features, method = "weighted")

  write_csv(team_season_features, TEAM_SEASON_FEATURES_CSV)
  write_csv(team_current_features, TEAM_CURRENT_FEATURES_CSV)

  invisible(list(
    player_df = player_df,
    team_season_features = team_season_features,
    team_current_features = team_current_features
  ))
}

# ------------------------------------------------------------------------------
# Match data cleaning
# ------------------------------------------------------------------------------

standardize_match_results <- function(match_df) {
  assert_feature_packages()

  match_df <- as_tibble(match_df)

  rename_map <- list(
    team_a = "team",
    home_team = "team",
    team1 = "team",
    team_b = "opponent",
    away_team = "opponent",
    team2 = "opponent",
    home_wc = "team",
    away_wc = "opponent",
    score_a = "team_goals",
    home_goals = "team_goals",
    goals_a = "team_goals",
    home_club_goals = "team_goals",
    score_b = "opponent_goals",
    away_goals = "opponent_goals",
    goals_b = "opponent_goals",
    away_club_goals = "opponent_goals",
    date = "match_date",
    match_day = "match_date",
    advancing_team = "winner",
    winning_team = "winner"
  )

  for (old in names(rename_map)) {
    match_df <- rename_if_present(match_df, old, rename_map[[old]])
  }

  if (!("team" %in% names(match_df) && "opponent" %in% names(match_df))) {
    stop("Match data must contain team and opponent columns (or compatible synonyms).", call. = FALSE)
  }

  if (!"neutral" %in% names(match_df)) {
    match_df$neutral <- 1
  }

  neutral_num <- safe_num(match_df$neutral)

  match_df <- match_df |>
    mutate(
      team = normalize_fwc26_team_name(team),
      opponent = normalize_fwc26_team_name(opponent),
      winner = if ("winner" %in% names(match_df)) normalize_fwc26_team_name(winner) else NA_character_,
      match_date = if ("match_date" %in% names(match_df)) as.Date(match_date) else as.Date(NA),
      season = if ("season" %in% names(match_df)) str_squish(as.character(season)) else NA_character_,
      neutral = ifelse(is.na(neutral_num), 0L, as.integer(neutral_num != 0))
    )

  if (!("season" %in% names(match_df)) || all(is.na(match_df$season) | match_df$season == "")) {
    if (any(!is.na(match_df$match_date))) {
      match_df <- match_df |>
        mutate(season = derive_season_code(match_date))
    }
  }

  if ("team_goals" %in% names(match_df)) {
    match_df <- match_df |> mutate(team_goals = safe_num(team_goals))
  }
  if ("opponent_goals" %in% names(match_df)) {
    match_df <- match_df |> mutate(opponent_goals = safe_num(opponent_goals))
  }

  if (!("team_win" %in% names(match_df))) {
    if ("winner" %in% names(match_df) && any(!is.na(match_df$winner) & match_df$winner != "")) {
      match_df <- match_df |>
        mutate(team_win = if_else(team == winner, 1L, 0L))
    } else if (all(c("team_goals", "opponent_goals") %in% names(match_df))) {
      match_df <- match_df |>
        mutate(team_win = if_else(team_goals > opponent_goals, 1L, 0L))
    } else {
      # Prediction fixtures can arrive without outcomes; keep them as NA and
      # let downstream code score the matchup.
      match_df <- match_df |> mutate(team_win = NA_integer_)
    }
  } else {
    if (is.factor(match_df$team_win) || is.character(match_df$team_win)) {
      team_win_chr <- tolower(str_squish(as.character(match_df$team_win)))
      match_df <- match_df |>
        mutate(team_win = if_else(team_win_chr %in% c("win", "1", "true", "t"), 1L, 0L))
    } else {
      match_df <- match_df |> mutate(team_win = safe_num(team_win))
    }
  }

  match_df <- match_df |>
    mutate(
      team_win = factor(if_else(team_win == 1, "win", "loss"), levels = c("loss", "win")),
      season_num = safe_num(season),
      team_goals = if ("team_goals" %in% names(match_df)) team_goals else NA_real_,
      opponent_goals = if ("opponent_goals" %in% names(match_df)) opponent_goals else NA_real_
    )

  match_df
}

make_match_features <- function(match_df, team_features, include_raw_metadata = TRUE) {
  assert_feature_packages()

  if (!all(c("team", "opponent") %in% names(match_df))) {
    stop("match_df must contain team and opponent columns.", call. = FALSE)
  }
  if (!"nation" %in% names(team_features)) {
    stop("team_features must contain a nation column.", call. = FALSE)
  }

  match_df <- standardize_match_results(match_df)

  team_features <- as_tibble(team_features)
  if ("season" %in% names(team_features)) {
    team_features <- team_features |>
      mutate(season = as.character(season))
  }
  numeric_features <- names(team_features)[vapply(team_features, is.numeric, logical(1))]
  numeric_features <- setdiff(numeric_features, c("season_num", "season"))

  join_by_season <- "season" %in% names(team_features) && any(!is.na(match_df$season))

  join_team <- team_features
  join_opp <- team_features

  if (join_by_season) {
    join_team <- join_team |>
      rename_with(~ paste0(.x, "_team"), all_of(numeric_features))
    join_opp <- join_opp |>
      rename_with(~ paste0(.x, "_opp"), all_of(numeric_features))

    out <- match_df |>
      left_join(join_team, by = c("team" = "nation", "season" = "season")) |>
      left_join(join_opp, by = c("opponent" = "nation", "season" = "season"))
  } else {
    join_team <- join_team |>
      rename_with(~ paste0(.x, "_team"), all_of(numeric_features))
    join_opp <- join_opp |>
      rename_with(~ paste0(.x, "_opp"), all_of(numeric_features))

    out <- match_df |>
      left_join(join_team, by = c("team" = "nation")) |>
      left_join(join_opp, by = c("opponent" = "nation"))
  }

  for (nm in numeric_features) {
    team_col <- paste0(nm, "_team")
    opp_col <- paste0(nm, "_opp")
    if (team_col %in% names(out) && opp_col %in% names(out)) {
      out[[paste0(nm, "_diff")]] <- out[[team_col]] - out[[opp_col]]
      out[[paste0(nm, "_sum")]] <- out[[team_col]] + out[[opp_col]]
      out[[paste0(nm, "_absdiff")]] <- abs(out[[paste0(nm, "_diff")]])
    }
  }

  out <- out |>
    mutate(
      team_goal_diff = if ("team_goals" %in% names(out) && "opponent_goals" %in% names(out)) {
        team_goals - opponent_goals
      } else {
        NA_real_
      }
    )

  if (!include_raw_metadata) {
    out <- out |>
      select(
        team, opponent, season, season_num, neutral, team_win,
        everything()
      )
  }

  out
}

# ------------------------------------------------------------------------------
# Model training / comparison
# ------------------------------------------------------------------------------

choose_train_test_split <- function(df, split_mode = c("auto", "latest_season", "time_split", "random_split"), split_prop = 0.8) {
  split_mode <- match.arg(split_mode)

  if (split_mode == "latest_season") {
    if ("season_num" %in% names(df)) {
      season_vals <- sort(unique(na.omit(df$season_num)))
      if (length(season_vals) >= 2) {
        latest <- max(season_vals)
        train <- df |> filter(season_num < latest)
        test <- df |> filter(season_num == latest)
        if (nrow(train) >= 30 && nrow(test) >= 5) {
          return(list(train = train, test = test, split_type = "latest_season"))
        }
      }
    }
    stop("latest_season split requested but season_num is unavailable or too small.", call. = FALSE)
  }

  if (split_mode == "time_split") {
    if (!("match_date" %in% names(df)) || !any(!is.na(df$match_date))) {
      stop("time_split requested but match_date is unavailable.", call. = FALSE)
    }
    df <- df |> arrange(match_date)
    split <- initial_time_split(df, prop = split_prop)
    return(list(train = training(split), test = testing(split), split_type = "time_split"))
  }

  if (split_mode == "random_split") {
    split <- initial_split(df, prop = split_prop, strata = team_win)
    return(list(train = training(split), test = testing(split), split_type = "random_split"))
  }

  if ("season_num" %in% names(df)) {
    season_vals <- sort(unique(na.omit(df$season_num)))
    if (length(season_vals) >= 2) {
      latest <- max(season_vals)
      train <- df |> filter(season_num < latest)
      test <- df |> filter(season_num == latest)
      if (nrow(train) >= 30 && nrow(test) >= 5) {
        return(list(train = train, test = test, split_type = "latest_season"))
      }
    }
  }

  if ("match_date" %in% names(df) && any(!is.na(df$match_date))) {
    df <- df |> arrange(match_date)
    if (nrow(df) >= 20) {
      split <- initial_time_split(df, prop = split_prop)
      return(list(train = training(split), test = testing(split), split_type = "time_split"))
    }
  }

  split <- initial_split(df, prop = split_prop, strata = team_win)
  list(train = training(split), test = testing(split), split_type = "random_split")
}

build_recipe <- function(train_df) {
  recipe(team_win ~ ., data = train_df) |>
    step_rm(
      any_of(c(
        "team", "opponent", "winner", "match_date", "season",
        "team_goals", "opponent_goals", "team_goal_diff", "season_num",
        "competition_id", "round", "source",
        "game_id", "home_club_id", "away_club_id",
        "home_club_position", "away_club_position",
        "attendance"
      ))
    ) |>
    step_rm(matches("^competition_id")) |>
    step_rm(matches("^round")) |>
    step_rm(matches("^source")) |>
    step_rm(matches("^season_num")) |>
    step_rm(matches("^feature_source")) |>
    step_rm(matches("^team_name")) |>
    step_rm(matches("^competition_type")) |>
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
    step_rm(matches("^feature_source\\.x")) |>
    step_rm(matches("^feature_source\\.y")) |>
    step_rm(matches("^feature_source$")) |>
    step_rm(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_impute_median(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors())
}

build_model_specs <- function(train_df) {
  predictor_cols <- names(train_df)[vapply(train_df, is.numeric, logical(1))]
  predictor_cols <- setdiff(predictor_cols, c("team_goals", "opponent_goals", "season_num", "team_goal_diff"))
  p <- max(2, length(predictor_cols))

  specs <- list()

  if (is_optional_engine_available("ranger")) {
    specs$rand_forest <- rand_forest(
      mode = "classification",
      trees = 1000,
      mtry = min(p, max(2, floor(sqrt(p)))),
      min_n = 5
    ) |>
      set_engine("ranger", probability = TRUE, importance = "impurity")
  }

  if (is_optional_engine_available("xgboost")) {
    specs$xgboost <- boost_tree(
      mode = "classification",
      trees = 1000,
      tree_depth = 4,
      learn_rate = 0.05,
      mtry = min(p, max(2, floor(p * 0.7))),
      min_n = 5,
      loss_reduction = 0
    ) |>
      set_engine("xgboost")
  }

  if (length(specs) == 0) {
    stop("No supported model engines are installed. Install ranger and/or xgboost.", call. = FALSE)
  }

  specs
}

fit_and_score_model <- function(model_name, model_spec, train_df, test_df) {
  rec <- build_recipe(train_df)
  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model_spec)

  fit_obj <- fit(wf, data = train_df)

  prob_pred <- predict(fit_obj, new_data = test_df, type = "prob")
  class_pred <- predict(fit_obj, new_data = test_df, type = "class")

  pred_df <- bind_cols(
    test_df |> select(team, opponent, season, match_date, team_win, team_goals, opponent_goals, team_goal_diff),
    class_pred,
    prob_pred
  )

  metrics_df <- tibble(
    model = model_name,
    accuracy = accuracy(pred_df, truth = team_win, estimate = .pred_class) |> pull(.estimate),
    roc_auc = roc_auc(pred_df, truth = team_win, .pred_win, event_level = "second") |> pull(.estimate),
    mn_log_loss = mn_log_loss(pred_df, truth = team_win, .pred_win) |> pull(.estimate)
  )

  list(
    model_name = model_name,
    fit = fit_obj,
    predictions = pred_df,
    metrics = metrics_df
  )
}

train_head_to_head_models <- function(
  match_results_path = MATCH_RESULTS_CSV,
  team_features = NULL,
  output_dir = MODEL_OUT_DIR,
  split_mode = c("auto", "latest_season", "time_split", "random_split"),
  split_prop = 0.8
) {
  assert_feature_packages()
  assert_training_packages()
  split_mode <- match.arg(split_mode)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(team_features)) {
    if (file.exists(TEAM_CURRENT_FEATURES_CSV)) {
      team_features <- read_csv(TEAM_CURRENT_FEATURES_CSV, show_col_types = FALSE)
    } else if (file.exists(TEAM_SEASON_FEATURES_CSV)) {
      team_features <- read_csv(TEAM_SEASON_FEATURES_CSV, show_col_types = FALSE)
    } else {
      stop(
        "No team feature CSV found. Run write_feature_tables() first.",
        call. = FALSE
      )
    }
    if (!"nation" %in% names(team_features)) {
      stop("team_features must contain a nation column.", call. = FALSE)
    }
  }

  if (!file.exists(match_results_path)) {
    stop("Historical match CSV not found: ", match_results_path, call. = FALSE)
  }

  match_raw <- read_csv(match_results_path, show_col_types = FALSE)
  match_std <- standardize_match_results(match_raw)
  match_feat <- make_match_features(match_std, team_features, include_raw_metadata = TRUE) |>
    drop_na(team_win)

  split <- choose_train_test_split(match_feat, split_mode = split_mode, split_prop = split_prop)
  train_df <- split$train
  test_df <- split$test

  if (nrow(train_df) < 20 || nrow(test_df) < 5) {
    stop(
      "Not enough rows after splitting to train and test models. ",
      "Provide more historical matches.",
      call. = FALSE
    )
  }

  model_specs <- build_model_specs(train_df)
  results <- map2(names(model_specs), model_specs, ~ fit_and_score_model(.x, .y, train_df, test_df))

  metrics_tbl <- bind_rows(map(results, "metrics"))
  preds_tbl <- bind_rows(map(results, "predictions"), .id = "model_index")

  write_csv(metrics_tbl, file.path(output_dir, "model_metrics.csv"))
  write_csv(preds_tbl, file.path(output_dir, "model_predictions.csv"))
  saveRDS(results, file.path(output_dir, "trained_model_suite.rds"))

  list(
    split_type = split$split_type,
    train = train_df,
    test = test_df,
    models = results,
    metrics = metrics_tbl,
    predictions = preds_tbl
  )
}

# ------------------------------------------------------------------------------
# Prediction and bracket simulation helpers
# ------------------------------------------------------------------------------

predict_fixture_win_prob <- function(model_fit, team_a, team_b, team_features, season = NULL, neutral = 1) {
  fixture <- tibble(
    team = team_a,
    opponent = team_b,
    season = if (!is.null(season)) as.character(season) else if ("season" %in% names(team_features)) as.character(team_features$season[1]) else NA_character_,
    neutral = neutral,
    competition_id = "projection",
    round = "Knockout",
    source = "projection"
  )

  feat <- make_match_features(fixture, team_features, include_raw_metadata = TRUE)
  prob <- predict(model_fit, new_data = feat, type = "prob")

  if (!".pred_win" %in% names(prob)) {
    stop("Model did not return a .pred_win probability column.", call. = FALSE)
  }

  tibble(
    team = team_a,
    opponent = team_b,
    win_prob = prob$.pred_win[[1]],
    loss_prob = prob$.pred_loss[[1]]
  )
}

simulate_knockout_game <- function(model_fit, team_a, team_b, team_features, season = NULL, neutral = 1) {
  probs <- predict_fixture_win_prob(
    model_fit = model_fit,
    team_a = team_a,
    team_b = team_b,
    team_features = team_features,
    season = season,
    neutral = neutral
  )

  winner <- sample(
    c(team_a, team_b),
    size = 1,
    prob = c(probs$win_prob[[1]], probs$loss_prob[[1]])
  )

  probs |> mutate(winner = winner)
}

simulate_bracket_round <- function(fixtures, model_fit, team_features, default_season = NULL, default_neutral = 1) {
  fixtures <- as_tibble(fixtures)
  required <- c("team", "opponent")
  missing <- setdiff(required, names(fixtures))
  if (length(missing) > 0) {
    stop("Fixtures must contain: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  pmap_dfr(
    fixtures,
    function(team, opponent, season = NULL, neutral = NULL, ...) {
      simulate_knockout_game(
        model_fit = model_fit,
        team_a = team,
        team_b = opponent,
        team_features = team_features,
        season = if (!is.null(season)) season else default_season,
        neutral = if (!is.null(neutral)) neutral else default_neutral
      )
    }
  )
}

# ------------------------------------------------------------------------------
# Main entry point
# ------------------------------------------------------------------------------

main <- function() {
  assert_feature_packages()
  message("Loading and feature-engineering player data...")
  player_df <- load_player_data()
  feature_tables <- write_feature_tables(player_df = player_df)

  message("Saved team-season features to: ", TEAM_SEASON_FEATURES_CSV)
  message("Saved current-form team features to: ", TEAM_CURRENT_FEATURES_CSV)

  if (file.exists(MATCH_RESULTS_CSV)) {
    message("Historical match data found. Training model suite...")
    model_suite <- train_head_to_head_models(match_results_path = MATCH_RESULTS_CSV)

    print(model_suite$metrics)
    invisible(model_suite)
  } else {
    message("No historical match CSV found at: ", MATCH_RESULTS_CSV)
    message("Add one with team/opponent/winner or team_goals/opponent_goals to train models.")
    invisible(feature_tables)
  }
}

if (identical(Sys.getenv("RUN_WC2026_MODEL_DEMO", unset = "0"), "1")) {
  main()
}
