# WC 2026 bracket simulation helpers
#
# This script takes group-stage standings, resolves them into the finalized FIFA
# knockout tree from the schedule PDF, and runs the model through each match.
# It is designed to work with the xG-based feature tables and the head-to-head
# model helpers in `wc2026_head_to_head_models.R`.

source("wc2026_schedule_map.R")
source("wc2026_head_to_head_models.R")

OUT_DIR <- file.path(getwd(), "wc2026_outputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

SCORE_LAMBDA_CACHE <- new.env(parent = emptyenv())

fwc26_group_slot_lookup <- function() {
  fwc26_seed_map() |>
    filter(slot_kind == "fixed_group") |>
    mutate(
      place = as.integer(substr(slot, 1, 1)),
      group = substr(slot, 2, 2)
    ) |>
    select(group, place, slot, first_match)
}

fwc26_third_place_slot_labels <- function() {
  fwc26_seed_map() |>
    filter(slot_kind == "third_place_pool") |>
    pull(slot)
}

pretty_fwc26_stage <- function(stage) {
  case_when(
    stage == "round_of_32" ~ "Round of 32",
    stage == "round_of_16" ~ "Round of 16",
    stage == "quarter_final" ~ "Quarter-final",
    stage == "semi_final" ~ "Semi-final",
    stage == "bronze_final" ~ "Bronze final",
    stage == "final" ~ "Final",
    TRUE ~ stage
  )
}

standardize_fwc26_group_results <- function(group_results) {
  assert_feature_packages()

  group_results <- as_tibble(group_results)
  group_results <- rename_if_present(group_results, "nation", "team_name")
  group_results <- rename_if_present(group_results, "team", "team_name")
  group_results <- rename_if_present(group_results, "group_id", "group")
  group_results <- rename_if_present(group_results, "group_letter", "group")
  group_results <- rename_if_present(group_results, "rank", "place")
  group_results <- rename_if_present(group_results, "position", "place")

  if (!"group" %in% names(group_results)) {
    stop("group_results must contain a group column.", call. = FALSE)
  }
  if (!"place" %in% names(group_results)) {
    stop("group_results must contain a place/rank/position column.", call. = FALSE)
  }

  if (!"team_name" %in% names(group_results)) {
    group_results$team_name <- NA_character_
  }
  if (!"team_code" %in% names(group_results)) {
    group_results$team_code <- NA_character_
  }
  if (all(is.na(group_results$team_name)) && all(is.na(group_results$team_code))) {
    stop(
      "group_results must contain team_name/team or team_code values for at least one row.",
      call. = FALSE
    )
  }

  assignments <- fwc26_group_assignments() |>
    mutate(team_name_norm = normalize_fwc26_team_name(team_name))

  out <- group_results |>
    mutate(
      group = str_squish(as.character(group)),
      place = safe_num(place),
      team_name = str_squish(as.character(team_name)),
      team_code = str_squish(as.character(team_code)),
      team_name_norm = normalize_fwc26_team_name(team_name)
    ) |>
    left_join(
      assignments |>
        select(
          group,
          team_code,
          team_name_from_code = team_name,
          team_name_norm_from_code = team_name_norm
        ),
      by = c("group", "team_code")
    ) |>
    mutate(
      team_name = coalesce(team_name, team_name_from_code),
      team_name_norm = coalesce(team_name_norm, team_name_norm_from_code)
    ) |>
    select(-team_name_from_code, -team_name_norm_from_code) |>
    left_join(
      assignments |>
        select(group, team_name_norm, team_code_from_name = team_code),
      by = c("group", "team_name_norm")
    ) |>
    mutate(
      team_code = coalesce(team_code, team_code_from_name)
    ) |>
    select(-team_code_from_name) |>
    arrange(group, place)

  out
}

build_fwc26_initial_slot_assignments <- function(group_results, third_place_assignments = NULL) {
  assert_feature_packages()

  group_results <- standardize_fwc26_group_results(group_results)
  fixed_slots <- fwc26_group_slot_lookup()

  top_two <- group_results |>
    filter(place %in% c(1, 2)) |>
    left_join(fixed_slots, by = c("group", "place")) |>
    mutate(
      slot_kind = "fixed_group",
      team_model = normalize_fwc26_team_name(team_name)
    )

  missing_fixed <- top_two |>
    filter(is.na(slot))
  if (nrow(missing_fixed) > 0) {
    stop(
      "Could not resolve fixed group slots for: ",
      paste(unique(missing_fixed$group), collapse = ", "),
      call. = FALSE
    )
  }

  third_place <- group_results |>
    filter(place == 3)

  if (nrow(third_place) == 0) {
    return(top_two |>
      select(slot, team_name, team_model, team_code, group, place, slot_kind, first_match))
  }

  if (is.null(third_place_assignments)) {
    warning(
      "Third-place teams are present but no third_place_assignments table was supplied. ",
      "Returning them with NA slots. Full bracket simulation will stop until the slots are filled.",
      call. = FALSE
    )
    third_out <- third_place |>
      mutate(
        slot = NA_character_,
        slot_kind = "third_place_unassigned",
        team_model = normalize_fwc26_team_name(team_name),
        first_match = NA_integer_
      )
  } else {
    third_place_assignments <- as_tibble(third_place_assignments)
    third_place_assignments <- rename_if_present(third_place_assignments, "team", "team_name")
    third_place_assignments <- rename_if_present(third_place_assignments, "nation", "team_name")
    third_place_assignments <- rename_if_present(third_place_assignments, "slot_label", "slot")
    third_place_assignments <- rename_if_present(third_place_assignments, "pool_slot", "slot")

    if (!"slot" %in% names(third_place_assignments)) {
      stop(
        "third_place_assignments must contain a slot column with values like 3ABCDF.",
        call. = FALSE
      )
    }
    if (!"group" %in% names(third_place_assignments) &&
        !"team_name" %in% names(third_place_assignments) &&
        !"team_code" %in% names(third_place_assignments)) {
      stop(
        "third_place_assignments must contain group, team_name, or team_code.",
        call. = FALSE
      )
    }

    third_place_assignments <- third_place_assignments |>
      mutate(
        slot = str_squish(as.character(slot)),
        group = if ("group" %in% names(third_place_assignments)) str_squish(as.character(group)) else NA_character_,
        team_name = if ("team_name" %in% names(third_place_assignments)) str_squish(as.character(team_name)) else NA_character_,
        team_code = if ("team_code" %in% names(third_place_assignments)) str_squish(as.character(team_code)) else NA_character_,
        team_name_norm = normalize_fwc26_team_name(team_name)
      )

    valid_third_slots <- fwc26_third_place_slot_labels()
    invalid_slots <- setdiff(unique(third_place_assignments$slot), valid_third_slots)
    if (length(invalid_slots) > 0) {
      stop(
        "Invalid third-place slots found: ",
        paste(invalid_slots, collapse = ", "),
        call. = FALSE
      )
    }

    if (all(c("group", "slot") %in% names(third_place_assignments)) &&
        any(!is.na(third_place_assignments$group))) {
      third_out <- third_place |>
        left_join(
          third_place_assignments |>
            select(group, slot),
          by = "group"
        ) |>
        mutate(
          slot_kind = "third_place_pool",
          team_model = normalize_fwc26_team_name(team_name),
          first_match = match(slot, fwc26_seed_map()$slot)
        ) |>
        select(slot, team_name, team_model, team_code, group, place, slot_kind, first_match)
    } else {
      third_out <- third_place |>
        left_join(
          third_place_assignments |>
            select(team_name_norm, slot),
          by = "team_name_norm"
        ) |>
        left_join(
          third_place_assignments |>
            select(team_code, slot_from_code = slot),
          by = "team_code"
        ) |>
        mutate(
          slot = coalesce(slot, slot_from_code),
          slot_kind = "third_place_pool",
          team_model = normalize_fwc26_team_name(team_name),
          first_match = match(slot, fwc26_seed_map()$slot)
        ) |>
        select(slot, team_name, team_model, team_code, group, place, slot_kind, first_match)
    }

    missing_third <- third_out |>
      filter(is.na(slot))
    if (nrow(missing_third) > 0) {
      warning(
        "Some third-place teams still do not have assigned slots: ",
        paste(unique(missing_third$team_name), collapse = ", "),
        call. = FALSE
      )
    }
  }

  bind_rows(
    top_two |>
      select(slot, team_name, team_model, team_code, group, place, slot_kind, first_match),
    third_out
  ) |>
    arrange(is.na(slot), slot, group, place)
}

complete_fwc26_team_features <- function(team_features, required_team_names) {
  assert_feature_packages()

  team_features <- as_tibble(team_features)
  if (!"nation" %in% names(team_features)) {
    stop("team_features must contain a nation column.", call. = FALSE)
  }

  existing <- team_features |>
    mutate(
      nation = normalize_fwc26_team_name(nation),
      feature_source = if ("feature_source" %in% names(team_features)) {
        as.character(feature_source)
      } else {
        "observed"
      }
    )

  required_team_names <- unique(normalize_fwc26_team_name(required_team_names))
  missing <- setdiff(required_team_names, unique(existing$nation))

  if (length(missing) == 0) {
    return(existing)
  }

  warning(
    "Imputing median feature rows for missing teams: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )

  numeric_cols <- names(existing)[vapply(existing, is.numeric, logical(1))]
  template <- existing |>
    summarise(across(all_of(numeric_cols), ~ median(.x, na.rm = TRUE)))

  imputed <- map_dfr(missing, function(team) {
    row <- template
    row$nation <- team
    row$feature_source <- "imputed"
    row
  })

  bind_rows(existing, imputed)
}

resolve_fwc26_slot <- function(slot_name, slot_state, match_number = NULL) {
  if (is.null(slot_name) || is.na(slot_name) || !nzchar(slot_name)) {
    return(NA_character_)
  }
  if (!slot_name %in% names(slot_state)) {
    prefix <- if (!is.null(match_number)) paste0("match ", match_number, ": ") else ""
    stop(
      prefix,
      "No team has been assigned to slot `",
      slot_name,
      "` yet. Check the group-stage inputs and third-place routing.",
      call. = FALSE
    )
  }
  slot_state[[slot_name]]
}

play_fwc26_fixture <- function(
  model_fit,
  team_a,
  team_b,
  team_features,
  season = NULL,
  neutral = 1,
  decision_rule = c("most_probable", "sample")
) {
  assert_feature_packages()

  decision_rule <- match.arg(decision_rule)
  probs <- predict_fixture_win_prob(
    model_fit = model_fit,
    team_a = team_a,
    team_b = team_b,
    team_features = team_features,
    season = season,
    neutral = neutral
  )

  if (decision_rule == "sample") {
    winner <- sample(
      c(team_a, team_b),
      size = 1,
      prob = c(probs$win_prob[[1]], probs$loss_prob[[1]])
    )
  } else {
    winner <- if (probs$win_prob[[1]] >= probs$loss_prob[[1]]) team_a else team_b
  }
  loser <- if (winner == team_a) team_b else team_a

  probs |>
    mutate(
      team_a = team_a,
      team_b = team_b,
      winner = winner,
      loser = loser,
      decision_rule = decision_rule
    )
}

resolve_score_model_components <- function(score_model, fallback_lambda = 1.25) {
  if (inherits(score_model, "workflow")) {
    return(list(
      fit = score_model,
      fallback_lambda = fallback_lambda,
      model_name = "score_model"
    ))
  }

  if (is.list(score_model) && !is.null(score_model$fit)) {
    model_name <- "score_model"
    if (!is.null(score_model$metrics) &&
        "model" %in% names(score_model$metrics) &&
        length(score_model$metrics$model) > 0 &&
        !is.null(score_model$metrics$model[[1]])) {
      model_name <- as.character(score_model$metrics$model[[1]])
    } else if (!is.null(score_model$model_name)) {
      model_name <- as.character(score_model$model_name)
    }

    if (is.null(score_model$fallback_lambda) || !is.finite(score_model$fallback_lambda)) {
      score_model$fallback_lambda <- fallback_lambda
    }

    return(list(
      fit = score_model$fit,
      fallback_lambda = score_model$fallback_lambda,
      model_name = model_name
    ))
  }

  stop(
    "score_model must be a workflow or a list containing a fit object.",
    call. = FALSE
  )
}

predict_score_fixture_lambda <- function(
  score_fit,
  team_a,
  team_b,
  team_features,
  season = NULL,
  neutral = 1,
  fallback_lambda = 1.25,
  round_label = "Knockout"
) {
  fixture <- tibble(
    team = team_a,
    opponent = team_b,
    season = if (!is.null(season)) {
      as.character(season)
    } else if ("latest_season" %in% names(team_features)) {
      as.character(team_features$latest_season[[1]])
    } else {
      NA_character_
    },
    neutral = neutral,
    competition_id = "world_cup_knockout",
    round = round_label,
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

predict_score_fixture_lambdas <- function(
  score_fit,
  team_a,
  team_b,
  team_features,
  season = NULL,
  neutral = 1,
  fallback_lambda = 1.25,
  round_label = "Knockout"
) {
  tibble(
    lambda_a = predict_score_fixture_lambda(
      score_fit = score_fit,
      team_a = team_a,
      team_b = team_b,
      team_features = team_features,
      season = season,
      neutral = neutral,
      fallback_lambda = fallback_lambda,
      round_label = round_label
    ),
    lambda_b = predict_score_fixture_lambda(
      score_fit = score_fit,
      team_a = team_b,
      team_b = team_a,
      team_features = team_features,
      season = season,
      neutral = neutral,
      fallback_lambda = fallback_lambda,
      round_label = round_label
    )
  )
}

adjust_score_lambdas_by_win_prob <- function(
  lambda_a,
  lambda_b,
  win_prob_a,
  adjust_scale = 0.75,
  clamp = c(0.75, 1.33)
) {
  win_prob_a <- safe_num(win_prob_a)
  if (is.na(win_prob_a) || !is.finite(win_prob_a)) {
    return(tibble(lambda_a = lambda_a, lambda_b = lambda_b))
  }

  strength <- exp(adjust_scale * (win_prob_a - 0.5))
  strength <- min(max(strength, clamp[[1]]), clamp[[2]])

  tibble(
    lambda_a = lambda_a * strength,
    lambda_b = lambda_b / strength
  )
}

simulate_penalty_shootout <- function(
  lambda_a,
  lambda_b,
  base_prob = 0.75,
  skill_scale = 0.03,
  max_rounds = 15
) {
  skill_delta <- tanh((lambda_a - lambda_b) / 1.5)
  penalty_prob_a <- pmin(pmax(base_prob + skill_scale * skill_delta, 0.65), 0.85)
  penalty_prob_b <- pmin(pmax(base_prob - skill_scale * skill_delta, 0.65), 0.85)

  goals_a <- rbinom(1, 5, penalty_prob_a)
  goals_b <- rbinom(1, 5, penalty_prob_b)
  rounds <- 5L

  while (goals_a == goals_b && rounds < max_rounds) {
    goals_a <- goals_a + rbinom(1, 1, penalty_prob_a)
    goals_b <- goals_b + rbinom(1, 1, penalty_prob_b)
    rounds <- rounds + 1L
  }

  if (goals_a == goals_b) {
    winner_side <- if (runif(1) < 0.5) "A" else "B"
  } else {
    winner_side <- if (goals_a > goals_b) "A" else "B"
  }

  tibble(
    penalty_prob_a = penalty_prob_a,
    penalty_prob_b = penalty_prob_b,
    penalty_goals_a = goals_a,
    penalty_goals_b = goals_b,
    penalty_rounds = rounds,
    penalty_winner_side = winner_side
  )
}

play_fwc26_score_fixture <- function(
  score_fit,
  team_a,
  team_b,
  team_features,
  season = NULL,
  neutral = 1,
  decision_rule = c("sample", "most_probable"),
  fallback_lambda = 1.25,
  et_factor = 1 / 3,
  penalty_base_prob = 0.75,
  penalty_skill_scale = 0.03,
  lambda_a = NULL,
  lambda_b = NULL
) {
  assert_feature_packages()

  decision_rule <- match.arg(decision_rule)
  if (is.null(lambda_a) || is.null(lambda_b)) {
    lambdas <- predict_score_fixture_lambdas(
      score_fit = score_fit,
      team_a = team_a,
      team_b = team_b,
      team_features = team_features,
      season = season,
      neutral = neutral,
      fallback_lambda = fallback_lambda
    )
    if (is.null(lambda_a)) lambda_a <- lambdas$lambda_a[[1]]
    if (is.null(lambda_b)) lambda_b <- lambdas$lambda_b[[1]]
  }

  lambda_a <- safe_num(lambda_a)
  lambda_b <- safe_num(lambda_b)

  if (decision_rule == "sample") {
    reg_goals_a <- rpois(1, lambda_a)
    reg_goals_b <- rpois(1, lambda_b)

    if (reg_goals_a != reg_goals_b) {
      resolution <- "regular"
      et_goals_a <- 0L
      et_goals_b <- 0L
      penalty <- tibble(
        penalty_prob_a = NA_real_,
        penalty_prob_b = NA_real_,
        penalty_goals_a = 0L,
        penalty_goals_b = 0L,
        penalty_rounds = 0L,
        penalty_winner_side = NA_character_
      )
      winner <- if (reg_goals_a > reg_goals_b) team_a else team_b
    } else {
      et_goals_a <- rpois(1, pmax(lambda_a * et_factor, 0.05))
      et_goals_b <- rpois(1, pmax(lambda_b * et_factor, 0.05))

      if ((reg_goals_a + et_goals_a) != (reg_goals_b + et_goals_b)) {
        resolution <- "extra_time"
        penalty <- tibble(
          penalty_prob_a = NA_real_,
          penalty_prob_b = NA_real_,
          penalty_goals_a = 0L,
          penalty_goals_b = 0L,
          penalty_rounds = 0L,
          penalty_winner_side = NA_character_
        )
        winner <- if ((reg_goals_a + et_goals_a) > (reg_goals_b + et_goals_b)) team_a else team_b
      } else {
        resolution <- "penalties"
        penalty <- simulate_penalty_shootout(
          lambda_a = lambda_a,
          lambda_b = lambda_b,
          base_prob = penalty_base_prob,
          skill_scale = penalty_skill_scale
        )
        winner <- if (penalty$penalty_winner_side[[1]] == "A") team_a else team_b
      }
    }
  } else {
    reg_goals_a <- round(lambda_a)
    reg_goals_b <- round(lambda_b)

    if (reg_goals_a != reg_goals_b) {
      resolution <- "regular"
      et_goals_a <- 0L
      et_goals_b <- 0L
      penalty <- tibble(
        penalty_prob_a = NA_real_,
        penalty_prob_b = NA_real_,
        penalty_goals_a = 0L,
        penalty_goals_b = 0L,
        penalty_rounds = 0L,
        penalty_winner_side = NA_character_
      )
      winner <- if (reg_goals_a > reg_goals_b) team_a else team_b
    } else {
      et_goals_a <- round(lambda_a * et_factor)
      et_goals_b <- round(lambda_b * et_factor)

      if ((reg_goals_a + et_goals_a) != (reg_goals_b + et_goals_b)) {
        resolution <- "extra_time"
        penalty <- tibble(
          penalty_prob_a = NA_real_,
          penalty_prob_b = NA_real_,
          penalty_goals_a = 0L,
          penalty_goals_b = 0L,
          penalty_rounds = 0L,
          penalty_winner_side = NA_character_
        )
        winner <- if ((reg_goals_a + et_goals_a) > (reg_goals_b + et_goals_b)) team_a else team_b
      } else {
        resolution <- "penalties"
        penalty <- simulate_penalty_shootout(
          lambda_a = lambda_a,
          lambda_b = lambda_b,
          base_prob = penalty_base_prob,
          skill_scale = penalty_skill_scale
        )
        winner <- if (penalty$penalty_winner_side[[1]] == "A") team_a else team_b
      }
    }
  }

  loser <- if (winner == team_a) team_b else team_a

  tibble(
    team_a = team_a,
    team_b = team_b,
    lambda_a = lambda_a,
    lambda_b = lambda_b,
    reg_goals_a = reg_goals_a,
    reg_goals_b = reg_goals_b,
    et_goals_a = et_goals_a,
    et_goals_b = et_goals_b,
    penalty_goals_a = penalty$penalty_goals_a[[1]],
    penalty_goals_b = penalty$penalty_goals_b[[1]],
    penalty_prob_a = penalty$penalty_prob_a[[1]],
    penalty_prob_b = penalty$penalty_prob_b[[1]],
    penalty_rounds = penalty$penalty_rounds[[1]],
    resolution = resolution,
    winner = winner,
    loser = loser,
    decision_rule = decision_rule
  )
}

simulate_fwc26_bracket <- function(
  model_fit,
  team_features,
  group_results = NULL,
  third_place_assignments = NULL,
  slot_assignments = NULL,
  season = NULL,
  neutral = 1,
  decision_rule = c("most_probable", "sample"),
  bracket_slots = fwc26_bracket_slots()
) {
  assert_feature_packages()

  decision_rule <- match.arg(decision_rule)

  if (is.null(slot_assignments)) {
    if (is.null(group_results)) {
      stop(
        "Provide either slot_assignments or group_results.",
        call. = FALSE
      )
    }
    slot_assignments <- build_fwc26_initial_slot_assignments(
      group_results = group_results,
      third_place_assignments = third_place_assignments
    )
  } else {
    slot_assignments <- as_tibble(slot_assignments)
    if (!"slot" %in% names(slot_assignments)) {
      stop("slot_assignments must contain a slot column.", call. = FALSE)
    }
    if (!"team_model" %in% names(slot_assignments)) {
      if ("team_name" %in% names(slot_assignments)) {
        slot_assignments <- slot_assignments |>
          mutate(team_model = normalize_fwc26_team_name(team_name))
      } else {
        stop(
          "slot_assignments must contain team_model or team_name.",
          call. = FALSE
        )
      }
    }
  }

  required_team_names <- unique(na.omit(slot_assignments$team_model))
  team_features <- complete_fwc26_team_features(team_features, required_team_names)

  slot_state <- setNames(slot_assignments$team_model, slot_assignments$slot)
  match_rows <- vector("list", nrow(bracket_slots))

  for (i in seq_len(nrow(bracket_slots))) {
    row <- bracket_slots[i, ]
    team_a <- resolve_fwc26_slot(row$team_a_slot[[1]], slot_state, row$match_number[[1]])
    team_b <- resolve_fwc26_slot(row$team_b_slot[[1]], slot_state, row$match_number[[1]])

    result <- play_fwc26_fixture(
      model_fit = model_fit,
      team_a = team_a,
      team_b = team_b,
      team_features = team_features,
      season = season,
      neutral = neutral,
      decision_rule = decision_rule
    ) |>
      mutate(
        match_number = row$match_number[[1]],
        stage = row$stage[[1]],
        team_a_slot = row$team_a_slot[[1]],
        team_b_slot = row$team_b_slot[[1]],
        winner_slot = paste0("W", row$match_number[[1]]),
        loser_slot = paste0("L", row$match_number[[1]])
      )

    match_rows[[i]] <- result
    slot_state[[result$winner_slot[[1]]]] <- result$winner[[1]]
    slot_state[[result$loser_slot[[1]]]] <- result$loser[[1]]
  }

  matches <- bind_rows(match_rows) |>
    select(
      match_number, stage, team_a_slot, team_b_slot,
      team_a, team_b, win_prob, loss_prob,
      winner, loser, winner_slot, loser_slot, decision_rule,
      everything()
    ) |>
    arrange(match_number)

  team_progress <- summarise_fwc26_team_progress(matches)

  list(
    slot_assignments = slot_assignments,
    matches = matches,
    team_progress = team_progress,
    slot_state = slot_state
  )
}

simulate_fwc26_score_bracket <- function(
  score_model,
  team_features,
  group_results = NULL,
  third_place_assignments = NULL,
  slot_assignments = NULL,
  season = NULL,
  neutral = 1,
  decision_rule = c("sample", "most_probable"),
  bracket_slots = fwc26_bracket_slots(),
  fallback_lambda = 1.25,
  et_factor = 1 / 3,
  penalty_base_prob = 0.75,
  penalty_skill_scale = 0.03,
  winner_model_fit = NULL,
  winner_model_name = NULL,
  winner_adjust_scale = 0.75,
  lambda_cache = NULL
) {
  assert_feature_packages()

  decision_rule <- match.arg(decision_rule)
  score_components <- resolve_score_model_components(score_model, fallback_lambda = fallback_lambda)
  score_fit <- score_components$fit
  fallback_lambda <- score_components$fallback_lambda
  if (is.null(lambda_cache)) {
    lambda_cache <- SCORE_LAMBDA_CACHE
  }

  if (is.null(slot_assignments)) {
    if (is.null(group_results)) {
      stop(
        "Provide either slot_assignments or group_results.",
        call. = FALSE
      )
    }
    slot_assignments <- build_fwc26_initial_slot_assignments(
      group_results = group_results,
      third_place_assignments = third_place_assignments
    )
  } else {
    slot_assignments <- as_tibble(slot_assignments)
    if (!"slot" %in% names(slot_assignments)) {
      stop("slot_assignments must contain a slot column.", call. = FALSE)
    }
    if (!"team_model" %in% names(slot_assignments)) {
      if ("team_name" %in% names(slot_assignments)) {
        slot_assignments <- slot_assignments |>
          mutate(team_model = normalize_fwc26_team_name(team_name))
      } else {
        stop(
          "slot_assignments must contain team_model or team_name.",
          call. = FALSE
        )
      }
    }
  }

  required_team_names <- unique(na.omit(slot_assignments$team_model))
  team_features <- complete_fwc26_team_features(team_features, required_team_names)

  if (is.null(winner_model_name)) {
    if (!is.null(winner_model_fit) && !is.null(winner_model_fit$model_name)) {
      winner_model_name <- as.character(winner_model_fit$model_name)
    } else if (!is.null(winner_model_fit) &&
               !is.null(winner_model_fit$metrics) &&
               "model" %in% names(winner_model_fit$metrics) &&
               length(winner_model_fit$metrics$model) > 0) {
      winner_model_name <- as.character(winner_model_fit$metrics$model[[1]])
    } else {
      winner_model_name <- "score_only"
    }
  }

  get_cached_lambdas <- function(team_a, team_b) {
    key <- paste(
      team_a,
      team_b,
      season,
      neutral,
      score_components$model_name,
      winner_model_name,
      winner_adjust_scale,
      sep = "||"
    )
    if (exists(key, envir = lambda_cache, inherits = FALSE)) {
      return(get(key, envir = lambda_cache, inherits = FALSE))
    }

    lambdas <- predict_score_fixture_lambdas(
      score_fit = score_fit,
      team_a = team_a,
      team_b = team_b,
      team_features = team_features,
      season = season,
      neutral = neutral,
      fallback_lambda = fallback_lambda
    )

    if (!is.null(winner_model_fit)) {
      win_prob <- predict_fixture_win_prob(
        model_fit = winner_model_fit,
        team_a = team_a,
        team_b = team_b,
        team_features = team_features,
        season = season,
        neutral = neutral
      )$win_prob[[1]]

      adjusted <- adjust_score_lambdas_by_win_prob(
        lambda_a = lambdas$lambda_a[[1]],
        lambda_b = lambdas$lambda_b[[1]],
        win_prob_a = win_prob,
        adjust_scale = winner_adjust_scale
      )
      lambdas$lambda_a <- adjusted$lambda_a
      lambdas$lambda_b <- adjusted$lambda_b
    }

    assign(key, lambdas, envir = lambda_cache)
    lambdas
  }

  slot_state <- setNames(slot_assignments$team_model, slot_assignments$slot)
  match_rows <- vector("list", nrow(bracket_slots))

  for (i in seq_len(nrow(bracket_slots))) {
    row <- bracket_slots[i, ]
    team_a <- resolve_fwc26_slot(row$team_a_slot[[1]], slot_state, row$match_number[[1]])
    team_b <- resolve_fwc26_slot(row$team_b_slot[[1]], slot_state, row$match_number[[1]])
    lambdas <- get_cached_lambdas(team_a, team_b)

    result <- play_fwc26_score_fixture(
      score_fit = score_fit,
      team_a = team_a,
      team_b = team_b,
      team_features = team_features,
      season = season,
      neutral = neutral,
      decision_rule = decision_rule,
      fallback_lambda = fallback_lambda,
      et_factor = et_factor,
      penalty_base_prob = penalty_base_prob,
      penalty_skill_scale = penalty_skill_scale,
      lambda_a = lambdas$lambda_a[[1]],
      lambda_b = lambdas$lambda_b[[1]]
    ) |>
      mutate(
        match_number = row$match_number[[1]],
        stage = row$stage[[1]],
        team_a_slot = row$team_a_slot[[1]],
        team_b_slot = row$team_b_slot[[1]],
        winner_slot = paste0("W", row$match_number[[1]]),
        loser_slot = paste0("L", row$match_number[[1]])
      )

    match_rows[[i]] <- result
    slot_state[[result$winner_slot[[1]]]] <- result$winner[[1]]
    slot_state[[result$loser_slot[[1]]]] <- result$loser[[1]]
  }

  matches <- bind_rows(match_rows) |>
    select(
      match_number, stage, team_a_slot, team_b_slot,
      team_a, team_b, lambda_a, lambda_b,
      reg_goals_a, reg_goals_b,
      et_goals_a, et_goals_b,
      penalty_goals_a, penalty_goals_b,
      penalty_prob_a, penalty_prob_b, penalty_rounds,
      resolution, winner, loser, winner_slot, loser_slot, decision_rule,
      everything()
    ) |>
    arrange(match_number)

  team_progress <- summarise_fwc26_team_progress(matches)

  list(
    model_name = if (!is.null(winner_model_fit)) {
      paste0(score_components$model_name, "_", winner_model_name)
    } else {
      score_components$model_name
    },
    slot_assignments = slot_assignments,
    matches = matches,
    team_progress = team_progress,
    slot_state = slot_state
  )
}

summarise_fwc26_team_progress <- function(match_results) {
  assert_feature_packages()

  if (!nrow(match_results)) {
    return(tibble())
  }

  long <- bind_rows(
    match_results |>
      transmute(
        match_number,
        stage,
        team = team_a,
        opponent = team_b,
        result = if_else(team_a == winner, "win", "loss")
      ),
    match_results |>
      transmute(
        match_number,
        stage,
        team = team_b,
        opponent = team_a,
        result = if_else(team_b == winner, "win", "loss")
      )
  ) |>
    arrange(team, match_number)

  long |>
    group_by(team) |>
    summarise(
      matches_played = n(),
      wins = sum(result == "win"),
      losses = sum(result == "loss"),
      match_path = paste(match_number, collapse = " -> "),
      last_match_number = last(match_number),
      last_stage = last(stage),
      last_result = last(result),
      furthest_round = pretty_fwc26_stage(last_stage),
      final_position = case_when(
        last_match_number == 104 & last_result == "win" ~ "Champion",
        last_match_number == 104 & last_result == "loss" ~ "Runner-up",
        last_match_number == 103 & last_result == "win" ~ "Third place",
        last_match_number == 103 & last_result == "loss" ~ "Fourth place",
        TRUE ~ pretty_fwc26_stage(last_stage)
      ),
      .groups = "drop"
    ) |>
    arrange(desc(last_match_number), team)
}

save_fwc26_bracket_simulation <- function(simulation, out_dir = OUT_DIR, prefix = "fwc26") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv(simulation$slot_assignments, file.path(out_dir, paste0(prefix, "_slot_assignments.csv")))
  write_csv(simulation$matches, file.path(out_dir, paste0(prefix, "_match_results.csv")))
  write_csv(simulation$team_progress, file.path(out_dir, paste0(prefix, "_team_progress.csv")))

  invisible(simulation)
}
