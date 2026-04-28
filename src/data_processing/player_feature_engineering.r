# Project - Player feature engineering
library(tidyverse)
library(caret)

# Response variable
Match_Data <- Match_Result_Final %>%
  mutate(
    Outcome = case_when(
      HomeGoals > AwayGoals ~ "Home_Win",
      HomeGoals == AwayGoals ~ "Draw",
      HomeGoals < AwayGoals ~ "Home_Lose"
    ),
    Outcome = factor(Outcome, levels = c("Home_Lose", "Draw", "Home_Win"))
  )


# Extracts the top 11 team members by play time and calculates their stats
aggregate_top_11 <- function(player_df) {

  expected_cols <- tibble(
    total_goals = NA_real_, total_xg = NA_real_, total_xa = NA_real_,
    total_shots = NA_real_, total_key_passes = NA_real_, total_xgchain = NA_real_,
    avg_xg_per90 = NA_real_, avg_xa_per90 = NA_real_, avg_xgbuildup_per90 = NA_real_,
    total_yellows = NA_real_, total_reds = NA_real_,
    avg_height = NA_real_, total_market_value = NA_real_, total_intl_caps = NA_real_
  )
  
  if (is.null(player_df) || nrow(player_df) == 0) {
    return(expected_cols)
  }
  
  top_players <- player_df %>%
    arrange(desc(minutes)) %>%
    slice_head(n = 11)
  
  n_players <- nrow(top_players)
  
  agg <- top_players %>%
    summarise(
      total_goals = sum(goals, na.rm = TRUE),
      total_xg = sum(xg, na.rm = TRUE),
      total_xa = sum(xa, na.rm = TRUE),
      total_shots = sum(shots, na.rm = TRUE),
      total_key_passes = sum(key_passes, na.rm = TRUE),
      total_xgchain = sum(xgchain, na.rm = TRUE),
      total_yellows = sum(yellow_cards, na.rm = TRUE),
      total_reds = sum(red_cards, na.rm = TRUE),
      total_market_value = sum(tm_market_value_in_eur, na.rm = TRUE),
      total_intl_caps = sum(tm_international_caps, na.rm = TRUE),
      
      avg_xg_per90 = mean(xg_per90, na.rm = TRUE),
      avg_xa_per90 = mean(xa_per90, na.rm = TRUE),
      avg_xgbuildup_per90 = mean(xgbuildup_per90, na.rm = TRUE),
      avg_height = mean(tm_height_in_cm, na.rm = TRUE)
    )
  
  # Scaling for teams of less than 11 people
  if (n_players > 0 && n_players < 11) {
    scale_factor <- 11 / n_players
    
    agg <- agg %>%
      mutate(across(c(total_goals, total_xg, total_xa, total_shots, 
                      total_key_passes, total_xgchain, total_yellows, 
                      total_reds, total_market_value, total_intl_caps), 
                    ~ .x * scale_factor))
  }
  
  return(agg)
}

Match_Data_Engineered <- Match_Data %>%
  mutate(
    Home_Team_Stats = map(Home_Players, aggregate_top_11),
    Away_Team_Stats = map(Away_Players, aggregate_top_11)
  ) %>%
  unnest_wider(Home_Team_Stats, names_sep = "_") %>%
  unnest_wider(Away_Team_Stats, names_sep = "_") %>%
  select(-Home_Players, -Away_Players)


Match_Data_Base <- Match_Data_Engineered %>%
  mutate(
    diff_total_goals = Home_Team_Stats_total_goals - Away_Team_Stats_total_goals,
    diff_total_xg = Home_Team_Stats_total_xg - Away_Team_Stats_total_xg,
    diff_total_xa = Home_Team_Stats_total_xa - Away_Team_Stats_total_xa,
    diff_total_shots = Home_Team_Stats_total_shots - Away_Team_Stats_total_shots,
    diff_total_key_passes = Home_Team_Stats_total_key_passes - Away_Team_Stats_total_key_passes,
    diff_total_xgchain = Home_Team_Stats_total_xgchain - Away_Team_Stats_total_xgchain,

    diff_avg_xg_per90 = Home_Team_Stats_avg_xg_per90 - Away_Team_Stats_avg_xg_per90,
    diff_avg_xa_per90 = Home_Team_Stats_avg_xa_per90 - Away_Team_Stats_avg_xa_per90,
    diff_avg_xgbuildup_per90 = Home_Team_Stats_avg_xgbuildup_per90 - Away_Team_Stats_avg_xgbuildup_per90,
    
    diff_total_yellows = Home_Team_Stats_total_yellows - Away_Team_Stats_total_yellows,
    diff_total_reds = Home_Team_Stats_total_reds - Away_Team_Stats_total_reds,
    
    diff_avg_height = Home_Team_Stats_avg_height - Away_Team_Stats_avg_height,
    diff_total_market_value = Home_Team_Stats_total_market_value - Away_Team_Stats_total_market_value,
    diff_total_intl_caps = Home_Team_Stats_total_intl_caps - Away_Team_Stats_total_intl_caps,
    
    diff_elo = Home_Elo - Away_Elo
  ) %>%
  select(Outcome, Date, starts_with("diff_")) %>%
  na.omit()

Match_Data_Base <- Match_Data_Base %>% arrange(Date)
write.csv(Match_Data_Base, "~/Downloads/MatchData.csv")

# 3. Time-Based Split (Oldest 80% Train, Newest 20% Test)
total_rows <- nrow(Match_Data_Base)
train_size <- floor(0.8 * total_rows)

train_data <- Match_Data_Base[1:train_size, ]
test_data <- Match_Data_Base[(train_size + 1):total_rows, ]

scaling_params <- preProcess(train_data %>% select(starts_with("diff_")), 
                             method = c("center", "scale"))

train_data_scaled <- predict(scaling_params, train_data)
test_data_scaled <- predict(scaling_params, test_data)

summary(train_data_scaled %>% select(starts_with("diff_")))
summary(test_data_scaled %>% select(starts_with("diff_")))

