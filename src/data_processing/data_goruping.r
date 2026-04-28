# Project - Grouping data into a single dataframe
rm(list = ls()) 
setwd('/Users/javier/Library/CloudStorage/OneDrive-Personal/UMIAMI CLASSES/6 SPRING2025/CSC 542/project')

library(tidyverse)

elo_data <- read_csv("./data/clean_elo_data.csv")
player_data <- read_csv("./data/player_data.csv")
match_data <- read_csv("./data/major_int_tournaments.csv")

head(elo_data)
head(player_data)
head(match_data)

grouped_players <- player_data %>%
  group_by(nation) %>%
  nest() %>%
  rename(player_stats = data)


Match_Result <- match_data %>%
  left_join(
    elo_data %>% rename(Home_Elo = rating, Home_Elo_Change = change), 
    join_by(Home == team, closest(Date >= date))
  ) %>%
  select(-date) %>%
  left_join(
    elo_data %>% rename(Away_Elo = rating, Away_Elo_Change = change), 
    join_by(Away == team, closest(Date >= date))
  ) %>%
  select(-date) %>%
  left_join(grouped_players, by = c("Home" = "nation")) %>%
  rename(Home_Players = player_stats) %>%
  left_join(grouped_players, by = c("Away" = "nation")) %>%
  rename(Away_Players = player_stats) %>%

  mutate(
    Home_Elo = replace_na(Home_Elo, 1500),
    Away_Elo = replace_na(Away_Elo, 1500),
    Home_Elo_Change = replace_na(Home_Elo_Change, 0),
    Away_Elo_Change = replace_na(Away_Elo_Change, 0),
    Home_Players = map(Home_Players, ~if(is.null(.x)) tibble() else .x),
    Away_Players = map(Away_Players, ~if(is.null(.x)) tibble() else .x)
  )

Match_Result <- Match_Result %>%
  select(-c(Season_End_Year, Competition_Name, Gender, Country, Round, Wk, Day, MatchURL, Notes, Referee, Time, Venue, 
            Home_Elo_Change, Away_Elo_Change, Attendance, Home_xG, Away_xG))

Match_Result_Swapped <- Match_Result %>%
  rename(
    Home = Away,
    Away = Home,
    Home_Elo = Away_Elo,
    Away_Elo = Home_Elo,
    Home_Players = Away_Players,
    Away_Players = Home_Players,
    HomeGoals = AwayGoals,
    AwayGoals = HomeGoals,
  )
Match_Result_Final <- bind_rows(Match_Result, Match_Result_Swapped)

nrow(Match_Result)
nrow(Match_Result_Final) 
head(Match_Result_Final)

