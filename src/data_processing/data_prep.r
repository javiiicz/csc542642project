# Project
rm(list = ls()) 
setwd('/Users/javier/Library/CloudStorage/OneDrive-Personal/UMIAMI CLASSES/6 SPRING2025/CSC 542/project')

# install.packages("remotes")
# remotes::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
library(tidyverse)
# Classified Teams
qualified_teams <- list(
  Hosts = c("Canada", "Mexico", "United States"),
  AFC_Asia = c(
    "Japan", "Iran", "Uzbekistan", "Korea Republic", 
    "Jordan", "Australia", "Qatar", "Saudi Arabia"
  ),
  CAF_Africa = c(
    "Morocco", "Tunisia", "Egypt", "Algeria", "Ghana", 
    "Cape Verde", "South Africa", "Senegal", "Ivory Coast"
  ),
  CONMEBOL_South_America = c(
    "Argentina", "Brazil", "Ecuador", "Colombia", "Paraguay", "Uruguay"
  ),
  UEFA_Europe = c(
    "England", "France", "Croatia", "Portugal", "Norway", 
    "Germany", "Netherlands", "Austria", "Belgium", "Scotland", 
    "Spain", "Switzerland"
  ),
  CONCACAF_North_Central_America = c("Curaçao", "Haiti", "Panama"),
  OFC_Oceania = c("New Zealand"),
  NEW_CLASSIFICATIONS = c("Bosnia and Herzegovina", "Sweden", "Turkey", "Czech Republic", "DR Congo", "Iraq")
)
# this has all the teams currently qualified for the world cup
all_teams <- unname(unlist(qualified_teams))
print(sort(all_teams))

# Players in the Top 5 European and their stats
player_data <- read_csv("./data/player_stats.csv")
unique(sort(player_data$nation))
player_data_clean <- player_data %>%
  mutate(nation = case_match(nation,
                             "Kingdom of the Netherlands" ~ "Netherlands",
                             "Korea, South" ~ "Korea Republic",
                             "Democratic Republic of the Congo" ~ "DR Congo",
                             "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
                             "Cote d'Ivoire" ~ "Ivory Coast",
                             "Türkiye" ~ "Turkey",
                             .default = nation
  )) %>%
  filter(nation %in% all_teams)


setdiff(all_teams, player_data_clean$nation)
setdiff(player_data_clean$nation,all_teams)
# missing teams RN: "Curaçao"


# PAST WORLD CUP MATCHES
mens_intl_comps <- c(
  "AFC Asian Cup",
  "AFC Asian Cup qualification",
  "Africa Cup of Nations",
  "Africa Cup of Nations qualification",
  "CONCACAF Gold Cup",
  "CONMEBOL Copa América",
  "FIFA Confederations Cup",
  "FIFA World Cup",
  "FIFA World Cup Qualification — AFC",
  "FIFA World Cup Qualification — CAF",
  "FIFA World Cup Qualification — CONCACAF",
  "FIFA World Cup Qualification — CONMEBOL",
  "FIFA World Cup Qualification — Inter-confederation play-offs",
  "FIFA World Cup Qualification — OFC",
  "FIFA World Cup Qualification — UEFA",
  "International Friendlies (M)",
  "OFC Nations Cup",
  "UEFA European Football Championship",
  "UEFA European Football Championship Qualifying",
  "UEFA Nations League"
)
# Get data from all those tournaments, keep only after 2021 to match player data
# delete columns with NA
all_results <- load_match_comp_results(comp_name = mens_intl_comps)
major_tournaments <- all_results %>%
  filter(year(Date) >= 2021)

major_tournaments_clean <- major_tournaments %>%
  drop_na(HomeGoals) %>%
  drop_na(AwayGoals) %>%
  mutate(
    Home = str_trim(str_remove(Home, "\\s+[a-z]{2,3}$")),
    Away = str_trim(str_remove(Away, "^[a-z]{2,3}\\s+"))
  ) %>%
  mutate(
    across(c(Home, Away), ~ case_match(.,
                                       "IR Iran"          ~ "Iran",
                                       "Côte d'Ivoire"    ~ "Ivory Coast",
                                       "Bosnia & Herz'na" ~ "Bosnia and Herzegovina",
                                       "Rep. of Ireland"  ~ "Ireland",
                                       "Türkiye"          ~ "Turkey",
                                       "Czechia"          ~ "Czech Republic",
                                       "Trin & Tobago"    ~ "Trinidad and Tobago",
                                       "China PR"         ~ "China",
                                       "Gambia"           ~ "The Gambia",
                                       "Denmark"          ~ "Kingdom of Denmark",
                                       "Congo DR" ~ "DR Congo",
                                       .default = .
    ))
  ) %>%
  filter(
    (Home %in% all_teams & Away %in% all_teams)
  )

# Both Should be empty
setdiff(all_teams, major_tournaments_clean$Home)
setdiff(major_tournaments_clean$Home, all_teams)

elo_data <- read_csv("./data/eloratings.csv")
elo_data <- elo_data %>%
  mutate(
    date = parse_date_time(date, orders = c("ymd", "dmy", "mdy")),
    date = as.Date(date),
    # fix weird spaces
    team = str_squish(team)
  ) %>%
  filter(year(date) >= 2021)
clean_elo_data <- elo_data %>%
  mutate(
    team = case_when(
      team == "Czechia" ~ "Czech Republic",
      team == "South Korea" ~ "Korea Republic",
      team == "Democratic Republic of Congo" ~ "DR Congo",
      TRUE ~ team
    )
  ) %>%
  filter(team %in% all_teams)

write.csv(player_data_clean, file = "./data/player_data.csv", row.names = FALSE)
write.csv(major_tournaments_clean, file = "./data/major_int_tournaments.csv", row.names = FALSE)
write.csv(clean_elo_data, file = "./data/clean_elo_data.csv", row.names = FALSE)



