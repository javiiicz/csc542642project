# WC 2026 schedule and bracket map
#
# This file captures the finalized FIFA 2026 bracket structure from the schedule PDF.
# It is intentionally separate from the ML pipeline:
# - `wc2026_head_to_head_models.R` handles feature engineering and model training.
# - This file handles bracket routing and seed/path lookup.
#
# The round-of-32 third-place slots are listed as pools (for example `3ABCDF`).
# The exact team-to-slot assignment for third-place qualifiers depends on the final
# set of eight best third-place teams. That mapping is not hardcoded here.

suppressPackageStartupMessages({
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Missing package: tibble. Install it with install.packages('tibble').", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Missing package: dplyr. Install it with install.packages('dplyr').", call. = FALSE)
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Missing package: readr. Install it with install.packages('readr').", call. = FALSE)
  }
  library(tibble)
  library(dplyr)
  library(readr)
})

OUT_DIR <- file.path(getwd(), "wc2026_outputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

fwc26_group_assignments <- function() {
  tibble::tribble(
    ~group, ~team_code, ~team_name,
    "A", "MEX", "Mexico",
    "A", "RSA", "South Africa",
    "A", "KOR", "Korea Republic",
    "A", "CZE", "Czechia",
    "B", "CAN", "Canada",
    "B", "BIH", "Bosnia and Herzegovina",
    "B", "QAT", "Qatar",
    "B", "SUI", "Switzerland",
    "C", "BRA", "Brazil",
    "C", "MAR", "Morocco",
    "C", "HAI", "Haiti",
    "C", "SCO", "Scotland",
    "D", "USA", "United States",
    "D", "PAR", "Paraguay",
    "D", "AUS", "Australia",
    "D", "TUR", "Turkiye",
    "E", "GER", "Germany",
    "E", "CUW", "Curacao",
    "E", "CIV", "Cote d'Ivoire",
    "E", "ECU", "Ecuador",
    "F", "NED", "Netherlands",
    "F", "JPN", "Japan",
    "F", "SWE", "Sweden",
    "F", "TUN", "Tunisia",
    "G", "BEL", "Belgium",
    "G", "EGY", "Egypt",
    "G", "IRN", "Iran",
    "G", "NZL", "New Zealand",
    "H", "ESP", "Spain",
    "H", "CPV", "Cabo Verde",
    "H", "KSA", "Saudi Arabia",
    "H", "URU", "Uruguay",
    "I", "FRA", "France",
    "I", "SEN", "Senegal",
    "I", "IRQ", "Iraq",
    "I", "NOR", "Norway",
    "J", "ARG", "Argentina",
    "J", "ALG", "Algeria",
    "J", "AUT", "Austria",
    "J", "JOR", "Jordan",
    "K", "POR", "Portugal",
    "K", "COD", "Congo DR",
    "K", "UZB", "Uzbekistan",
    "K", "COL", "Colombia",
    "L", "ENG", "England",
    "L", "CRO", "Croatia",
    "L", "GHA", "Ghana",
    "L", "PAN", "Panama"
  ) |>
    arrange(group, team_code)
}

fwc26_bracket_slots <- function() {
  tibble::tribble(
    ~match_number, ~stage, ~team_a_slot, ~team_b_slot, ~winner_advances_to, ~loser_advances_to, ~time_et,
    73,  "round_of_32",  "2A",      "2B",       90L,  NA_integer_, "15:00",
    74,  "round_of_32",  "1E",      "3ABCDF",   89L,  NA_integer_, "16:30",
    75,  "round_of_32",  "1F",      "2C",       90L,  NA_integer_, "21:00",
    76,  "round_of_32",  "1C",      "2F",       91L,  NA_integer_, "13:00",
    77,  "round_of_32",  "1I",      "3CDFGH",   89L,  NA_integer_, "17:00",
    78,  "round_of_32",  "2E",      "2I",       91L,  NA_integer_, "13:00",
    79,  "round_of_32",  "1A",      "3CEFHI",   92L,  NA_integer_, "21:00",
    80,  "round_of_32",  "1L",      "3EHIJK",   92L,  NA_integer_, "12:00",
    81,  "round_of_32",  "1D",      "3BEFIJ",   94L,  NA_integer_, "20:00",
    82,  "round_of_32",  "1G",      "3AEHIJ",   94L,  NA_integer_, "16:00",
    83,  "round_of_32",  "2K",      "2L",       93L,  NA_integer_, "19:00",
    84,  "round_of_32",  "1H",      "2J",       93L,  NA_integer_, "15:00",
    85,  "round_of_32",  "1B",      "3EFGIJ",   96L,  NA_integer_, "23:00",
    86,  "round_of_32",  "1J",      "2H",       95L,  NA_integer_, "18:00",
    87,  "round_of_32",  "1K",      "3DEIJL",   96L,  NA_integer_, "21:30",
    88,  "round_of_32",  "2D",      "2G",       95L,  NA_integer_, "14:00",
    89,  "round_of_16",  "W74",     "W77",      97L,  NA_integer_, "17:00",
    90,  "round_of_16",  "W73",     "W75",      97L,  NA_integer_, "13:00",
    91,  "round_of_16",  "W76",     "W78",      99L,  NA_integer_, "16:00",
    92,  "round_of_16",  "W79",     "W80",      99L,  NA_integer_, "20:00",
    93,  "round_of_16",  "W83",     "W84",      98L,  NA_integer_, "15:00",
    94,  "round_of_16",  "W81",     "W82",      98L,  NA_integer_, "20:00",
    95,  "round_of_16",  "W86",     "W88",     100L,  NA_integer_, "12:00",
    96,  "round_of_16",  "W85",     "W87",     100L,  NA_integer_, "16:00",
    97,  "quarter_final","W89",     "W90",     101L,  NA_integer_, "16:00",
    98,  "quarter_final","W93",     "W94",     101L,  NA_integer_, "15:00",
    99,  "quarter_final","W91",     "W92",     102L,  NA_integer_, "17:00",
    100, "quarter_final","W95",     "W96",     102L,  NA_integer_, "21:00",
    101, "semi_final",   "W97",     "W98",     104L, 103L,         "15:00",
    102, "semi_final",   "W99",     "W100",    104L, 103L,         "15:00",
    103, "bronze_final", "L101",    "L102",    NA_integer_, NA_integer_, "17:00",
    104, "final",        "W101",    "W102",    NA_integer_, NA_integer_, "15:00"
  ) |>
    arrange(match_number)
}

fwc26_seed_map <- function() {
  tibble::tribble(
    ~slot, ~first_match, ~slot_kind,
    "1A", 79L, "fixed_group",
    "2A", 73L, "fixed_group",
    "1B", 85L, "fixed_group",
    "2B", 73L, "fixed_group",
    "1C", 76L, "fixed_group",
    "2C", 75L, "fixed_group",
    "1D", 81L, "fixed_group",
    "2D", 88L, "fixed_group",
    "1E", 74L, "fixed_group",
    "2E", 78L, "fixed_group",
    "1F", 75L, "fixed_group",
    "2F", 76L, "fixed_group",
    "1G", 82L, "fixed_group",
    "2G", 88L, "fixed_group",
    "1H", 84L, "fixed_group",
    "2H", 86L, "fixed_group",
    "1I", 77L, "fixed_group",
    "2I", 78L, "fixed_group",
    "1J", 86L, "fixed_group",
    "2J", 84L, "fixed_group",
    "1K", 87L, "fixed_group",
    "2K", 83L, "fixed_group",
    "1L", 80L, "fixed_group",
    "2L", 83L, "fixed_group",
    "3ABCDF", 74L, "third_place_pool",
    "3CDFGH", 77L, "third_place_pool",
    "3CEFHI", 79L, "third_place_pool",
    "3BEFIJ", 81L, "third_place_pool",
    "3AEHIJ", 82L, "third_place_pool",
    "3EFGIJ", 85L, "third_place_pool",
    "3EHIJK", 80L, "third_place_pool",
    "3DEIJL", 87L, "third_place_pool"
  )
}

compute_seed_path <- function(first_match, bracket_slots) {
  path <- integer()
  current <- first_match

  while (!is.na(current)) {
    path <- c(path, current)
    row <- bracket_slots |>
      filter(.data$match_number == current)

    if (nrow(row) == 0) break
    current <- row$winner_advances_to[[1]]
  }

  path
}

fwc26_slot_paths <- function() {
  bracket_slots <- fwc26_bracket_slots()
  seed_map <- fwc26_seed_map()

  seed_map |>
    rowwise() |>
    mutate(
      path = list(compute_seed_path(first_match, bracket_slots)),
      path_str = paste(path, collapse = " -> ")
    ) |>
    ungroup()
}

save_fwc26_schedule_tables <- function(out_dir = OUT_DIR) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  bracket_slots <- fwc26_bracket_slots()
  seed_map <- fwc26_slot_paths()
  groups <- fwc26_group_assignments()

  write_csv(bracket_slots, file.path(out_dir, "fwc26_bracket_slots.csv"))
  write_csv(seed_map, file.path(out_dir, "fwc26_seed_paths.csv"))
  write_csv(groups, file.path(out_dir, "fwc26_group_assignments.csv"))

  invisible(list(
    bracket_slots = bracket_slots,
    seed_paths = seed_map,
    group_assignments = groups
  ))
}
