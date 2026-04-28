# Source Code Directory

This folder contains the source code used to build the datasets, train the models, run the World Cup simulations, and generate the presentation/report assets.

The final curated inputs and outputs used in the paper are stored in `../data`. The original modeling scripts were developed from the project working directory and write intermediate results to `wc2026_outputs/`. For the GitHub submission, the relevant final files were copied into `../data/processed`, `../data/reference`, and `../data/results`.

## Recommended Run Environment

Run commands from the repository root unless a step explicitly says otherwise:

```bash
cd csc542642project
```

Python dependencies:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r src/python/requirements.txt
python -m playwright install chromium
```

R packages used across the current modeling pipeline:

```r
install.packages(c(
  "dplyr", "readr", "stringr", "tibble", "tidyr", "lubridate", "purrr",
  "ggplot2", "scales", "tidymodels", "ranger", "xgboost", "poissonreg",
  "workflows", "grid", "caret", "glmnet", "e1071"
))
```

`worldfootballR` is optional for refreshing historical international match data. If it is not installed, the score-model script falls back to existing local H2H data where possible.

```r
install.packages("remotes")
remotes::install_github("JaseZiv/worldfootballR")
```

## Important Path Note

Most current R scripts define:

```r
OUT_DIR <- file.path(getwd(), "wc2026_outputs")
```

That means a full rerun creates outputs under `wc2026_outputs/` in the current working directory. The repository stores the final selected files under `data/` for submission, but the scripts themselves still write to `wc2026_outputs/`.

If you want to rerun the pipeline inside the repo and then mirror the outputs into the submitted data layout, run the scripts first, then copy the relevant CSV/PNG files into `data/processed` and `data/results`.

## Directory Layout

- `data_processing/`: original partner preprocessing scripts for the baseline LR/SVM dataset.
- `lr_model.r`: baseline multinomial Logistic Regression model.
- `svm_model.r`: baseline SVM model.
- `notebooks/`: exploratory scraping, merging, and visualization notebooks.
- `python/`: scraping and merge utilities used to build player-level Understat/Transfermarkt data.
- `r/`: current World Cup modeling and simulation pipeline.

## Full Reproduction Order

The exact final outputs in the paper come from this high-level order:

1. Build or refresh player-level Understat data.
2. Merge player data with nationality and Transfermarkt profile data.
3. Build the 2026 schedule/bracket mapping.
4. Build xG-based national-team features.
5. Train RF/XGBoost head-to-head models.
6. Build the score model and group-stage expected-points table.
7. Run score-only and hybrid knockout simulations.
8. Generate slide/report assets.
9. Run partner baseline LR/SVM scripts separately.

The final repo already includes the curated outputs, so rerunning is only needed if the data changes.

## Python Data Pipeline

These scripts generate or support the player-level input data used before the R modeling stage.

### `python/understat.py`

Scrapes/loads Understat-style Top 5 European league player-season data for seasons `2122` through `2526`.

Run:

```bash
python src/python/understat.py
```

Main outputs:

- `wc2026_outputs/understat_big5_player_season_2122_2526_full.csv`
- `wc2026_outputs/understat_big5_player_season_2122_2526_wc_filtered.csv`
- season-level folders under `wc2026_outputs/21-22`, `22-23`, etc.

Notes:

- Uses `soccerdata`.
- Understat does not always provide nationality fields directly, so metadata/country mapping may be needed.
- The final submitted player-feature table is stored at `data/processed/player_features/understat_tm_merged_specific_nation_filtered.csv`.

### `python/nation_scrape.py` 

Uses Wikidata-style lookup logic to build or update a player-to-country mapping (LEGACY SCRIPT, it didn't work very well...).

Run:

```bash
python src/python/nation_scrape.py
```

Main output:

- `player_country_map.csv`

Submitted copy:

- `data/reference/player_country_map.csv`

### `python/merge_understat_transfermarkt.py` (LEGACY SCRIPT, Test File that we ended up just using the `../notebooks/scrape_merge.ipynb` to end up performing the merges.)

Merges Understat player-season rows with Transfermarkt active-player/profile rows using fuzzy matching helpers from `transfermarkt_match.py`.

Run:

```bash
python src/python/merge_understat_transfermarkt.py
```

Main outputs depend on the local input paths inside the script, but the final submitted merged dataset is:

- `data/processed/player_features/understat_tm_merged_specific_nation_filtered.csv`

### `python/transfermarkt_match.py`

Helper module for player-name normalization and fuzzy matching between Understat-style names and Transfermarkt records. This is imported by merge scripts rather than run directly.

Typical use:

```python
from transfermarkt_match import merge_understat_with_transfermarkt
```

### `python/soccer_data.py` (LEGACY SCRIPT, worked sometimes other times didn't so we couldn't consistently scrape data from this)

Alternative/older player-data scrape using `soccerdata` and browser fallback logic. This was used during data collection and experimentation.

Run:

```bash
python src/python/soccer_data.py
```

Main outputs:

- `wc2026_outputs/fbref_big5_player_season_2122_2526_raw_merged.csv`
- `wc2026_outputs/fbref_big5_player_season_2122_2526_wc_filtered.csv`

Note: this is retained for reproducibility of exploratory scraping, but the final paper emphasizes Understat-based xG features.

### `python/scraper.py` and `python/fbref_turnstile.py` (LEGACY SCRIPT)

Older scraping utilities and browser/Turnstile support code from the exploratory phase. These are not the primary path for the final submitted RF/XGBoost results.

Run only if refreshing the older scrape:

```bash
python src/python/scraper.py
```

### `python/merge_countries.py` (LEGACY SCRIPT)

Small local utility for joining country mappings. It contains an absolute local path from the development machine, so edit the `base` variable before rerunning on another machine.

## Current R World Cup Pipeline

The current RF/XGBoost, score-model, expected-points, and bracket-simulation workflow is in `src/r`.

Because the R files source each other using relative filenames such as `source("wc2026_model_comparison.R")`, run them from inside `src/r`:

```bash
cd src/r
```

From there, outputs will be written under `src/r/wc2026_outputs/`. If you want outputs at the repository root instead, run from the root and change source paths in the scripts, or source them manually with the correct working directory.

Before running the R pipeline from `src/r`, stage the required submitted input files into the `wc2026_outputs/` location expected by the scripts:

```bash
mkdir -p wc2026_outputs
cp ../../data/processed/player_features/understat_tm_merged_specific_nation_filtered.csv wc2026_outputs/
cp ../../data/processed/player_features/team_current_features.csv wc2026_outputs/
cp ../../data/processed/player_features/team_season_features.csv wc2026_outputs/
cp ../../data/processed/matches/national_team_games_h2h_wc_teams_2018_2026.csv wc2026_outputs/
cp ../../data/processed/matches/historical_matches_combined_1672.csv wc2026_outputs/
```

For an exact rerun of the full-history hybrid workflow, the development-only file `historical_matches_transfermarkt_recent.csv` is also expected by `wc2026_hybrid_knockout_comparison.R`. If your workspace still has the original project-level `wc2026_outputs/` folder next to this GitHub repo, copy it with:

```bash
cp ../../../wc2026_outputs/historical_matches_transfermarkt_recent.csv wc2026_outputs/
```

Otherwise, place it manually at:

```bash
wc2026_outputs/historical_matches_transfermarkt_recent.csv
```

The final GitHub package does not require that raw development file for grading/report review because the curated full-history dataset and final model outputs are already submitted under `data/processed/matches/` and `data/results/`. It is only needed if you want to regenerate the hybrid workflow exactly from the scripts as currently written.

### `wc2026_schedule_map.R` 

Builds the structured 2026 World Cup group assignments, bracket slots, and seed-path lookup tables.

Run from `src/r`:

```bash
Rscript -e "source('wc2026_schedule_map.R'); save_fwc26_schedule_tables()"
```

Main outputs:

- `wc2026_outputs/fwc26_group_assignments.csv`
- `wc2026_outputs/fwc26_bracket_slots.csv`
- `wc2026_outputs/fwc26_seed_paths.csv`

Submitted copies:

- `data/processed/tournament/fwc26_group_assignments.csv`
- `data/processed/tournament/fwc26_bracket_slots.csv`
- `data/processed/tournament/fwc26_seed_paths.csv`

### `wc2026_head_to_head_models.R`

Builds xG-based team features and trains the current RF/XGBoost head-to-head model suite.

Run:

```bash
RUN_WC2026_MODEL_DEMO=1 Rscript wc2026_head_to_head_models.R
```

Main outputs:

- `wc2026_outputs/team_season_features.csv`
- `wc2026_outputs/team_current_features.csv`
- `wc2026_outputs/r_model_outputs/model_metrics.csv`
- `wc2026_outputs/r_model_outputs/model_predictions.csv`
- `wc2026_outputs/r_model_outputs/trained_model_suite.rds`

Submitted feature copies:

- `data/processed/player_features/team_season_features.csv`
- `data/processed/player_features/team_current_features.csv`

### `wc2026_model_comparison.R` 

Runs the earlier deterministic-group model comparison workflow. It trains available winner models and simulates the bracket across models.

Run:

```bash
RUN_WC2026_MODEL_COMPARISON=1 Rscript wc2026_model_comparison.R
```

Main outputs:

- `wc2026_outputs/r_model_outputs/model_comparison_summary.csv`
- `wc2026_outputs/r_model_outputs/model_champion_probabilities.csv`
- `wc2026_outputs/r_model_outputs/model_final_position_probabilities.csv`
- `wc2026_outputs/r_model_outputs/model_sampled_matches.csv`
- `wc2026_outputs/r_model_outputs/deterministic_team_progress.csv`

This workflow was useful during model comparison, but the final paper emphasizes the full-history hybrid outputs.

### `select_winner.R`

Analyzes sampled model runs and chooses representative/high-confidence runs for display.

Run after `wc2026_model_comparison.R`:

```bash
Rscript select_winner.R
```

Main outputs:

- `wc2026_outputs/r_model_outputs/select_winner_outputs/run_scores.csv`
- `wc2026_outputs/r_model_outputs/select_winner_outputs/best_runs_by_model.csv`
- `wc2026_outputs/r_model_outputs/select_winner_outputs/selection_summary.csv`
- model-specific plots and CSV files under `select_winner_outputs/models/`

### `wc2026_group_stage_expected_points.R`

Builds the historical scoreline dataset, trains the Poisson score model, and simulates group-stage expected points.

Run:

```bash
RUN_WC2026_GROUP_STAGE_POINTS=1 Rscript wc2026_group_stage_expected_points.R
```

Main outputs:

- `wc2026_outputs/r_model_outputs/group_stage_outputs/historical_matches_combined.csv`
- `wc2026_outputs/r_model_outputs/group_stage_outputs/score_model_metrics.csv`
- `wc2026_outputs/r_model_outputs/group_stage_outputs/score_model.rds`
- `wc2026_outputs/r_model_outputs/group_stage_outputs/group_stage_fixtures.csv`
- `wc2026_outputs/r_model_outputs/group_stage_outputs/group_stage_match_results.csv`
- `wc2026_outputs/r_model_outputs/group_stage_outputs/group_stage_simulations.csv`
- `wc2026_outputs/r_model_outputs/group_stage_outputs/group_stage_expected_points.csv`

Submitted copies:

- `data/results/group_stage/group_stage_expected_points.csv`
- `data/results/group_stage/score_model_metrics.csv`

### `wc2026_score_knockout_simulation.R`

Runs the score-only knockout simulation using the score model.

Run after `wc2026_group_stage_expected_points.R`:

```bash
RUN_WC2026_SCORE_KNOCKOUT=1 Rscript wc2026_score_knockout_simulation.R
```

Main outputs:

- `wc2026_outputs/r_model_outputs/score_knockout_outputs/score_model_metadata.csv`
- `wc2026_outputs/r_model_outputs/score_knockout_outputs/projected_group_results.csv`
- `wc2026_outputs/r_model_outputs/score_knockout_outputs/projected_slot_assignments.csv`
- `wc2026_outputs/r_model_outputs/score_knockout_outputs/deterministic_matches.csv`
- `wc2026_outputs/r_model_outputs/score_knockout_outputs/champion_probabilities.csv`
- `wc2026_outputs/r_model_outputs/score_knockout_outputs/final_position_probabilities.csv`

### `wc2026_hybrid_knockout_comparison.R`

Runs the final score-plus-winner hybrid workflow. This is the main current pipeline for the RF/XGBoost results in the paper.

Run after `wc2026_group_stage_expected_points.R`:

```bash
RUN_WC2026_HYBRID_KNOCKOUT=1 Rscript -e "source('wc2026_hybrid_knockout_comparison.R'); main(n_sims = 200)"
```

Main outputs:

- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/hybrid_model_summary_recent_vs_full.csv`
- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/hybrid_model_summary_recent_vs_full_80_20.csv`
- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/full_history_80_20/hybrid_model_summary.csv`
- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/full_history_80_20/score_model_metadata.csv`
- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/full_history_80_20/winner_training/model_metrics.csv`
- model folders under `full_history_80_20/models/score_only`, `rand_forest`, and `xgboost`

Submitted copies:

- `data/results/full_history_80_20/hybrid_model_summary.csv`
- `data/results/full_history_80_20/score_model_metadata.csv`
- `data/results/full_history_80_20/winner_model_metrics.csv`
- `data/results/full_history_80_20/winner_model_predictions.csv`

### `wc2026_result_slide_assets.R`

Creates confusion matrices, test-set summary CSV files, classification reports, and representative Random Forest tree plots for presentation/report use.

Run after the model-training outputs exist:

```bash
RUN_WC2026_RESULT_ASSETS=1 Rscript wc2026_result_slide_assets.R
```

Main outputs:

- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/slide_assets/test_set_summary.csv`
- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/slide_assets/test_set_classification_report.csv`
- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/slide_assets/rand_forest_confusion_matrix.png`
- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/slide_assets/rand_forest_representative_tree.png`
- `wc2026_outputs/r_model_outputs/hybrid_knockout_outputs/slide_assets/xgboost_confusion_matrix.png`

Submitted copies:

- `data/results/full_history_80_20/slide_assets/`

### `wc2026_bracket_simulation.R`

Helper library for bracket routing and simulation. It is sourced by other scripts and usually does not need to be run directly.

If needed, source it inside R:

```r
source("wc2026_bracket_simulation.R")
```

## Partner Baseline LR/SVM Workflow

These scripts are retained for the partner baseline models described in the paper. They use `MatchData.csv`, Elo-based features, xG-derived differential features, and an 80/20 split.

Before running, update the hardcoded `~/Downloads/MatchData.csv` path in both scripts to:

```r
read_csv("data/MatchData.csv")
```

or run from a machine where `~/Downloads/MatchData.csv` exists.

### `lr_model.r`

Runs multinomial Logistic Regression with Lasso regularization and class weights.

Run from the repository root after fixing the input path:

```bash
Rscript src/lr_model.r
```

Outputs are printed to the console:

- confusion matrix
- accuracy
- precision/recall/F1 by class
- selected Lasso coefficients
- coefficient path plot

### `svm_model.r`

Runs radial and linear SVM models with cross-validation over cost/gamma grids and class weights.

Run from the repository root after fixing the input path:

```bash
Rscript src/svm_model.r
```

Outputs are printed to the console:

- radial and linear best model summaries
- confusion matrices
- accuracy
- precision/recall/F1 by class
- PCA-style prediction visuals if generated by the local plotting section

## Original Data Processing Scripts

The files in `data_processing/` are the original preprocessing path for the baseline LR/SVM dataset.

- `data_prep.r`: pulls worldfootballR match data, filters World Cup teams, and prepares initial player/match datasets.
- `data_goruping.r`: joins match data with Elo and nested player statistics, then performs symmetrical home/away augmentation.
- `player_feature_engineering.r`: aggregates top-11 player stats and creates differential features for modeling.

These scripts contain local absolute paths from the original development machine. Before running them on a new machine, update `setwd(...)` and input file locations.

Suggested order:

```bash
Rscript src/data_processing/data_prep.r
Rscript src/data_processing/data_goruping.r
Rscript src/data_processing/player_feature_engineering.r
```

## Notebooks

### Used pipeline: 

- `notebooks/scrape_merge.ipynb`: notebook combining scraping from understat, cleaning, merging with the transfermarkt data, and feature review work. 

### Data Visualization:

- `notebooks/data_visuals.ipynb`: exploratory visualization notebook used to inspect xG/player/team relationships.

Run notebooks from the repository root or update relative paths inside the notebook as needed.

## Final Submitted Results

The final report uses these result files:

- `../data/results/group_stage/group_stage_expected_points.csv`
- `../data/results/group_stage/score_model_metrics.csv`
- `../data/results/full_history_80_20/hybrid_model_summary.csv`
- `../data/results/full_history_80_20/winner_model_metrics.csv`
- `../data/results/full_history_80_20/winner_model_predictions.csv`
- `../data/results/full_history_80_20/slide_assets/`

The key paper numbers come from:

- Random Forest accuracy: `0.5821`
- Random Forest log loss: `0.9962`
- XGBoost accuracy: `0.5642`
- XGBoost ROC AUC: `0.5716`
- Score model RMSE: `1.0246`
- Score model MAE: `0.8154`
- 200-simulation hybrid champion outputs in `hybrid_model_summary.csv`
