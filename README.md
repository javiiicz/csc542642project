# CSC 542/642 World Cup Prediction

This repository contains the final project package for predicting 2026 FIFA World Cup outcomes from international match history, xG-based player features, team-level feature engineering, and tournament simulation.

The project has two modeling tracks:

- A legacy logistic regression / SVM baseline using the original class project dataset.
- The current World Cup pipeline using xG-derived national-team features, recent and full-history match results, Random Forest, XGBoost, and bracket simulation outputs.

GitHub repository: <https://github.com/javiiicz/csc542642project>

## Repository Structure

```text
.
├── data
│   ├── MatchData.csv                         # Original baseline dataset used by LR/SVM scripts
│   ├── legacy_svm/                           # Copy of the legacy baseline dataset
│   ├── processed/
│   │   ├── matches/                          # Curated match-history training files
│   │   ├── player_features/                  # xG/player-to-national-team feature tables
│   │   └── tournament/                       # World Cup groups, bracket slots, projected seed paths
│   ├── reference/                            # Reference mapping files and official schedule PDF
│   └── results/                              # Final model metrics, predictions, plots, and bracket outputs
├── docs
│   └── WorldCupPredictionCSC642542.pdf       # Presentation deck
├── src
│   ├── data_processing/                      # Original baseline preprocessing scripts
│   ├── notebooks/                            # Notebook-based scraping, merging, and visualization work
│   ├── python/                               # Scraping and data merge utilities
│   ├── r/                                    # Current World Cup modeling and simulation pipeline
│   ├── lr_model.r                            # Legacy logistic regression baseline
│   └── svm_model.r                           # Legacy SVM baseline
└── README.md
```

## Data

The current modeling pipeline uses only the curated files under `data/processed` and `data/reference`.

Important files:

- `data/processed/player_features/understat_tm_merged_specific_nation_filtered.csv`: merged player-level xG and Transfermarkt-style national-team data.
- `data/processed/player_features/team_current_features.csv`: weighted current-form national-team feature table.
- `data/processed/player_features/team_season_features.csv`: nation-season feature table.
- `data/processed/matches/national_team_games_h2h_wc_teams_2018_2026.csv`: curated recent head-to-head match data for qualified World Cup teams.
- `data/processed/matches/historical_matches_combined_1672.csv`: full-history combined match dataset used for the 80/20 holdout branch.
- `data/processed/tournament/`: group assignments, bracket slots, seed paths, and projected knockout slot assignments.
- `data/reference/FWC26 Match Schedule_v17_10042026_EN.pdf`: official schedule reference used to map the bracket.

Large raw Transfermarkt dumps are intentionally not included because the final models use the curated processed tables above.

## Source Code

The current R pipeline lives in `src/r`:

- `wc2026_head_to_head_models.R`: feature preparation, train/test split, model training, and evaluation.
- `wc2026_bracket_simulation.R`: World Cup bracket simulation utilities.
- `wc2026_hybrid_knockout_comparison.R`: hybrid score + winner-model knockout comparison.
- `wc2026_group_stage_expected_points.R`: group-stage expected-points workflow.
- `wc2026_score_knockout_simulation.R`: score-based knockout simulation.
- `wc2026_result_slide_assets.R`: confusion matrices and tree plots used in the presentation.
- `select_winner.R`: model/run selection and report assets.

The notebooks in `src/notebooks` document the scraping, merging, and visualization process. The Python utilities in `src/python` contain the scraping and merge helpers used to generate the player data.

## Methods

The project engineers national-team features from player-level data, including xG per 90, non-penalty xG per 90, xA per 90, shots, key passes, xG chain, xG buildup, squad depth, player concentration, market value, and international experience.

For match prediction, team features are merged into match rows and converted into relative matchup features such as home-minus-away differences and absolute differences. The current model suite includes Random Forest and XGBoost.

## Evaluation

The current full-history evaluation uses a chronological 80/20 holdout:

- Total match rows: 1,672
- Test rows: 335
- Test window: June 10, 2022 through January 18, 2026

Metrics reported:

- Accuracy
- ROC AUC
- Mean log loss
- Confusion matrix
- Per-class precision, recall, and F1

Final result files are under `data/results/full_history_80_20`.

## Results

The final 80/20 holdout metrics are stored in:

- `data/results/full_history_80_20/winner_model_metrics.csv`
- `data/results/full_history_80_20/hybrid_model_summary.csv`
- `data/results/full_history_80_20/slide_assets/test_set_summary.csv`

Model-specific bracket outputs are under:

- `data/results/full_history_80_20/models/rand_forest`
- `data/results/full_history_80_20/models/xgboost`
- `data/results/full_history_80_20/models/score_only`

## Reproducibility Notes

The scripts were developed from the project root that originally contained `wc2026_outputs/`. For the final submission package, the curated datasets and outputs have been copied into `data/` so the repository contains the files used in the report and presentation.

If rerunning the pipeline from scratch, adjust the paths in the R scripts or recreate the expected `wc2026_outputs/` directory structure from the files in `data/processed`.
