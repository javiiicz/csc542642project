# Data Directory

This folder contains the datasets and result tables used in the final project.

## Layout

- `MatchData.csv`: original baseline dataset used by the legacy logistic regression and SVM scripts.
- `legacy_svm/`: copy of the original baseline dataset for the partner SVM workflow.
- `processed/player_features/`: player and national-team feature tables used by the current World Cup models.
- `processed/matches/`: curated match-history datasets used for model training and evaluation.
- `processed/tournament/`: World Cup group assignments, bracket slot mappings, and projected bracket inputs.
- `reference/`: reference files such as the official schedule PDF and team/player mapping files.
- `results/`: final model metrics, predictions, confusion matrices, bracket outputs, and presentation assets.

Large raw source dumps are not included. The final models use the curated processed tables in this directory.
