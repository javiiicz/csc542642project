# Soccer World Cup Prediction Through Statistical Models

This repository contains the code, data, and documentation for predicting international soccer match outcomes using statistical machine learning models. Developed for CSC 542/642, this project focuses on forecasting World Cup tournament results by analyzing historical match data and engineering player-specific features. 

## Repository Structure

```text
.
├── data
│   └── MatchData.csv                 # Final processed dataset used for model training
├── docs
│   └── WorldCupPredictionCSC642542.pdf # Comprehensive research report and methodology
├── README.md
└── src
    ├── data_processing
    │   ├── data_grouping.r           # Aggregates and groups raw data by match/team
    │   ├── data_prep.r               # Cleans and formats initial datasets
    │   └── player_feature_engineering.r # Extracts and builds predictive player metrics
    ├── lr_model.r                    # Logistic Regression model implementation
    └── svm_model.r                   # Support Vector Machine model implementation

    Here is a polished, professional README that takes your existing structure and draft text and expands them into a standard, portfolio-ready format. You can copy and paste the block below directly into your `README.md` file.
```

## Data Pipeline

The project follows a linear data processing workflow to prepare historical soccer data for predictive modeling. 

The scripts located in the `/src/data_processing` directory must be run first to generate the final dataset:
1. **`data_prep.r`**: Handles initial cleaning, missing value imputation, and formatting of the raw data.
2. **`player_feature_engineering.r`**: Constructs key predictive metrics (e.g., player performance, team strength).
3. **`data_grouping.r`**: Aggregates the engineered features into a single, cohesive dataset.

Executing these scripts outputs `/data/MatchData.csv`, which serves as the foundation for all statistical modeling.

## Statistical Modeling

Once the `MatchData.csv` file is generated, the models can be trained and evaluated using the scripts in the `/src` directory:

* **Logistic Regression (`lr_model.r`)**: Establishes a baseline statistical model to predict match outcomes (Win/Loss/Draw) based on team and player features.
* **Support Vector Machine (`svm_model.r`)**: A more complex, non-linear approach mapping feature space topology to classify match results.