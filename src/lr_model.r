# Project - Linear Regression Model
library(glmnet)
library(caret)
library(tidyverse)

Match_Data_Base <- read_csv("~/Downloads/MatchData.csv")
total_rows <- nrow(Match_Data_Base)
train_size <- floor(0.8 * total_rows)
train_data <- Match_Data_Base[1:train_size, ]
test_data <- Match_Data_Base[(train_size + 1):total_rows, ]
scaling_params <- preProcess(train_data %>% select(starts_with("diff_")), 
                             method = c("center", "scale"))
train_data_scaled <- predict(scaling_params, train_data)
test_data_scaled <- predict(scaling_params, test_data)

# Convert scaled dataframes into matrices
x_train <- model.matrix(Outcome ~ . - 1 - Date, data = train_data_scaled) 
y_train <- train_data_scaled$Outcome

x_test <- model.matrix(Outcome ~ . - 1 - Date, data = test_data_scaled)
y_test <- test_data_scaled$Outcome

# Calculate Class Weights 
class_counts <- table(y_train)
total_samples <- length(y_train)
num_classes <- length(class_counts)

class_weights <- total_samples / (num_classes * class_counts)

cat("\nCalculated Class Weights:\n")
print(class_weights)
observation_weights <- class_weights[as.character(y_train)]

# CV Lasso with weights
cv_lasso <- cv.glmnet(
  x = x_train, 
  y = y_train, 
  family = "multinomial", 
  alpha = 1, 
  weights = as.numeric(observation_weights)
)

predictions <- predict(cv_lasso, newx = x_test, s = "lambda.min", type = "class")
predictions <- as.vector(predictions)

actuals <- factor(y_test)
preds <- factor(predictions, levels = levels(actuals))

conf_mat <- confusionMatrix(data = preds, reference = actuals, mode = "prec_recall")
cat("\n--- Full Confusion Matrix & Statistics ---\n")
print(conf_mat)
accuracy <- conf_mat$overall["Accuracy"]
metrics_df <- as.data.frame(conf_mat$byClass)
precision <- metrics_df$Precision
recall <- metrics_df$Recall
f1_score <- metrics_df$F1
cat("\n--- Extracted Metrics ---\n")
cat("Overall Accuracy:", round(accuracy, 4), "\n\n")
cat("Per-Class Metrics:\n")
per_class_summary <- data.frame(
  Class = rownames(metrics_df),
  Precision = round(precision, 4),
  Recall = round(recall, 4),
  F1_Score = round(f1_score, 4)
)
print(per_class_summary)

# View Coefficients and Plot
print(coef(cv_lasso, s = "lambda.min"))
plot(cv_lasso$glmnet.fit, xvar = "lambda", label = TRUE)
abline(v = -log(cv_lasso$lambda.min), col = "red", lty = 2)

