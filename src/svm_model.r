# Project - SVM Model
library(e1071)
library(tidyverse)
library(caret)

Match_Data_Base <- read_csv("~/Downloads/MatchData.csv")
Match_Data_Base$Outcome <- as.factor(Match_Data_Base$Outcome)

total_rows <- nrow(Match_Data_Base)
train_size <- floor(0.8 * total_rows)

train_data <- Match_Data_Base[1:train_size, ]
test_data <- Match_Data_Base[(train_size + 1):total_rows, ]

# Preprocessing
scaling_params <- preProcess(train_data %>% select(starts_with("diff_")), 
                             method = c("center", "scale"))
train_data_scaled <- predict(scaling_params, train_data)
test_data_scaled <- predict(scaling_params, test_data)

train_model_data <- train_data_scaled %>% select(-Date)
test_model_data <- test_data_scaled %>% select(-Date)

# Calculate weights
class_counts <- table(train_model_data$Outcome)
total_samples <- nrow(train_model_data)
num_classes <- length(class_counts)

smart_weights <- total_samples / (num_classes * class_counts)
cat("\nClass Weights:\n")
print(smart_weights)

# CV SVM - Radial
set.seed(1)
tune_out_radial <- tune(
  svm,
  Outcome ~ .,
  data = train_model_data,
  kernel = "radial",
  ranges = list(
    cost = 10^(-2:3),
    gamma = 10^(-3:1)
  ),
  scale = FALSE,
  class.weights = smart_weights
)

# CV SVM - Linear
set.seed(1)
tune_out_linear <- tune(
  svm,
  Outcome ~ .,
  data = train_model_data,
  kernel = "linear",
  ranges = list(
    cost = c(0.01, 0.1, 1, 10, 100)
  ),
  scale = FALSE,
  class.weights = smart_weights
)

cat("\n=== Radial Best Model Summary ===\n")
best_svm_radial <- tune_out_radial$best.model
print(summary(best_svm_radial))

cat("\n=== Linear Best Model Summary ===\n")
best_svm_linear <- tune_out_linear$best.model
print(summary(best_svm_linear))

evaluate_svm_model <- function(model, test_data, model_name) {
  cat("\n=============================================\n")
  cat("Evaluating:", model_name, "\n")
  cat("=============================================\n")
  
  test_predictions <- predict(model, test_data)
  
  cat("\nConfusion Matrix:\n")
  conf_matrix <- table(Predicted = test_predictions, Actual = test_data$Outcome)
  print(conf_matrix)
  
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  print(paste("Overall Accuracy:", round(accuracy, 4) * 100, "%"))
  
  precision <- diag(conf_matrix) / rowSums(conf_matrix)
  recall <- diag(conf_matrix) / colSums(conf_matrix)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  metrics_summary <- data.frame(Precision = precision, Recall = recall, F1 = f1_score)
  print("\nMetrics per Class:")
  print(round(metrics_summary, 4))
  
  cat("\nMacro-Average Precision:", round(mean(precision, na.rm = TRUE), 4))
  cat("\nMacro-Average Recall:", round(mean(recall, na.rm = TRUE), 4), "\n")
  
  return(test_predictions)
}

preds_radial <- evaluate_svm_model(best_svm_radial, test_model_data, "Radial Kernel")
preds_linear <- evaluate_svm_model(best_svm_linear, test_model_data, "Linear Kernel")


outcome_col <- which(names(test_model_data) == "Outcome")
pca_res <- prcomp(test_model_data[, -outcome_col], scale. = FALSE)

plot_svm_pca <- function(predictions, model_name) {
  plot_data <- data.frame(
    PC1 = pca_res$x[, 1],
    PC2 = pca_res$x[, 2],
    Actual = test_model_data$Outcome,
    Predicted = predictions
  )
  
  p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Predicted, shape = Actual)) +
    geom_point(alpha = 0.8, size = 3) +
    theme_minimal() +
    labs(
      title = paste("SVM Test Predictions on PCA-Reduced Data (", model_name, ")", sep=""),
      subtitle = "Shapes = True Outcome | Colors = Model Prediction",
      x = paste0("Principal Component 1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)"),
      y = paste0("Principal Component 2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)")
    )
  
  return(p)
}

plot_radial <- plot_svm_pca(preds_radial, "Radial Kernel")
plot_linear <- plot_svm_pca(preds_linear, "Linear Kernel")

print(plot_radial)
print(plot_linear)