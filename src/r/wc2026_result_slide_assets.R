source("wc2026_model_comparison.R")

library(dplyr)
library(ggplot2)
library(grid)
library(readr)
library(scales)
library(stringr)
library(tibble)
library(workflows)

RESULT_ASSETS_DIR <- file.path(MODEL_OUT_DIR, "hybrid_knockout_outputs", "slide_assets")
dir.create(RESULT_ASSETS_DIR, recursive = TRUE, showWarnings = FALSE)

model_name_lookup <- function(model_metrics_csv = file.path(MODEL_OUT_DIR, "model_metrics.csv")) {
  read_csv(model_metrics_csv, show_col_types = FALSE) |>
    mutate(model_index = row_number()) |>
    select(model_index, model)
}

load_model_predictions <- function(
  predictions_csv = file.path(MODEL_OUT_DIR, "model_predictions.csv"),
  model_metrics_csv = file.path(MODEL_OUT_DIR, "model_metrics.csv")
) {
  preds <- read_csv(predictions_csv, show_col_types = FALSE)
  lookup <- model_name_lookup(model_metrics_csv)
  preds |>
    left_join(lookup, by = "model_index") |>
    mutate(
      model = coalesce(model, paste0("model_", model_index)),
      team_win = factor(team_win, levels = c("loss", "win")),
      .pred_class = factor(.pred_class, levels = c("loss", "win"))
    )
}

classification_summary <- function(df, model_nm) {
  sub <- df |>
    filter(model == model_nm)

  if (nrow(sub) == 0) {
    stop("No predictions found for model: ", model_nm, call. = FALSE)
  }

  cm <- table(Predicted = sub$.pred_class, Actual = sub$team_win)

  tn <- as.integer(cm["loss", "loss"])
  fp <- as.integer(cm["win", "loss"])
  fn <- as.integer(cm["loss", "win"])
  tp <- as.integer(cm["win", "win"])

  precision_loss <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
  recall_loss <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  f1_loss <- if (is.na(precision_loss) || is.na(recall_loss) || (precision_loss + recall_loss) == 0) {
    NA_real_
  } else {
    2 * precision_loss * recall_loss / (precision_loss + recall_loss)
  }

  precision_win <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  recall_win <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  f1_win <- if (is.na(precision_win) || is.na(recall_win) || (precision_win + recall_win) == 0) {
    NA_real_
  } else {
    2 * precision_win * recall_win / (precision_win + recall_win)
  }

  tibble(
    model = model_nm,
    n_test = nrow(sub),
    accuracy = mean(sub$team_win == sub$.pred_class, na.rm = TRUE),
    tn = tn,
    fp = fp,
    fn = fn,
    tp = tp,
    loss_precision = precision_loss,
    loss_recall = recall_loss,
    loss_f1 = f1_loss,
    win_precision = precision_win,
    win_recall = recall_win,
    win_f1 = f1_win
  )
}

classification_report <- function(df, model_nm) {
  sub <- df |>
    filter(model == model_nm)

  cm <- table(Predicted = sub$.pred_class, Actual = sub$team_win)

  tibble(
    model = model_nm,
    class = c("loss", "win"),
    precision = c(
      as.numeric(cm["loss", "loss"]) / max(1, as.numeric(cm["loss", "loss"]) + as.numeric(cm["loss", "win"])),
      as.numeric(cm["win", "win"]) / max(1, as.numeric(cm["win", "win"]) + as.numeric(cm["win", "loss"]))
    ),
    recall = c(
      as.numeric(cm["loss", "loss"]) / max(1, as.numeric(cm["loss", "loss"]) + as.numeric(cm["win", "loss"])),
      as.numeric(cm["win", "win"]) / max(1, as.numeric(cm["win", "win"]) + as.numeric(cm["loss", "win"]))
    ),
    f1 = c(
      {
        p <- as.numeric(cm["loss", "loss"]) / max(1, as.numeric(cm["loss", "loss"]) + as.numeric(cm["loss", "win"]))
        r <- as.numeric(cm["loss", "loss"]) / max(1, as.numeric(cm["loss", "loss"]) + as.numeric(cm["win", "loss"]))
        if ((p + r) == 0) NA_real_ else 2 * p * r / (p + r)
      },
      {
        p <- as.numeric(cm["win", "win"]) / max(1, as.numeric(cm["win", "win"]) + as.numeric(cm["win", "loss"]))
        r <- as.numeric(cm["win", "win"]) / max(1, as.numeric(cm["win", "win"]) + as.numeric(cm["loss", "win"]))
        if ((p + r) == 0) NA_real_ else 2 * p * r / (p + r)
      }
    ),
    support = c(
      as.integer(cm[, "loss"] |> sum()),
      as.integer(cm[, "win"] |> sum())
    )
  )
}

make_confusion_plot <- function(df, model_nm, title_suffix = "") {
  sub <- df |>
    filter(model == model_nm)

  cm <- as.data.frame(table(Predicted = sub$.pred_class, Actual = sub$team_win)) |>
    mutate(
      Predicted = factor(Predicted, levels = c("win", "loss")),
      Actual = factor(Actual, levels = c("loss", "win"))
    )

  accuracy <- mean(sub$team_win == sub$.pred_class, na.rm = TRUE)
  n_test <- nrow(sub)

  ggplot(cm, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = Freq), size = 8, fontface = "bold") +
    scale_fill_gradient(low = "#f5f5f5", high = "#153e75") +
    scale_x_discrete(labels = c(loss = "Loss", win = "Win")) +
    scale_y_discrete(labels = c(win = "Win", loss = "Loss")) +
    coord_equal() +
    labs(
      title = paste0(pretty_model_name(model_nm), " Test Set Confusion Matrix"),
      subtitle = paste0("Accuracy: ", percent(accuracy, accuracy = 0.1), " | N = ", n_test, title_suffix),
      x = "Actual",
      y = "Predicted",
      fill = "Count"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold")
    )
}

wrap_split_name <- function(x, width = 24) {
  x |>
    str_replace_all("_", " ") |>
    str_wrap(width = width)
}

pretty_model_name <- function(model_nm) {
  switch(
    model_nm,
    rand_forest = "Random Forest",
    xgboost = "XGBoost",
    str_to_title(str_replace_all(model_nm, "_", " "))
  )
}

build_tree_layout <- function(tree_df, node_id = 0, depth = 0, next_leaf_x = 1, max_depth = Inf) {
  node <- tree_df |>
    filter(nodeID == node_id)

  if (nrow(node) != 1) {
    stop("Could not find nodeID ", node_id, " in ranger tree data.", call. = FALSE)
  }

  node <- node[1, ]

  is_cutoff <- is.finite(max_depth) && depth >= max_depth

  if (isTRUE(node$terminal) || is_cutoff) {
    leaf_label <- if (isTRUE(node$terminal)) {
      paste0(
        "Leaf\n",
        "P(loss) = ", number(node$pred.loss, accuracy = 0.01), "\n",
        "P(win) = ", number(node$pred.win, accuracy = 0.01)
      )
    } else {
      paste0(
        wrap_split_name(node$splitvarName),
        "\n<= ", signif(node$splitval, 3),
        "\n(truncated)"
      )
    }

    node_row <- tibble(
      nodeID = node_id,
      x = next_leaf_x,
      y = -depth,
      depth = depth,
      terminal = TRUE,
      label = leaf_label,
      fill = if (isTRUE(node$terminal)) {
        if_else(node$pred.win >= node$pred.loss, "#cfe8ff", "#ffe3d1")
      } else {
        "#ededed"
      }
    )

    return(list(
      nodes = node_row,
      edges = tibble(),
      next_leaf_x = next_leaf_x + 1,
      x = next_leaf_x
    ))
  }

  left <- build_tree_layout(tree_df, node$leftChild, depth + 1, next_leaf_x, max_depth = max_depth)
  right <- build_tree_layout(tree_df, node$rightChild, depth + 1, left$next_leaf_x, max_depth = max_depth)
  x <- mean(c(left$x, right$x))

  split_label <- paste0(
    wrap_split_name(node$splitvarName),
    "\n<= ",
    signif(node$splitval, 3)
  )

  node_row <- tibble(
    nodeID = node_id,
    x = x,
    y = -depth,
    depth = depth,
    terminal = FALSE,
    label = split_label,
    fill = "#d9d9d9"
  )

  edge_rows <- tibble(
    x = c(x, x),
    y = c(-depth, -depth),
    xend = c(left$x, right$x),
    yend = c(-(depth + 1), -(depth + 1))
  )

  list(
    nodes = bind_rows(left$nodes, right$nodes, node_row),
    edges = bind_rows(left$edges, right$edges, edge_rows),
    next_leaf_x = right$next_leaf_x,
    x = x
  )
}

make_rf_tree_plot <- function(model_suite_rds = file.path(MODEL_OUT_DIR, "trained_model_suite.rds"),
                              tree_id = 1,
                              max_depth = Inf) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("ranger is required to plot the tree.", call. = FALSE)
  }

  suite <- readRDS(model_suite_rds)
  rf_workflow <- suite[[3]]$fit
  rf_engine <- workflows::extract_fit_engine(rf_workflow)
  tree_df <- ranger::treeInfo(rf_engine, tree = tree_id)

  layout <- build_tree_layout(tree_df, node_id = 0, depth = 0, next_leaf_x = 1, max_depth = max_depth)
  nodes <- layout$nodes |>
    mutate(
      node_type = if_else(terminal, "leaf", "split"),
      text_color = if_else(terminal, "#102a43", "#111111")
    )

  x_max <- max(nodes$x, na.rm = TRUE)
  y_min <- min(nodes$y, na.rm = TRUE)

  ggplot() +
    geom_segment(
      data = layout$edges,
      aes(x = x, y = y, xend = xend, yend = yend),
      linewidth = 0.7,
      color = "#6b7280",
      arrow = arrow(type = "closed", length = unit(0.12, "inches"))
    ) +
    geom_label(
      data = nodes,
      aes(x = x, y = y, label = label, fill = fill, color = text_color),
      linewidth = 0.35,
      size = 3.6,
      fontface = "bold",
      label.r = unit(0.15, "lines"),
      label.padding = unit(0.22, "lines"),
      lineheight = 0.95
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(limits = c(0.2, x_max + 0.8), expand = c(0, 0)) +
    scale_y_continuous(limits = c(y_min - 0.7, 0.5), expand = c(0, 0)) +
    coord_equal(clip = "off") +
    labs(
      title = "Representative tree from the best-performing random forest model",
      subtitle = if (is.finite(max_depth)) {
        paste0("Tree 1 from the saved ranger fit; shown to depth ", max_depth, " for readability")
      } else {
        "Tree 1 from the saved ranger fit; split variables are standardized matchup differences"
      },
      caption = if (is.finite(max_depth)) {
        paste0("Holdout test set: accuracy 83.3%, ROC AUC 0.969, log loss 1.197 | truncated at depth ", max_depth)
      } else {
        "Holdout test set: accuracy 83.3%, ROC AUC 0.969, log loss 1.197"
      }
    ) +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12, color = "#374151"),
      plot.caption = element_text(size = 11, color = "#374151"),
      plot.margin = margin(10, 20, 10, 20)
    )
}

generate_result_slide_assets_from <- function(
  predictions_csv,
  model_metrics_csv,
  model_suite_rds,
  output_dir,
  keep_models = c("rand_forest", "xgboost"),
  tree_max_depth = Inf
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  preds <- load_model_predictions(
    predictions_csv = predictions_csv,
    model_metrics_csv = model_metrics_csv
  )

  preds <- preds |> filter(model %in% keep_models)

  summaries <- bind_rows(lapply(keep_models, function(m) classification_summary(preds, m)))
  reports <- bind_rows(lapply(keep_models, function(m) classification_report(preds, m)))

  write_csv(summaries, file.path(output_dir, "test_set_summary.csv"))
  write_csv(reports, file.path(output_dir, "test_set_classification_report.csv"))

  for (m in keep_models) {
    cm_wide <- preds |>
      filter(model == m) |>
      mutate(
        Predicted = as.character(.pred_class),
        Actual = as.character(team_win)
      ) |>
      count(Predicted, Actual, name = "count") |>
      tidyr::pivot_wider(names_from = Actual, values_from = count, values_fill = 0) |>
      arrange(Predicted)

    cm_long <- preds |>
      filter(model == m) |>
      mutate(
        Predicted = as.character(.pred_class),
        Actual = as.character(team_win)
      ) |>
      count(Predicted, Actual, name = "count") |>
      arrange(Predicted, Actual)

    write_csv(cm_wide, file.path(output_dir, paste0(m, "_confusion_matrix_wide.csv")))
    write_csv(cm_long, file.path(output_dir, paste0(m, "_confusion_matrix_long.csv")))

    cm_plot <- make_confusion_plot(preds, m)
    ggsave(
      filename = file.path(output_dir, paste0(m, "_confusion_matrix.png")),
      plot = cm_plot,
      width = 9,
      height = 7,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  }

  tree_plot <- make_rf_tree_plot(model_suite_rds = model_suite_rds, max_depth = tree_max_depth)
  ggsave(
    filename = file.path(output_dir, "rand_forest_representative_tree.png"),
    plot = tree_plot,
    width = 14,
    height = 10,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  invisible(list(
    summaries = summaries,
    reports = reports
  ))
}

generate_result_slide_assets <- function() {
  generate_result_slide_assets_from(
    predictions_csv = file.path(MODEL_OUT_DIR, "model_predictions.csv"),
    model_metrics_csv = file.path(MODEL_OUT_DIR, "model_metrics.csv"),
    model_suite_rds = file.path(MODEL_OUT_DIR, "trained_model_suite.rds"),
    output_dir = RESULT_ASSETS_DIR,
    tree_max_depth = Inf
  )
}

main <- function() {
  generate_result_slide_assets()
}

if (identical(Sys.getenv("RUN_WC2026_RESULT_ASSETS", unset = "0"), "1")) {
  main()
}
