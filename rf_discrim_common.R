# =========================
# File: R/rf_discrim_common.R
# =========================

ensure_packages <- function(pkgs) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(TRUE) 
}

# Helper function to check if a required file exists
check_required_file <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Input file not found: %s\nPlease ensure the data file is placed in the correct directory.", path), call. = FALSE)
  }
}

# Reads an Excel sheet and uses one column as row names (IDs)
excel_to_df <- function(path, sheet, row_names_col = 1) {
  df <- readxl::read_excel(path, sheet = sheet)
  
  rn <- df[[row_names_col]]
  df[[row_names_col]] <- NULL
  df <- as.data.frame(df, check.names = FALSE)
  rownames(df) <- rn
  
  for (j in seq_along(df)) {
    if (is.character(df[[j]])) {
      x <- gsub(",", "", df[[j]])
      if (all(grepl("^\\s*-?\\d+(\\.\\d+)?\\s*$", x) | x == "")) {
        suppressWarnings(df[[j]] <- as.numeric(ifelse(x == "", NA, x)))
      }
    }
  }
  df
}

# Stop if any non-numeric columns exist.
fix_numeric <- function(df, sheet_name = NULL) {
  non_num <- names(df)[!vapply(df, is.numeric, logical(1))]
  
  if (length(non_num)) {
    msg <- paste0(
      "Non-numeric columns detected",
      if (!is.null(sheet_name)) paste0(" in sheet '", sheet_name, "'") else "",
      ": ",
      paste(non_num, collapse = ", "),
      "\nPlease fix the Excel file so all feature columns are numeric."
    )
    stop(msg, call. = FALSE)
  }
  df
}

# Loads four feature tables (char/pos/phrase/liwc) from one Excel file
load_four_sheets <- function(path_xlsx,
                             sheets = list(char="char", pos="pos", phrase="phrase", liwc="liwc")) {
  check_required_file(path_xlsx)
  
  char_df   <- excel_to_df(path_xlsx, sheets$char)
  pos_df    <- excel_to_df(path_xlsx, sheets$pos)
  phrase_df <- excel_to_df(path_xlsx, sheets$phrase)
  liwc_df   <- excel_to_df(path_xlsx, sheets$liwc)
  
  list(
    char   = fix_numeric(char_df,   sheet_name = sheets$char),
    pos    = fix_numeric(pos_df,    sheet_name = sheets$pos),
    phrase = fix_numeric(phrase_df, sheet_name = sheets$phrase),
    liwc   = fix_numeric(liwc_df,   sheet_name = sheets$liwc)
  )
}

assign_category_by_blocks <- function(df, labels_block, counts_block) {
  
  # Ensure the block sizes match the number of rows
  if (sum(counts_block) != nrow(df)) {
    stop(sprintf(
      "Row count mismatch: nrow(df)=%d but sum(counts_block)=%d",
      nrow(df), sum(counts_block)
    ), call. = FALSE)
  }
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  df$Category <- factor(rep(labels_block, counts_block))
  df
}

# ---- main (one dataset) ----
run_rf_one_caret <- function(df,
                             labels_block, counts_block,
                             train_human,         # e.g., c("Uno","Izumi") or c("Uno_Pre","Uno_Post")
                             class_order,         # e.g., c("Uno","Izumi") or c("Uno_Pre","Uno_Post")
                             positive_class,      # e.g., "Uno" or "Uno_Pre"
                             true_map_fun,        # Category -> True (2-class)
                             model_tag_fun,       # Category -> "GPT-4o"/"GPT-5"/"Gemini"/NA
                             use_rownames_for_tag = FALSE,
                             seed = 123) {
  
  df <- assign_category_by_blocks(df, labels_block, counts_block)
  
  #Train only on human-labeled subset; test on everything else.
  train_data <- subset(df, Category %in% train_human)
  test_data  <- subset(df, !(Category %in% train_human))
  train_data$Category <- droplevels(train_data$Category)
  test_data$Category  <- droplevels(test_data$Category)
  
  # enforce factor order
  train_data$Category <- factor(train_data$Category, levels = class_order)
  
  # Fit Random Forest
  set.seed(seed)
  rf_model <- randomForest::randomForest(Category ~ ., data = train_data, importance = TRUE)
  
  # ---- Train metrics (caret, fixed positive) ----
  train_pred <- predict(rf_model, newdata = train_data)
  train_pred <- factor(train_pred, levels = class_order)
  
  cm_train <- caret::confusionMatrix(
    data = train_pred,
    reference = train_data$Category,
    positive = positive_class
  )
  
  # ---- Test predictions ----
  pred <- predict(rf_model, newdata = test_data[, setdiff(colnames(test_data), "Category")])
  pred <- factor(pred, levels = class_order)
  
  true_base <- true_map_fun(as.character(test_data$Category))
  true_base <- factor(true_base, levels = class_order)
  
  cm_all <- caret::confusionMatrix(
    data = pred,
    reference = true_base,
    positive = positive_class
  )
  
  # overall metrics
  f1_all   <- unname(cm_all$byClass["F1"])
  acc_all  <- unname(cm_all$overall["Accuracy"])
  prec_all <- unname(cm_all$byClass["Pos Pred Value"])
  rec_all  <- unname(cm_all$byClass["Sensitivity"])
  
  # per-model subsets
  tag <- if (use_rownames_for_tag) rownames(test_data) else as.character(test_data$Category)
  get_mask <- function(pattern) grepl(pattern, tag)
  
  cm_sub <- function(mask) {
    if (!any(mask)) return(NULL)
    caret::confusionMatrix(
      data = factor(pred[mask], levels = class_order),
      reference = factor(true_base[mask], levels = class_order),
      positive = positive_class
    )
  }
  
  # Subset CMs by model name
  cm_gpt4o  <- if (use_rownames_for_tag) cm_sub(get_mask("GPT-4o")) else cm_sub(model_tag_fun(as.character(test_data$Category)) == "GPT-4o")
  cm_gpt5   <- if (use_rownames_for_tag) cm_sub(get_mask("GPT-5"))  else cm_sub(model_tag_fun(as.character(test_data$Category)) == "GPT-5")
  cm_gemini <- if (use_rownames_for_tag) cm_sub(get_mask("Gemini")) else cm_sub(model_tag_fun(as.character(test_data$Category)) == "Gemini")
  
  f1_of <- function(cm_obj) if (is.null(cm_obj)) NA_real_ else unname(cm_obj$byClass["F1"])
  
  list(
    metrics = c(
      # Train metrics (on training set only)
      Train_Accuracy = unname(cm_train$overall["Accuracy"]),
      Train_F1       = unname(cm_train$byClass["F1"]),
      # Test metrics (overall)
      Accuracy       = acc_all,
      Precision      = prec_all,
      Recall         = rec_all,
      F1             = f1_all,
      # Test metrics (per-model subsets)
      F1_GPT4o       = f1_of(cm_gpt4o),
      F1_GPT5        = f1_of(cm_gpt5),
      F1_Gemini      = f1_of(cm_gemini)
    ),
    cm_train   = cm_train$table,
    cm_overall = cm_all$table,
    cm_gpt4o   = if (is.null(cm_gpt4o)) NULL else cm_gpt4o$table,
    cm_gpt5    = if (is.null(cm_gpt5)) NULL else cm_gpt5$table,
    cm_gemini  = if (is.null(cm_gemini)) NULL else cm_gemini$table
  )
}

# ---- export to Excel ----
write_results_xlsx <- function(results, metrics_table, out_path) {
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "metrics")
  openxlsx::writeData(wb, "metrics", metrics_table)
  
  write_cm_block <- function(wb, sheet, cm, title, start_row) {
    if (is.null(cm)) return(start_row)
    df <- as.data.frame.matrix(cm)
    df <- cbind(Predicted = rownames(df), df)
    rownames(df) <- NULL
    openxlsx::writeData(wb, sheet, title, startRow = start_row, startCol = 1)
    openxlsx::writeData(wb, sheet, "True",  startRow = start_row + 1, startCol = 2)
    openxlsx::writeData(wb, sheet, df,      startRow = start_row + 2, startCol = 1)
    start_row + 2 + nrow(df) + 2
  }
  
  for (nm in names(results)) {
    res <- results[[nm]]
    sheet_name <- substr(nm, 1, 31)
    openxlsx::addWorksheet(wb, sheet_name)
    r <- 1
    r <- write_cm_block(wb, sheet_name, res$cm_train,   "[Train confusion matrix]", r)
    r <- write_cm_block(wb, sheet_name, res$cm_overall, "[Overall confusion matrix]", r)
    r <- write_cm_block(wb, sheet_name, res$cm_gpt4o,   "[Confusion matrix: GPT-4o]", r)
    r <- write_cm_block(wb, sheet_name, res$cm_gpt5,    "[Confusion matrix: GPT-5]", r)
    r <- write_cm_block(wb, sheet_name, res$cm_gemini,  "[Confusion matrix: Gemini]", r)
  }
  
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
}
