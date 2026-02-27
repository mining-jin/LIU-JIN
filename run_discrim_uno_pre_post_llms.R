# =========================
# File: run_discrim_uno_pre_post_llms.R
# Purpose: RF discrimination analysis (Uno_Pre vs. Uno_Post) 
#          comparing Humans and LLMs across 4 feature sets.
# =========================

source("rf_discrim_common.R")

ensure_packages(c("randomForest", "caret", "readxl", "openxlsx"))

cfg <- list(
  xlsx_path = file.path("..","data_raw", "Uno_Pre+Uno_Post+LLMs.xlsx"),
  sheets = list(char = "char", pos = "pos", phrase = "phrase", liwc = "liwc"),
  out_dir = file.path("..","outputs", "tables"),
  out_file = "Uno_Pre_Uno_Post_RF_results.xlsx",
  seed = 123,
  
  # MUST match Excel row-block order:
  labels_block = c(
    "Uno_Post", "Uno_Pre",
    "Gemini_Uno_Post", "Gemini_Uno_Pre",
    "GPT-4o_Uno_Post", "GPT-4o_Uno_Pre",
    "GPT-5_Uno_Post",  "GPT-5_Uno_Pre"
  ),
  counts_block = c(25, 27, 25, 27, 25, 27, 25, 27),
  
  train_human = c("Uno_Pre", "Uno_Post"),
  class_order = c("Uno_Pre", "Uno_Post"),
  positive_class = "Uno_Pre"
)

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

datasets <- load_four_sheets(cfg$xlsx_path, cfg$sheets)
datasets_named <- list(
  "Character and Symbol Bigrams" = datasets$char,
  "POS Bigrams"                  = datasets$pos,
  "Phrase Patterns"              = datasets$phrase,
  "LIWC"                         = datasets$liwc
)

# Map Category -> binary truth (Uno_Pre/Uno_Post)
true_map_fun <- function(cat) sub("^(Gemini|GPT-4o|GPT-5)_", "", cat)

# Map Category -> model tag (GPT-4o/GPT-5/Gemini/NA)
model_tag_fun <- function(cat) {
  ifelse(grepl("^GPT-4o_", cat), "GPT-4o",
         ifelse(grepl("^GPT-5_", cat), "GPT-5",
                ifelse(grepl("^Gemini_", cat), "Gemini", NA_character_)))
}

results <- list()
for (nm in names(datasets_named)) {
  cat("\n==== Dataset:", nm, "====\n")
  results[[nm]] <- run_rf_one_caret(
    df = datasets_named[[nm]],
    labels_block = cfg$labels_block,
    counts_block = cfg$counts_block,
    train_human = cfg$train_human,
    class_order = cfg$class_order,
    positive_class = cfg$positive_class,
    true_map_fun = true_map_fun,
    model_tag_fun = model_tag_fun,
    use_rownames_for_tag = FALSE, 
    seed = cfg$seed
  )
}

metrics_mat <- do.call(rbind, lapply(results, `[[`, "metrics"))
metrics_table <- data.frame(
  Feature = rownames(metrics_mat),
  metrics_mat,
  row.names = NULL,
  check.names = FALSE
)

print(metrics_table)

out_path <- file.path(cfg$out_dir, cfg$out_file)
write_results_xlsx(results, metrics_table, out_path)
cat("\nSaved: ", out_path, "\n", sep = "")
