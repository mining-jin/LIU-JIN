# =========================
# Script: run_fig4.R
# Purpose: Generate Figure 4 (Random Forest and SHAP analysis)
# =========================

source("fig4_common.R")
ensure_packages(fig4_required_packages())

cfg <- list(
  xlsx_path = file.path("..","data_raw", "Uno+Izumi+LLMs.xlsx"),
  sheet = "liwc",
  out_dir = file.path("..","outputs", "figures"),
  out_basename = "Fig4",
  class_levels = c("Uno_Pre", "Uno_Post", "Izumi"),
  
  labels_block = c(
    "Uno_Post","Uno_Pre","Gemini_Uno_Post","Gemini_Uno_Pre",
    "GPT-4o_Uno_Post","GPT-4o_Uno_Pre","GPT-5_Uno_Post","GPT-5_Uno_Pre",
    "Izumi","Gemini_Izumi","GPT-4o_Izumi","GPT-5_Izumi"
  ),
  counts_block = c(25,27,25,27,25,27,25,27,25,25,25,25),
  
  pred_table_levels = c(
    "Gemini_Izumi","Gemini_Uno_Pre","Gemini_Uno_Post",
    "GPT-4o_Izumi","GPT-4o_Uno_Pre","GPT-4o_Uno_Post",
    "GPT-5_Izumi","GPT-5_Uno_Pre","GPT-5_Uno_Post"
  ),
  
  rf_seed = 123,
  shap_max_n = 400,
  ovr_ntree = 300,
  out_w_mm = 183,
  out_h_mm = 210
)

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

# 1) Load + label
data <- fig4_load_labeled(
  xlsx_path = cfg$xlsx_path,
  sheet = cfg$sheet,
  labels_block = cfg$labels_block,
  counts_block = cfg$counts_block
)

# 2) Split
spl <- fig4_split(data, cfg$class_levels)
train_data <- spl$train
test_data  <- spl$test

# 3) Train multiclass RF
rf_model <- fig4_rf_train(train_data, seed = cfg$rf_seed)

# 4) Predict table (LLM categories)
cm <- fig4_rf_table(rf_model, test_data, cfg$pred_table_levels)
print(cm)

# 5) OvR TreeSHAP
shap_long <- fig4_shap_long(
  train_data = train_data,
  class_levels = cfg$class_levels,
  shap_max_n = cfg$shap_max_n,
  ovr_ntree = cfg$ovr_ntree
)

# 6) Plots
p_bee   <- fig4_plot_topN(shap_long, cfg$class_levels, top_n = 20, type = "bee")
p_lolli <- fig4_plot_topN(shap_long, cfg$class_levels, top_n = 10, type = "lolli")
p_all   <- fig4_stack(p_bee, p_lolli, heights = c(2.8, 1.2))
print(p_all)

# 7) Export
out_eps <- file.path(cfg$out_dir, paste0(cfg$out_basename, ".eps"))
out_pdf <- file.path(cfg$out_dir, paste0(cfg$out_basename, ".pdf"))

save_eps(p_all, out_eps, cfg$out_w_mm, cfg$out_h_mm, family = "Helvetica")
save_pdf(p_all, out_pdf, cfg$out_w_mm, cfg$out_h_mm)
