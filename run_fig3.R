# =========================
# File: run_fig3.R  (FINAL)
# Figure 3. Stylometric MDS of Uno’s pre-illness and post-illness texts and their LLM-generated imitations.
# Expected input: data_raw/Uno_Pre+Uno_Post+LLMs.xlsx
# Outputs: outputs/figures/Fig3.pdf and outputs/figures/Fig3.eps
# =========================

source("mds_common.R")

ensure_packages(c("ggplot2", "patchwork", "readxl", "showtext", "sysfonts", "ragg"))

BASE_FAMILY <- get_base_family()
enable_showtext(TRUE)

cfg <- list(
  xlsx_path = file.path("..", "data_raw", "Uno_Pre+Uno_Post+LLMs.xlsx"),
  sheets = list(char = "char", pos = "pos", phrase = "phrase", liwc = "liwc"),
  
  # Row-block order in Excel
  labels_block = c(
    "Uno_Post", "Uno_Pre",
    "Gemini_Uno_Post", "Gemini_Uno_Pre",
    "GPT-4o_Uno_Post", "GPT-4o_Uno_Pre",
    "GPT-5_Uno_Post",  "GPT-5_Uno_Pre"
  ),
  counts_block = c(25, 27, 25, 27, 25, 27, 25, 27),
  
  # Legend display order
  legend_levels = c(
    "Uno_Pre", "Gemini_Uno_Pre", "GPT-4o_Uno_Pre", "GPT-5_Uno_Pre",
    "Uno_Post","Gemini_Uno_Post","GPT-4o_Uno_Post","GPT-5_Uno_Post"
  ),
  
  pal_map = c(
    "Uno_Pre"="#009E73", "Gemini_Uno_Pre"="#7570B3", "GPT-4o_Uno_Pre"="#8c564b", "GPT-5_Uno_Pre"="#4E79A7",
    "Uno_Post"="#D55E00","Gemini_Uno_Post"="#59A14F","GPT-4o_Uno_Post"="#E69F00","GPT-5_Uno_Post"="#CC79A7"
  ),
  shape_map = c(
    "Uno_Pre"=15, "Gemini_Uno_Pre"=3, "GPT-4o_Uno_Pre"=4, "GPT-5_Uno_Pre"=8,
    "Uno_Post"=16,"Gemini_Uno_Post"=5,"GPT-4o_Uno_Post"=1,"GPT-5_Uno_Post"=10
  ),
  
  out_dir = file.path("..","outputs", "figures"),
  out_basename = "Fig3",
  out_w_mm = 183,
  out_h_mm = 183,
  legend_nrow = 2
)

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

datasets <- load_four_sheets(cfg$xlsx_path, cfg$sheets)

p <- make_mds_figure_2x2(
  datasets = datasets,
  labels_block = cfg$labels_block,
  counts_block = cfg$counts_block,
  legend_levels = cfg$legend_levels,
  pal_map = cfg$pal_map,
  shape_map = cfg$shape_map,
  legend_nrow = cfg$legend_nrow,
  base_family = BASE_FAMILY
)

print(p)

export_eps(
  p,
  file.path(cfg$out_dir, paste0(cfg$out_basename, ".eps")),
  cfg$out_w_mm, cfg$out_h_mm
)

export_pdf(
  p,
  file.path(cfg$out_dir, paste0(cfg$out_basename, ".pdf")),
  cfg$out_w_mm, cfg$out_h_mm
)

