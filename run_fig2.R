# =========================
# File: run_fig2.R
# Figure 2. Stylometric MDS of human (Uno, Izumi) and LLM-generated texts.
# Expected input: data_raw/Uno+Izumi+LLMs.xlsx
# Outputs: outputs/figures/Fig2.pdf and outputs/figures/Fig2.eps
# =========================

source("mds_common.R")

ensure_packages(c("ggplot2", "patchwork", "readxl", "showtext", "sysfonts", "ragg"))

BASE_FAMILY <- get_base_family()
enable_showtext(TRUE)

cfg <- list(
  xlsx_path = file.path("..","data_raw", "Uno+Izumi+LLMs.xlsx"),
  sheets = list(char = "char", pos = "pos", phrase = "phrase", liwc = "liwc"),
  
  # Row-block order in Excel
  labels_block = c("Uno", "Gemini_Uno", "GPT-4o_Uno", "GPT-5_Uno",
                   "Izumi", "Gemini_Izumi", "GPT-4o_Izumi", "GPT-5_Izumi"),
  counts_block = c(52, 52, 52, 52, 25, 25, 25, 25),
  
  # Legend display order
  legend_levels = c("Izumi", "Gemini_Izumi", "GPT-4o_Izumi", "GPT-5_Izumi",
                    "Uno", "Gemini_Uno", "GPT-4o_Uno", "GPT-5_Uno"),
  
  pal_map = c(
    "Izumi"="#009E73", "Gemini_Izumi"="#7570B3", "GPT-4o_Izumi"="#8c564b", "GPT-5_Izumi"="#0072B2",
    "Uno"="#D55E00",   "Gemini_Uno"="#59A14F",   "GPT-4o_Uno"="#E69F00",   "GPT-5_Uno"="#CC79A7"
  ),
  shape_map = c(
    "Izumi"=15, "Gemini_Izumi"=3, "GPT-4o_Izumi"=4, "GPT-5_Izumi"=8,
    "Uno"=16,   "Gemini_Uno"=5,   "GPT-4o_Uno"=1,   "GPT-5_Uno"=10
  ),
  
  out_dir = file.path("..","outputs", "figures"),
  out_basename = "Fig2",
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

