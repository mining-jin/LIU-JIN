# =========================
# File: run_fig1.R  
# Figure 1. Stylometric MDS of Uno’s pre- and post-illness texts and Izumi’s texts.
# Expected input: data_raw/Uno+Izumi.xlsx
# Outputs: outputs/figures/Fig1.pdf and outputs/figures/Fig1.eps
# =========================

source("mds_common.R")

ensure_packages(c("ggplot2", "patchwork", "readxl", "showtext", "sysfonts", "ragg"))

library(showtext)
font_add(family = "Arial", regular = "arial.ttf") 
BASE_FAMILY <- "Arial" 
showtext_auto()


cfg <- list(
  xlsx_path = file.path("..","data_raw", "Uno+Izumi.xlsx"),
  sheets = list(char = "char", pos = "pos", phrase = "phrase", liwc = "liwc"),
  
  # Row-block order in Excel
  labels_block  = c("Uno_Post", "Uno_Pre", "Izumi"),
  counts_block  = c(25, 27, 25),
  
  # Legend display order
  legend_levels = c("Izumi", "Uno_Pre", "Uno_Post"),
  
  pal_map   = c("Izumi"="#009E73", "Uno_Pre"="#0072B2", "Uno_Post"="#D55E00"),
  shape_map = c("Izumi"=15, "Uno_Pre"=16, "Uno_Post"=17),
  
  out_dir = file.path("..","outputs", "figures"),
  out_basename = "Fig1",
  out_w_mm = 183,
  out_h_mm = 183,
  legend_nrow = 1
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

