# =========================
# File: R/mds_common.R 
# =========================

#' Get appropriate font family based on the Operating System
#' @return A string representing the font family
get_base_family <- function() {
  os <- Sys.info()[["sysname"]]
  if (os == "Darwin") {
    return("sans")      # macOS default
  } else if (os == "Windows") {
    return("Arial")     # Windows standard
  } else {
    return("sans")      # Linux/Others
  }
}

#' Toggle showtext rendering for consistent font display across devices
#' @param active Logical, whether to enable showtext_auto
enable_showtext <- function(active = TRUE) {
  if (active) {
    showtext::showtext_auto()
  } else {
    showtext::showtext_auto(FALSE)
  }
}

# Install missing packages and load all required packages
ensure_packages <- function(pkgs) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
}

# Read one Excel sheet into a data.frame:
# - Use the first column as row names
# - Convert numeric-like character columns to numeric
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

# replace NA with 0
fix_numeric <- function(df) {
  df[is.na(df)] <- 0
  df
}

# Load the four feature sheets and return them as a named list (for a 2x2 figure)
load_four_sheets <- function(path_xlsx,
                             sheets = list(char = "char", pos = "pos", phrase = "phrase", liwc = "liwc")) {
  list(
    "Character and Symbol Bigrams" = fix_numeric(excel_to_df(path_xlsx, sheets$char)),
    "POS Bigrams"                  = fix_numeric(excel_to_df(path_xlsx, sheets$pos)),
    "Phrase Pattern"               = fix_numeric(excel_to_df(path_xlsx, sheets$phrase)),
    "LIWC"                         = fix_numeric(excel_to_df(path_xlsx, sheets$liwc))
  )
}

# Convert each row into a probability distribution
to_row_prob <- function(df) {
  rs <- rowSums(df)
  idx <- which(rs == 0)
  if (length(idx)) df[idx, ] <- 1 / ncol(df)
  rs <- rowSums(df)
  sweep(df, 1, rs, "/")
}

# Compute Jensen–Shannon distance matrix
jsd_matrix <- function(x) {
  x <- pmax(x, 0) + 1e-12
  n <- nrow(x)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) {
    y <- x[i, ]
    for (j in seq_len(n)) {
      z <- x[j, ]
      m <- 0.5 * (y + z)
      D[i, j] <- sqrt(0.5 * (sum(y * log2(y / m)) + sum(z * log2(z / m))))
    }
  }
  D
}

# For one dataset (one sheet):
# row-normalize -> JSD distance -> 2D MDS -> scatter plot
mds_plot_one <- function(df, title,
                         labels_block, counts_block,
                         legend_levels, pal_map, shape_map,
                         base_family = "sans") {
  # Assign categories by row-block order (must match Excel row order)
  cat_vec <- factor(rep(labels_block, counts_block), levels = legend_levels)
  
  X_prob <- to_row_prob(df)
  D <- jsd_matrix(as.matrix(X_prob))
  D <- (D + t(D)) / 2
  diag(D) <- 0
  
  # mds
  mds_fit <- cmdscale(as.dist(D), k = 2, eig = TRUE)
  scores <- data.frame(
    Dim.1 = mds_fit$points[, 1],
    Dim.2 = mds_fit$points[, 2],
    Category = cat_vec,
    row.names = rownames(df)
  )
  
  ggplot2::ggplot(scores, ggplot2::aes(Dim.1, Dim.2, color = Category, shape = Category)) +
    ggplot2::geom_point(size = 1.3) +
    ggplot2::scale_color_manual(values = pal_map, breaks = legend_levels, drop = FALSE) +
    ggplot2::scale_shape_manual(values = shape_map, breaks = legend_levels, drop = FALSE) +
    ggplot2::theme_bw(base_family = base_family, base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "plain", size = 11),
      axis.title = ggplot2::element_text(face = "plain", size = 11, color = "black"),
      legend.title = ggplot2::element_text(face = "plain", size = 11, color = "black"),
      legend.text = ggplot2::element_text(size = 10)
    ) + 
    ggplot2::labs(title = title, x = "Dimension 1", y = "Dimension 2",
                  color = "Category", shape = "Category")
}

# Combine 4 MDS panels into a 2x2 layout and collect legends at the bottom.
make_mds_figure_2x2 <- function(datasets,
                                labels_block, counts_block,
                                legend_levels, pal_map, shape_map,
                                legend_nrow = 1,
                                base_family = "sans") {
  plots <- lapply(names(datasets), function(nm) {
    mds_plot_one(
      df = datasets[[nm]],
      title = paste0("MDS : ", nm),
      labels_block = labels_block,
      counts_block = counts_block,
      legend_levels = legend_levels,
      pal_map = pal_map,
      shape_map = shape_map,
      base_family = base_family
    )
  })
  
  combined <-
    (plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]]) +
    patchwork::plot_layout(ncol = 2, guides = "collect")
  
  combined &
    ggplot2::theme(legend.position = "bottom") &
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = legend_nrow, byrow = TRUE, override.aes = list(size = 3)),
      shape = ggplot2::guide_legend(nrow = legend_nrow, byrow = TRUE)
    )
}

# Export as EPS
export_eps <- function(figure, path, width_mm, height_mm, bg = "white") {
  w_in <- width_mm / 25.4
  h_in <- height_mm / 25.4
  grDevices::cairo_ps(
    filename = path,
    width = w_in,
    height = h_in,
    onefile = FALSE,
    fallback_resolution = 600,
    bg = bg
  )
  print(figure)
  grDevices::dev.off()
}

# Export as PDF
export_pdf <- function(figure, path, width_mm, height_mm, bg = "white") {
  ggplot2::ggsave(
    filename = path,
    plot = figure,
    device = grDevices::cairo_pdf,
    width = width_mm,
    height = height_mm,
    units = "mm",
    bg = bg,
    dpi = 500
  )
}

