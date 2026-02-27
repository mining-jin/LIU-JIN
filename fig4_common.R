# =========================
# R/fig4_common.R  (UPDATED: macro-average ranking for y-axis order)
# =========================

fig4_required_packages <- function() c(
  "randomForest","ggplot2","dplyr","tidyr","treeshap","ggbeeswarm","patchwork","readxl","scales"
)

ensure_packages <- function(pkgs){
  ins <- setdiff(pkgs, rownames(installed.packages()))
  if(length(ins)) install.packages(ins, repos="https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only=TRUE))
}

excel_to_df <- function(path, sheet) readxl::read_excel(path, sheet=sheet) |> as.data.frame()

label_cleaner <- function(x){
  s <- as.character(x); s[is.na(s)] <- ""
  s <- gsub("^X","",s); s <- gsub("\\."," ",s)
  s <- gsub("(?i)[a-z]+(?=\\d)","",s, perl=TRUE)
  s <- gsub("\\s+"," ",s); trimws(s)
}

save_eps <- function(fig, path, w_mm, h_mm, bg="white", family="Helvetica"){
  grDevices::cairo_ps(path, w_mm/25.4, h_mm/25.4, onefile=FALSE,
                      fallback_resolution=600, family=family, bg=bg)
  print(fig); grDevices::dev.off()
}
save_pdf <- function(fig, path, w_mm, h_mm, bg="white"){
  ggplot2::ggsave(path, fig, device=grDevices::cairo_pdf, width=w_mm, height=h_mm,
                  units="mm", bg=bg, dpi=500)
}

# Load + label data by row-block order
fig4_load_labeled <- function(xlsx_path, sheet, labels_block, counts_block, drop_cols=c("OTHERS")){
  if(!file.exists(xlsx_path)) stop(paste0("File not found: ", xlsx_path), call.=FALSE)
  d <- excel_to_df(xlsx_path, sheet)
  for(cc in drop_cols) if(cc %in% names(d)) d <- d[, setdiff(names(d), cc), drop=FALSE]
  stopifnot(sum(counts_block) == nrow(d))
  colnames(d) <- make.names(names(d), unique=TRUE)
  d$Category <- factor(rep(labels_block, counts_block))
  d
}

# Train/Test split
fig4_split <- function(d, class_levels){
  tr <- subset(d, Category %in% class_levels); tr$Category <- droplevels(tr$Category)
  te <- subset(d, !(Category %in% class_levels)); te$Category <- droplevels(te$Category)
  list(train=tr, test=te)
}

# Multiclass RF + confusion table
fig4_rf_train <- function(train_data, seed=123){
  set.seed(seed)
  f <- reformulate(setdiff(names(train_data), "Category"), "Category")
  randomForest::randomForest(f, data=train_data, importance=TRUE)
}
fig4_rf_table <- function(rf_model, test_data, pred_levels){
  pred <- predict(rf_model, newdata=test_data[, setdiff(names(test_data),"Category")])
  pred <- factor(pred, levels = levels(rf_model$y))
  true <- factor(test_data$Category, levels=pred_levels)
  table(True=true, Predicted=pred)
  }

# OvR TreeSHAP -> long format (with z)
fig4_shap_long <- function(train_data, class_levels, shap_max_n=400, ovr_ntree=300, seed_sample=42){
  Sys.setenv(OMP_NUM_THREADS="1", OMP_THREAD_LIMIT="1")
  
  X <- dplyr::select(train_data, -Category) |>
    dplyr::mutate(
      dplyr::across(where(is.character), ~ suppressWarnings(as.numeric(.))),
      dplyr::across(where(is.factor), ~ as.integer(.))
    )
  X[!is.finite(as.matrix(X))] <- 0
  
  set.seed(seed_sample)
  Xs <- if(nrow(X) > shap_max_n) X[sample(nrow(X), shap_max_n), , drop=FALSE] else X
  
  xz <- scale(Xs) |> as.data.frame()
  xz$._row <- seq_len(nrow(xz))
  X_long <- xz |>
    tidyr::pivot_longer(-._row, names_to="Feature", values_to="z") |>
    dplyr::mutate(FeatureLabel = label_cleaner(Feature))
  
  one_class <- function(c1){
    set.seed(1000 + match(c1, class_levels))
    y <- factor(ifelse(train_data$Category == c1, 1, 0), levels=c(0,1))
    rf_bin <- randomForest::randomForest(x=X, y=y, ntree=ovr_ntree, importance=FALSE)
    uni <- treeshap::randomForest.unify(rf_bin, data=X)
    ts <- tryCatch(treeshap::treeshap(uni, x=Xs, interactions=FALSE),
                   error=function(e){ message("treeshap error @ ", c1, ": ", e$message); NULL })
    if(is.null(ts)) return(NULL)
    S <- as.data.frame(ts$shaps); S$._row <- seq_len(nrow(S))
    tidyr::pivot_longer(S, -._row, names_to="Feature", values_to="SHAP") |>
      dplyr::mutate(Class=c1, FeatureLabel=label_cleaner(Feature))
  }
  
  lst <- Filter(Negate(is.null), lapply(class_levels, one_class))
  if(!length(lst)) stop("All treeshap runs failed.", call.=FALSE)
  
  dplyr::bind_rows(lst) |>
    dplyr::left_join(X_long, by=c("._row","Feature","FeatureLabel")) |>
    dplyr::mutate(Class = factor(Class, levels=class_levels))
}

# One plot function: type = "bee" (TopN beeswarm) or "lolli" (TopN mean|SHAP|)
# UPDATED: y-axis ordering uses MACRO average across classes (each class equal weight).
fig4_plot_topN <- function(shap_long, class_levels, top_n=20, type=c("bee","lolli")){
  type <- match.arg(type)
  
  # ---- UPDATED RANKING (macro average over classes) ----
  # For each feature, compute mean(|SHAP|) within each class, then average across classes.
  top_rank <- shap_long |>
    dplyr::group_by(Class, Feature, FeatureLabel) |>
    dplyr::summarise(m = mean(abs(SHAP), na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(Feature, FeatureLabel) |>
    dplyr::summarise(overall = mean(m, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(overall)) |>
    dplyr::slice(1:top_n)
  
  feat_levels <- rev(top_rank$FeatureLabel)
  
  d_top <- shap_long |>
    dplyr::semi_join(top_rank, by=c("Feature","FeatureLabel")) |>
    dplyr::mutate(
      FeatureLabel = factor(FeatureLabel, levels=feat_levels),
      Class = factor(Class, levels=class_levels)
    )
  
  if(type == "bee"){
    Zmax <- suppressWarnings(stats::quantile(abs(d_top$z), 0.99, na.rm=TRUE))
    if(!is.finite(Zmax) || Zmax <= 0) Zmax <- 2.5
    x_max <- max(abs(d_top$SHAP), na.rm=TRUE)
    x_lim <- c(-x_max*1.05, x_max*1.18)
    
    return(
      ggplot2::ggplot(d_top, ggplot2::aes(SHAP, FeatureLabel, color=z)) +
        ggbeeswarm::geom_quasirandom(size=0.9, alpha=0.65, width=0.3) +
        ggplot2::geom_vline(xintercept=0, colour="#B8B8B8", linewidth=0.3, linetype="dashed") +
        ggplot2::facet_wrap(~Class, nrow=1) +
        ggplot2::scale_y_discrete(limits=feat_levels, drop=FALSE) +
        ggplot2::scale_x_continuous(limits=x_lim, breaks=scales::pretty_breaks(5),
                                    expand=ggplot2::expansion(mult=c(0,0.02))) +
        ggplot2::scale_color_gradient2(low="#2166AC", mid="grey90", high="#B2182B",
                                       midpoint=0, limits=c(-Zmax, Zmax),
                                       oob=scales::squish, na.value="grey80",
                                       name="Feature value\n(z-score)") +
        ggplot2::labs(
          x="SHAP value (Contribution to Prediction)",
          y="Feature (LIWC)",
          title="Class-specific SHAP distributions (OvR Random Forest, TreeSHAP)"
        ) +
        ggplot2::coord_cartesian(clip="off") +
        ggplot2::theme_minimal(base_size=10) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face="bold", hjust=0, size=11),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(colour="black", fill=NA, linewidth=0.3),
          strip.background = ggplot2::element_rect(fill="#B8B8B8", colour=NA),
          strip.text = ggplot2::element_text(face="bold"),
          axis.text.y = ggplot2::element_text(size=8, colour="black"),
          axis.text.x = ggplot2::element_text(size=8, colour="black", margin=ggplot2::margin(t=3)),
          legend.position="right"
        ) +
        ggplot2::guides(color = ggplot2::guide_colorbar(title.hjust=0.5))
    )
  }
  
  mean_abs <- d_top |>
    dplyr::group_by(Class, FeatureLabel) |>
    dplyr::summarise(mean_abs = mean(abs(SHAP), na.rm=TRUE), .groups="drop")
  
  x_max <- max(mean_abs$mean_abs, na.rm=TRUE)
  x_lim <- c(0, x_max*1.18)
  
  ggplot2::ggplot(mean_abs, ggplot2::aes(mean_abs, FeatureLabel)) +
    ggplot2::geom_segment(ggplot2::aes(xend=mean_abs, yend=FeatureLabel), x=0,
                          linewidth=0.3, colour="#B8B8B8") +
    ggplot2::geom_point(shape=21, fill="#C4423A", colour="white", size=2, stroke=0.9) +
    ggplot2::geom_text(ggplot2::aes(label=sprintf("%+.2f", mean_abs)), hjust=-0.2, size=2.5) +
    ggplot2::facet_wrap(~Class, nrow=1) +
    ggplot2::scale_y_discrete(limits=feat_levels, drop=FALSE) +
    ggplot2::scale_x_continuous(limits=x_lim, breaks=scales::pretty_breaks(),
                                expand=ggplot2::expansion(mult=c(0,0.02))) +
    ggplot2::coord_cartesian(clip="off") +
    ggplot2::labs(x="Mean |SHAP| (per class)", y="Feature (LIWC)",
                  title="Top features by mean |SHAP| (per class)") +
    ggplot2::theme_minimal(base_size=10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face="bold", hjust=0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour="black", fill=NA, linewidth=0.3),
      strip.background = ggplot2::element_rect(fill="#B8B8B8", colour=NA),
      strip.text = ggplot2::element_text(face="bold"),
      axis.text.y = ggplot2::element_text(size=8, colour="black"),
      axis.text.x = ggplot2::element_text(size=8, colour="black", margin=ggplot2::margin(t=3))
    )
}

fig4_stack <- function(p_bee, p_lolli, heights=c(2.8,1.2)){
  p_bee / p_lolli + patchwork::plot_layout(heights=heights)
}
