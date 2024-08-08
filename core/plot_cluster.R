plot_cluster <- function(compare_result_dir,
                         type = "sample",
                         corr_method = "pearson",
                         cell_width = 30,
                         cell_height = 30,
                         row_names_side = "right",
                         column_names_side = "left",
                         cluster_rows = T,
                         cluster_columns = T,
                         show_row_tree = T,
                         show_column_tree = T,
                         row_tree_side = "left",
                         column_tree_side = "top",
                         legend_side = "right",
                         legend_direction = "vertical",
                         legend_height = 4,
                         legend_title_position = "topleft",
                         font_size = 12,
                         legend_font_size = 12,
                         label_font_size = 10,
                         show_label = T) {
  # Import the necessary libraries
  if (!("package:tidyverse" %in% search())) {
    library(tidyverse)
  }
  if (!("package:ggplot2" %in% search())) {
    library(ggplot2)
  }
  
  # Check if the input directory exists
  if (!dir.exists(compare_result_dir)) {
    stop(
      "The compare result directory parameter does not set. Please provide the necessary parameter."
    )
  } else {
    if (!file.exists(paste0(compare_result_dir, "/", "sample_info.xlsx")) |
        !file.exists(paste0(compare_result_dir, "/", "peak.xlsx"))) {
      stop(
        "The necessary files(sample_info.xlsx and peak.xlsx) do not exist",
        "please check if the path is correct or re-run the comparison programme!"
      )
    }
  }
  
  # Read the necessary files
  groups <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "sample_info.xlsx"))
  df <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "peak.xlsx"))
  
  # Screening for actually detected compounds
  ensure_id <- df[, -1:-6] %>% rowSums()
  df <- df[which(ensure_id > 0), ]
  
  # Change of color range and data processing according to the type of drawing
  if (type == "compound") {
    rownames(df) <- df$Compound
    df_corr <- t(df[, -1:-6])
  } else {
    df_corr <- df[, -1:-6]
  }
  cs <- circlize::colorRamp2(c(-1, 0, 1), c("cornflowerblue", "white", "firebrick1"))
  
  # Calculate correlation
  corr_list <- psych::corr.test(df_corr, method = corr_method, adjust = "fdr")
  corr_df <- reshape2::melt(corr_list$r) %>%
    mutate(
      p = reshape2::melt(corr_list$p)[, 3],
      p_signif = symnum(
        p,
        corr = FALSE,
        na = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
        symbols = c("***", "**", "*", "", " ")
      )
    ) %>%
    magrittr::set_colnames(c("sample1", "sample2", "r", "p", "p_signif")) %>%
    mutate(label = paste0(round(r, 3), "\n", p_signif))
  if (show_label == T) {
    corr_df <- corr_df %>% mutate(label = paste0(round(r, 3), "\n", p_signif))
  } else {
    corr_df <- corr_df %>% mutate(label = "")
  }
  
  
  # Convert correlation coefficients and label matrices to wide format
  r_value <- corr_df %>%
    select(1, 2, 3) %>%
    pivot_wider(names_from = "sample2", values_from = r) %>%
    column_to_rownames(var = "sample1")
  label_value <- corr_df %>%
    select(1, 2, 6) %>%
    pivot_wider(names_from = "sample2", values_from = label) %>%
    column_to_rownames(var = "sample1")
  diag(label_value) <- ""
  
  # Plot cluster
  cluster.plot <- ComplexHeatmap::Heatmap(
    as.matrix(r_value),
    # set cell's width and height
    width = ncol(corr_list$r) * unit(cell_width, "pt"),
    height = ncol(corr_list$r) * unit(cell_height, "pt"),
    row_names_side = row_names_side,
    column_names_side = column_names_side,
    # whether to cluster and draw tree
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    show_row_dend = show_row_tree,
    show_column_dend = show_column_tree,
    row_dend_side = row_tree_side,
    column_dend_side = column_tree_side,
    col = cs,
    row_names_gp = grid::gpar(fontsize = font_size, fontfamily = "serif"),
    column_names_gp = grid::gpar(fontsize = font_size, fontfamily = "serif"),
    rect_gp = grid::gpar(col = "white", lwd = 2),
    border = F,
    heatmap_legend_param = list(
      title = "Correlation",
      title_gp = grid::gpar(fontsize = legend_font_size, fontfamily = "serif"),
      title_position = legend_title_position,
      tick_length = unit(1, "mm"),
      border = "black",
      labels_gp = grid::gpar(fontsize = legend_font_size - 2, fontfamily = "serif"),
      legend_height = unit(legend_height, "cm"),
      direction = legend_direction
    ),
    # show label in cell
    cell_fun = function(j, i, x, y, width, height, fill) {
      grid::grid.text(
        sprintf("%s", as.matrix(label_value)[i, j]),
        x,
        y,
        gp = grid::gpar(fontsize = label_font_size, fontfamily = "serif")
      )
    }
  )
  cluster_plot <- ComplexHeatmap::draw(cluster.plot, heatmap_legend_side = legend_side)
  
  result <- list(plot = cluster_plot, df = corr_df)
  return(result)
}