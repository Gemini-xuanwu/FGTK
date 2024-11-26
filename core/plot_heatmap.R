plot_heatmap <- function(compare_result_dir,
                         cell_width = 30,
                         cell_height = 30,
                         cluster_rows = T,
                         cluster_columns = T,
                         show_row_tree = T,
                         show_column_tree = T,
                         row_tree_side = "left",
                         column_tree_side = "top",
                         row_names_side = "right",
                         column_names_side = "bottom",
                         font_size = 12,
                         legend_font_size = 12) {
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
  
  rownames(df)<-df$Hitname
  new_df <- df[-1:-6] %>%
    apply(1, function(x)
      (x - mean(x)) / sd(x)) %>%
    t()
  out_df <- cbind(df[1:6], new_df)
  cs <- circlize::colorRamp2(c(min(new_df), 0, max(new_df)), c("cornflowerblue", "white", "firebrick1"))
  
  # Plot heatmap
  heatmap.plot <- ComplexHeatmap::Heatmap(
    new_df,
    # whether to cluster and draw tree
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    show_row_dend = show_row_tree,
    show_column_dend = show_column_tree,
    row_dend_side = row_tree_side,
    column_dend_side = column_tree_side,
    # set cell's width and height
    width = ncol(new_df) * unit(cell_width, "pt"),
    height = nrow(new_df) * unit(cell_height, "pt"),
    row_names_side = row_names_side,
    column_names_side = column_names_side,
    col = cs,
    row_names_gp = grid::gpar(fontsize = font_size, fontfamily = "serif"),
    column_names_gp = grid::gpar(fontsize = font_size, fontfamily = "serif"),
    rect_gp = grid::gpar(col = "white", lwd = 2),
    border = F,
    top_annotation = ComplexHeatmap::HeatmapAnnotation(
      group = groups$Group,
      col = list(group = setNames(
        RColorBrewer::brewer.pal(length(unique(groups$Group)), "Set1"),
        unique(groups$Group)
      )),
      show_annotation_name = F,
      simple_anno_size = unit(cell_height/4,'pt'),
      annotation_legend_param = list(
        title = "Group",
        title_gp = grid::gpar(fontsize = legend_font_size, fontfamily = "serif"),
        title_position = "topleft",
        tick_length = unit(0.15, "snpc"),
        labels_gp = grid::gpar(fontsize = legend_font_size-2, fontfamily = "serif"),
        legend_height = unit(0.15, "snpc"),
        direction = "vertical"
      )
    ),
    heatmap_legend_param = list(
        title = "Z-score",
      title_gp = grid::gpar(fontsize = legend_font_size, fontfamily = "serif"),
      title_position = "topleft",
      tick_length = unit(1, "mm"),
      border = "black",
      tick_length = unit(0.15, "snpc"),
      labels_gp = grid::gpar(fontsize = legend_font_size-2, fontfamily = "serif"),
      legend_height = unit(0.15, "snpc"),
      direction = "vertical"
    )
  ) %>% ComplexHeatmap::draw(heatmap_legend_side = "right")
  
  result <- list(plot = heatmap.plot, df = out_df)
  return(result)
}