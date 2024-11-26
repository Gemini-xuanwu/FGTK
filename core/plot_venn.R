plot_venn <- function(compare_result_dir,
                      group_label_font_size = 2,
                      number_label_font_size = 2) {
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
        "The necessary files(sample_info.xlsx and peak.xlsx) do not exist, ",
        "please check if the path is correct or re-run the comparison programme!"
      )
    }
  }
  
  # Read the necessary files
  groups <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "sample_info.xlsx"))
  df <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "peak.xlsx"))
  
  # Calculate which compounds are in each treatment
  venn_list <- list()
  for (one_group in unique(groups$Group)) {
    compound_id <-
      df[, which(colnames(df) %in% groups$Sample[which(groups$Group == one_group)])] %>% rowSums(.)
    compound_id_filter <- df$Compound[which(compound_id > 0)]
    venn_list <- append(venn_list, list(compound_id_filter))
  }
  names(venn_list) <- unique(groups$Group)
  
  # Check whether the number of groups is more than 5
  if (length(venn_list) > 5) {
    stop("Cannot run the function with more than 5 groups!!!")
  }
  
  # plot venn
  if (length(venn_list) >= 3) {
    cs <- RColorBrewer::brewer.pal(length(venn_list), "Set1")
    venn.plot <-
      VennDiagram::venn.diagram(
        venn_list,
        col = cs,
        cat.col = cs,
        fill = cs,
        filename = NULL,
        euler.d = F,
        scaled = F,
        imagetype = "svg",
        fontfamily = "serif",
        cex = number_label_font_size,
        cat.fontfamily = "serif",
        cat.cex = group_label_font_size,
        disable.logging = TRUE
      )
  } else{
    cs <- c("#E41A1C", "#377EB8")
    venn.plot <- VennDiagram::venn.diagram(
      venn_list,
      col = cs,
      cat.col = cs,
      fill = cs,
      filename = NULL,
      euler.d = F,
      scaled = F,
      imagetype = "svg",
      fontfamily = "serif",
      cex = number_label_font_size,
      cat.fontfamily = "serif",
      cat.cex = group_label_font_size,
      cat.pos = c(90, -90),
      disable.logging = TRUE
    )
  }
  venn_plot <- ggplotify::as.ggplot(cowplot::plot_grid(grid::grobTree(venn.plot)))
  
  return(venn_plot)
}