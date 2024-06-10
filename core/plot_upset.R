plot_upset <- function(compare_result_dir,
                       max_intersections = 20,
                       set_size_position = "right",
                       intersection_matrix_height_ratio = 0.2,
                       set_size_width_ratio = 0.3,
                       sort_intersections_by = "degree",
                       sort_intersections = "ascending",
                       font_size = 14,
                       annotate_font_size = 4) {
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
  
  # Calculate which compounds are in each treatment
  upset_df <- df[, 1:6]
  for (one_group in unique(groups$Group)) {
    compound_id <-
      df[, which(colnames(df) %in% groups$Sample[which(groups$Group == one_group)])] %>% rowSums(.) %>% replace(., . > 0, 1)
    upset_df <- upset_df %>% mutate(new_column = compound_id)
    colnames(upset_df)[dim(upset_df)[2]] <- one_group
  }
  upset_plot_df <-
    upset_df %>% pivot_longer(cols = -1:-6,
                              names_to = "group",
                              values_to = "exist") %>% filter(exist > 0)
  
  # Check whether the number of groups is less than 3
  if (ncol(upset_df[, -1:-6]) < 3) {
    stop("Cannot run the function with less than 3 groups!!!")
  }
  
  upset_plot <- ComplexUpset::upset(
    upset_df[, -1:-6],
    colnames(upset_df)[-1:-6],
    n_intersections = max_intersections,
    height_ratio = intersection_matrix_height_ratio,
    width_ratio = set_size_width_ratio,
    sort_intersections_by = sort_intersections_by,
    sort_intersections = sort_intersections,
    stripes = c('grey90', 'white'),
    base_annotations = list(
      'Intersection size' = ComplexUpset::intersection_size(
        text_colors = c(on_background = 'black', on_bar = 'white'),
        bar_number_threshold = 0.9,
        text_mapping = aes(family = "serif")
      )
      + annotate(
        geom = 'text',
        x = Inf,
        y = Inf,
        label = paste('Total:', nrow(upset_df)),
        vjust = 1,
        hjust = 1,
        family = "serif",
        size = annotate_font_size
      )
      + ylab('Numbers')
      + theme(axis.ticks.y = element_line())
    ),
    set_sizes = (
      ComplexUpset::upset_set_size(position = set_size_position)
      + ylab('Numbers')
      + theme(axis.ticks.x = element_line())
    ),
    themes = ComplexUpset::upset_modify_themes(
      list(
        'intersections_matrix' = theme(text = element_text(family = 'serif', size = font_size)),
        'overall_sizes' = theme(
          text = element_text(family = 'serif', size = font_size),
          axis.text.x = element_text(angle = 90)
        ),
        'Intersection size' = theme(
          text = element_text(family = 'serif', size = font_size),
          legend.position = "none"
        )
      )
    )
  )
  
  return(upset_plot)
}