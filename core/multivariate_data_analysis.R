multivariate_data_analysis <- function(compare_result_dir,
                                       calculate_percentage = TRUE,
                                       font_size = 16,
                                       width = 15,
                                       height = 12) {
  # create a list of returned result
  return_result <- list()
  
  # pre-processing ----
  
  # Import the necessary libraries
  if (!("package:tidyverse" %in% search())) {
    library(tidyverse)
  }
  
  # Import the necessary libraries
  if (!("package:ggplot2" %in% search())) {
    library(ggplot2)
  }
  
  # Check if the input directory exists
  if (!dir.exists(compare_result_dir)) {
    return_result$type <- "error"
    return_result$text_message <-
      "The compare result directory parameter does not set. Please provide the necessary parameter."
    return(return_result)
  } else {
    if (!file.exists(paste0(compare_result_dir, "/", "sample_info.xlsx")) |
        !file.exists(paste0(compare_result_dir, "/", "peak.xlsx"))) {
      return_result$type <- "error"
      return_result$text_message <-
        "The necessary files(sample_info.xlsx and peak.xlsx) do not exist, please check if the path is correct or re-run the comparison programme!"
      return(return_result)
    }
  }
  
  # Read the necessary files
  groups <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "sample_info.xlsx"))
  df <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "peak.xlsx"))
  row.names(df) <- df$Compound
  df <- df[, -(1:6)] %>%
    t(.)
  
  # calculate percentage
  if (calculate_percentage == TRUE) {
    df <- t(apply(df, 1, function(row)
      row / sum(row) * 100))
  }
  
  # PCA ----
  
  PCA <-
    ropls::opls(
      df,
      crossvalI = nrow(df) - 1,
      scaleC = "standard",
      fig.pdfC = "none",
      info.txtC = "none"
    )
  # Check PCA result
  if (PCA@summaryDF$pre < 2) {
    return_result$type <- "error"
    return_result$text_message <-
      "This data could not be analysed by PCA!!!"
    return(return_result)
  }
  df_PCA <- as.data.frame(PCA@scoreMN) %>%
    mutate(Sample = row.names(.)) %>%
    left_join(groups, by = c("Sample" = "Sample"))
  # Draw figure
  PCA_plot <- ggplot(df_PCA, aes(x = p1, y = p2, color = Group)) +
    labs(
      x = paste("PC 1 (", round(PCA@modelDF[["R2X"]][1] * 100, 2), "%)", sep = ""),
      y = paste("PC 2 (", round(PCA@modelDF[["R2X"]][2] * 100, 2), "%)", sep = "")
    ) +
    geom_point(size = 3) +
    scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(df_PCA$Group)), "Set1")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(df_PCA$Group)), "Set1")) +
    geom_vline(aes(xintercept = 0), linetype = 2) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    ggprism::theme_prism(
      base_size = font_size,
      base_line_size = 0.3,
      palette = "floral",
      base_family = "serif",
      base_fontface = "plain",
      border = TRUE
    ) +
    theme(legend.key.height = unit(20, "pt"),
          legend.key.width = unit(20, "pt"))
  # Add confidence ellipse
  if (all(table(groups$Group) > 3)) {
    PCA_plot <-
      PCA_plot + stat_ellipse(
        aes(fill = Group),
        geom = "polygon",
        alpha = 0.4,
        level = 0.95,
        color = "gray"
      )
  } else {
    PCA_plot <-
      PCA_plot + ggalt::geom_encircle(
        aes(fill = Group),
        expand = 0,
        spread = 1,
        s_shape = 1,
        alpha = 0.4
      )
  }
  # Save figure
  ggsave(
    paste0(compare_result_dir, "/", "PCA.pdf"),
    width = width,
    height = height,
    units = "cm"
  )
  
  # PCoA ----
  
  df_bray <- vegan::vegdist(df, method = "bray", binary = F)
  PCoA <- cmdscale(df_bray, k = 3, eig = T)
  df_PCoA <- as.data.frame(PCoA$points)
  eig_percent <- round(PCoA$eig / sum(PCoA$eig) * 100, 2)
  colnames(df_PCoA) <- paste0("PCoA", 1:3)
  df_PCoA <- cbind(df_PCoA, groups)
  # Draw figure
  PCoA_plot <- ggplot(df_PCoA, aes(x = PCoA1, y = PCoA2, color = Group)) +
    labs(
      x = paste("PCoA 1 (", eig_percent[1], "%)", sep = ""),
      y = paste("PCoA 2 (", eig_percent[2], "%)", sep = "")
    ) +
    geom_point(size = 3) +
    scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(df_PCoA$Group)), "Set1")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(df_PCoA$Group)), "Set1")) +
    geom_vline(aes(xintercept = 0), linetype = 2) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    ggprism::theme_prism(
      base_size = font_size,
      base_line_size = 0.3,
      palette = "floral",
      base_family = "serif",
      base_fontface = "plain",
      border = TRUE
    ) +
    theme(legend.key.height = unit(20, "pt"),
          legend.key.width = unit(20, "pt"))
  # Add confidence ellipse
  if (all(table(groups$Group) > 3)) {
    PCoA_plot <-
      PCoA_plot + stat_ellipse(
        aes(fill = Group),
        geom = "polygon",
        alpha = 0.4,
        level = 0.95,
        color = "gray"
      )
  } else {
    PCoA_plot <-
      PCoA_plot + ggalt::geom_encircle(
        aes(fill = Group),
        expand = 0,
        spread = 1,
        s_shape = 1,
        alpha = 0.4
      )
  }
  # Save figure
  ggsave(
    paste0(compare_result_dir, "/", "PCoA.pdf"),
    width = width,
    height = height,
    units = "cm"
  )
  
  # NMDS ----
  
  NMDS <-
    vegan::metaMDS(
      df,
      try = 5,
      trymax = 5000,
      k = 3,
      distance = "bray"
    )
  df_NMDS <- as.data.frame(NMDS$points)
  df_NMDS$Sample <- groups$Sample
  df_NMDS$Group <- groups$Group
  names(df_NMDS)[1:2] <- c("NMDS1", "NMDS2")
  # Draw figure
  NMDS_plot <- ggplot(df_NMDS, aes(x = NMDS1, y = NMDS2, color = Group)) +
    labs(
      x = "NMDS 1",
      y = "NMDS 2",
      # title = "NMDS plot",
      subtitle = bquote("Non-metric fit, R" ^ 2 ~ "=" ~ .(format((1 - NMDS$stress ^
                                                                    2), digits = 3
      )) ~ "; Stress = " ~ .(format(
        NMDS$stress, digits = 3
      )))
    ) +
    geom_point(size = 3) +
    scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(df_NMDS$Group)), "Set1")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(df_NMDS$Group)), "Set1")) +
    geom_vline(aes(xintercept = 0), linetype = 2) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    ggprism::theme_prism(
      base_size = font_size,
      base_line_size = 0.3,
      palette = "floral",
      base_family = "serif",
      base_fontface = "plain",
      border = TRUE
    ) +
    theme(legend.key.height = unit(20, "pt"),
          legend.key.width = unit(20, "pt"))
  # Add confidence ellipse
  if (all(table(groups$Group) > 3)) {
    NMDS_plot <-
      NMDS_plot + stat_ellipse(
        aes(fill = Group),
        geom = "polygon",
        alpha = 0.4,
        level = 0.95,
        color = "gray"
      )
  } else {
    NMDS_plot <-
      NMDS_plot + ggalt::geom_encircle(
        aes(fill = Group),
        expand = 0,
        spread = 1,
        s_shape = 1,
        alpha = 0.4
      )
  }
  # Save figure
  ggsave(
    paste0(compare_result_dir, "/", "NMDS.pdf"),
    width = width,
    height = height,
    units = "cm"
  )
  
  # PERMANOVA ----
  
  adonis <- vegan::adonis2(df ~ groups$Group,
                           permutations = 999,
                           method = "bray")
  pairwise_adonis <-
    pairwiseAdonis::pairwise.adonis(
      x = df,
      factors = groups$Group,
      sim.function = "vegdist",
      sim.method = "bray",
      p.adjust.m = "fdr",
      reduce = NULL,
      perm = 999
    )
  # Save two dataframes to the same Excel file
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "adonis")
  openxlsx::writeData(wb, "adonis", adonis)
  
  openxlsx::addWorksheet(wb, "pairwise_adonis")
  openxlsx::writeData(wb, "pairwise_adonis", pairwise_adonis)
  
  openxlsx::saveWorkbook(wb,
                         paste0(compare_result_dir, "/", "PERMANOVA.xlsx"),
                         overwrite = TRUE)
  
  
  return_result$type <- "message"
  return_result$text_message <-
    "The multivariate data analysis function has been run!"
  return(return_result)
}