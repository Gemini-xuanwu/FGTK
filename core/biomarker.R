biomarker <- function(compare_result_dir,
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
  
  # Reorder according to retention time
  df <- df %>%
    arrange(RT)
  
  # calculate percentage
  if (calculate_percentage == TRUE) {
    df <-
      cbind(df[, (1:6)], apply(df[, -(1:6)], 2, function(row)
        row / sum(row) * 100))
  }
  rownames(df) <- df$Compound
  
  # Creating a biomarker folder ----
  grouped_biomarker_dir <-
    paste0(compare_result_dir, "/", "biomarker")
  if (!file.exists(grouped_biomarker_dir)) {
    dir.create(grouped_biomarker_dir)
  }
  
  ### Create grouped_biomarker inner function to get biomarker between two groups
  grouped_biomarker <-
    function(df,
             groups,
             grouped_biomarker_dir,
             group1,
             group2,
             font_size = font_size,
             width = width,
             height = height) {
      # Creating a new vs folder ----
      group_vs_biomarker_dir <-
        paste0(grouped_biomarker_dir, "/", group1, "_vs_", group2)
      if (!file.exists(group_vs_biomarker_dir)) {
        dir.create(group_vs_biomarker_dir)
      }
      
      # Extract the data and transform it into long data and filter the data belonging to the two groups.
      dfaux <- df %>%
        as_tibble() %>%
        pivot_longer(-(1:6), names_to = "Sample", values_to = "value") %>%
        left_join(groups, by = c("Sample" = "Sample")) %>%
        filter(Group %in% c(group1, group2))
      
      # Calculate FC and P ----
      
      ### Calculate FC group1/group2
      options(dplyr.summarise.inform = FALSE)
      dfFC <- dfaux %>%
        group_by(Compound, Group) %>%
        summarise(mean = mean(value, na.rm = T)) %>%
        pivot_wider(names_from = Group, values_from = mean)
      dfFC$FC <-
        as.vector(as.matrix(dfFC[, which(group1 == colnames(dfFC))] / dfFC[, which(group2 == colnames(dfFC))]))
      
      # Tests for conformity to normal distribution
      df_normal_condition <- dfaux %>%
        group_by(Compound) %>%
        filter(sum(value) > 0) %>%
        rstatix::shapiro_test(value) %>%
        mutate(Normal_Distribution = case_when(p > 0.05 ~ "YES", TRUE ~ "NO"))
      dfaux <- dfaux %>%
        left_join(df_normal_condition[, c("Compound", "Normal_Distribution")], by = join_by(Compound == Compound))
      
      ### Calculate P value
      # Compounds with data not normally distributed were subjected to wilcox-test
      if ("NO" %in% dfaux$Normal_Distribution) {
        df_wt_test <- dfaux %>%
          filter(Normal_Distribution == "NO") %>%
          group_by(Compound) %>%
          rstatix::wilcox_test(value ~ Group, p.adjust.method = "fdr")
      }
      # Compounds with normally distributed data were subjected to t-test
      t_test_status <- "no run"
      if ("YES" %in% dfaux$Normal_Distribution) {
        df_t_test <- dfaux %>%
          filter(Normal_Distribution == "YES") %>%
          group_by(Compound) %>%
          rstatix::t_test(value ~ Group,
                          var.equal = F,
                          p.adjust.method = "fdr")
        t_test_status <- "run"
      }
      if (t_test_status == "run")
      {
        df_test_all <- df_wt_test %>% full_join(df_t_test)
      }
      else{
        df_test_all <- df_wt_test
      }
      dfP <- df %>%
        left_join(df_test_all[, c("Compound", "p")])
      
      # Combine results
      dfaux_diff <- dfaux %>%
        select(-Group) %>%
        pivot_wider(names_from = Sample, values_from = value) %>%
        left_join(dfFC, by = join_by(Compound == Compound)) %>%
        left_join(dfP[, c("Compound", "p")], by = join_by(Compound == Compound))
      
      # set cutoff
      FC <- 1.5
      PValue <- 0.05
      # Determine the up- and down-regulation of each compound by adding sig and label columns to the data frame.
      dfaux_diff$sig <- NA
      dfaux_diff$sig[(-1 * log10(dfaux_diff$p) < -1 * log10(PValue) |
                        dfaux_diff$p == "NA") |
                       (log2(dfaux_diff$FC) < log2(FC)) &
                       log2(dfaux_diff$FC) > -log2(FC)] <-
        "NotSig"
      dfaux_diff$sig[-1 * log10(dfaux_diff$p) >= -1 * log10(PValue) &
                       log2(dfaux_diff$FC) >= log2(FC)] <- "Up"
      dfaux_diff$sig[-1 * log10(dfaux_diff$p) >= -1 * log10(PValue) &
                       log2(dfaux_diff$FC) <= -log2(FC)] <- "Down"
      dfaux_diff$label <- ifelse(dfaux_diff$p < PValue &
                                   abs(log2(dfaux_diff$FC)) >= log2(FC),
                                 dfaux_diff$Compound,
                                 "")
      
      # Volcano plot ----
      if (length(which(dfaux_diff$label != "")) != 0) {
        DiffPlot <- ggplot(dfaux_diff[which(!is.na(dfaux_diff$sig)), ], aes(log2(FC), -1 * log10(p))) +
          geom_point(aes(color = sig), alpha = 0.8, size = 3) +
          labs(# title = paste(group1, " vs ", group2, " - Volcano plot", sep = ""),
            x = bquote(log[2](FC)), y = bquote(-log[10](PValue))) +
          scale_color_manual(values = c(
            "Down" = "blue",
            "NotSig" = "grey",
            "Up" = "red"
          )) +
          geom_hline(yintercept = -log10(PValue),
                     linetype = 2) +
          geom_vline(xintercept = c(-log2(FC), log2(FC)),
                     linetype = 2) +
          # geom_text_repel(aes(x = log2(FC),
          #                     y = -1*log10(p),
          #                     label=label),
          #                 max.overlaps = 1000,
          #                 size=3,
          #                 #box.padding=unit(0.3,'lines'),
          #                 #point.padding=unit(0.2, 'lines'),
          #                 segment.color='black',
          #                 show.legend=FALSE)+
          ggprism::theme_prism(
            base_size = font_size,
            base_line_size = 0.3,
            palette = "floral",
            base_family = "serif",
            base_fontface = "plain",
            border = TRUE
          ) +
          theme(
            legend.text = element_text(size = font_size - 4, face = "plain"),
            legend.key.height = unit(0.05, "snpc"),
            legend.key.width = unit(0.05, "snpc")
          )
        ggsave(
          paste0(
            group_vs_biomarker_dir,
            "/",
            group1,
            "_vs_",
            group2,
            "_DiffPlot.pdf"
          ),
          width = width,
          height = height,
          units = "cm"
        )
      }
      
      dfDiff <- dfaux_diff
      dfPV <- dfaux_diff %>% filter(label != "")
      openxlsx::write.xlsx(
        dfDiff,
        paste0(
          group_vs_biomarker_dir,
          "/",
          group1,
          "_vs_",
          group2,
          "_Diff.xlsx"
        )
      )
      openxlsx::write.xlsx(dfPV,
                           paste0(
                             group_vs_biomarker_dir,
                             "/",
                             group1,
                             "_vs_",
                             group2,
                             "_P.xlsx"
                           ))
      
      # Create the required intermediate data
      dfaux1 <- df %>%
        as_tibble() %>%
        pivot_longer(-(1:6), names_to = "Sample", values_to = "value") %>%
        left_join(groups, by = c("Sample" = "Sample")) %>%
        filter(Group %in% c(group1, group2))
      dfaux2 <- dfaux1 %>%
        select(-Group) %>%
        pivot_wider(names_from = Sample, values_from = value) %>%
        as.data.frame(.)
      row.names(dfaux2) <- dfaux2$Compound
      dfaux2 <- dfaux2[, -(1:6)] %>% t(.)
      
      # Setting cross-validation parameters
      crossvalI_n <- if (nrow(dfaux2) > 7)
        7
      else
        nrow(dfaux2) - 2
      
      ### PLS
      # PLS-DA ----
      PLS <-
        ropls::opls(
          dfaux2,
          groups$Group[which(groups$Group %in% c(group1, group2))],
          crossvalI = crossvalI_n,
          permI = 1000,
          scaleC = "standard",
          fig.pdfC = "none",
          info.txtC = "none"
        )
      
      # Drawing if PLS-DA runs unsuccessfully
      if (dim(PLS@modelDF)[1] == 0) {
        PLS <-
          ropls::opls(
            dfaux2,
            groups$Group[which(groups$Group %in% c(group1, group2))],
            predI = 2,
            crossvalI = crossvalI_n,
            permI = 1000,
            scaleC = "standard",
            fig.pdfC = "none",
            info.txtC = "none"
          )
      } else{
        # Ensure that PLS-DA results have a dimension of 2 after dimensionality reduction
        if (PLS@summaryDF["Total", "pre"] <= 1) {
          PLS <-
            ropls::opls(
              dfaux2,
              groups$Group[which(groups$Group %in% c(group1, group2))],
              predI = 2,
              crossvalI = crossvalI_n,
              permI = 1000,
              scaleC = "standard",
              fig.pdfC = "none",
              info.txtC = "none"
            )
        }
      }
      
      # Drawing if PLS-DA runs successfully
      if (dim(PLS@modelDF)[1] != 0) {
        if (PLS@summaryDF["Total", "pre"] > 1) {
          # * PLS-DA plot ----
          dfaux_PLS <- as.data.frame(PLS@scoreMN) %>%
            cbind(., PLS@orthoScoreMN) %>%
            mutate(Sample = row.names(.)) %>%
            left_join(groups[which(groups$Group %in% c(group1, group2)), ], by = c("Sample" = "Sample"))
          PLSPlot <- ggplot(dfaux_PLS, aes(
            x = p1,
            y = p2,
            color = Group
          )) +
            labs(
              x = paste("Component 1 (", round(PLS@modelDF[["R2X"]][1] * 100, 2), "%)", sep = ""),
              y = paste("Component 2 (", round(PLS@modelDF[["R2X"]][2] * 100, 2), "%)", sep = ""),
              #title = paste(group1, " vs ", group2, " - PLS-DA plot", sep = ""),
              subtitle = paste("R2Y: ", PLS@modelDF[nrow(PLS@modelDF), "R2Y(cum)"], ", Q2: ", PLS@modelDF[nrow(PLS@modelDF), "Q2(cum)"], sep = "")
            ) +
            geom_point(size = 3) +
            scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(
              dfaux_PLS$Group
            )) +
              1, "Set1")) +
            scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(
              dfaux_PLS$Group
            )) +
              1, "Set1")) +
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
            theme(
              legend.text = element_text(size = font_size - 4, face = "plain"),
              legend.key.height = unit(0.05, "snpc"),
              legend.key.width = unit(0.05, "snpc")
            )
          if (all(table(groups$Group[which(groups$Group %in% c(group1, group2))]) > 3)) {
            PLSPlot <- PLSPlot + stat_ellipse(
              aes(fill = Group),
              geom = "polygon",
              alpha = 0.4,
              level = 0.95,
              color = "gray"
            )
          } else {
            PLSPlot <-
              PLSPlot + ggalt::geom_encircle(
                aes(fill = Group),
                expand = 0,
                spread = 1,
                s_shape = 1,
                alpha = 0.4
              )
          }
          ggsave(
            paste(
              group_vs_biomarker_dir,
              "/",
              group1,
              "_vs_",
              group2,
              "_PLS-DA-Plot.pdf",
              sep = ""
            ),
            width = width,
            height = height,
            units = "cm"
          )
          
          # * Permutation testing plot ----
          dfaux_PLSpt <- as.data.frame(PLS@suppLs[["permMN"]]) %>%
            select("R2Y(cum)", "Q2(cum)", "sim") %>%
            pivot_longer(
              cols = c("R2Y(cum)", "Q2(cum)"),
              names_to = "group",
              values_to = "value"
            )
          dfaux_PLSpt$group[dfaux_PLSpt$group == "R2Y(cum)"] <-
            "R2"
          dfaux_PLSpt$group[dfaux_PLSpt$group == "Q2(cum)"] <-
            "Q2"
          R2YLM <- dfaux_PLSpt %>%
            filter(group == "R2") %>%
            lm(formula = value ~ sim)
          Q2YLM <- dfaux_PLSpt %>%
            filter(group == "Q2") %>%
            lm(formula = value ~ sim)
          PLSPtPlot <- ggplot(dfaux_PLSpt) +
            geom_point(aes(
              x = sim,
              y = value,
              color = group
            ), size = 3) +
            scale_color_manual(values = c("#A493FF", "#FFB2A5")) +
            geom_abline(
              intercept = Q2YLM$coefficients[[1]],
              slope = Q2YLM$coefficients[[2]],
              color = "#A493FF",
              lwd = 0.75
            ) +
            geom_abline(
              intercept = R2YLM$coefficients[[1]],
              slope = R2YLM$coefficients[[2]],
              color = "#FFB2A5",
              lwd = 0.75
            ) +
            labs(
              x = bquote(Similarity(y, y[perm])),
              y = "R2 & Q2 Value",
              # title = paste(
              #   group1,
              #   " vs ",
              #   group2,
              #   " - PLS-DA Permutation testing plot",
              #   sep = ""
              # ),
              subtitle = paste(
                "R2:(0.0, ",
                round(R2YLM$coefficients[[1]], 2),
                "), Q2:(0.0, ",
                round(Q2YLM$coefficients[[1]], 2),
                ")",
                sep = ""
              )
            ) +
            geom_hline(aes(yintercept = 0),
                       linetype = 2,
                       lwd = 0.75) +
            geom_vline(aes(xintercept = 0),
                       linetype = 2,
                       lwd = 0.75) +
            theme(legend.title = element_blank()) +
            ggprism::theme_prism(
              base_size = font_size,
              base_line_size = 0.3,
              palette = "floral",
              base_family = "serif",
              base_fontface = "plain",
              border = TRUE
            ) +
            theme(
              legend.text = element_text(size = font_size - 4, face = "plain"),
              legend.key.height = unit(0.05, "snpc"),
              legend.key.width = unit(0.05, "snpc")
            )
          ggsave(
            paste(
              group_vs_biomarker_dir,
              "/",
              group1,
              "_vs_",
              group2,
              "_PLS-DA-Pt-Plot.pdf",
              sep = ""
            ),
            width = width,
            height = height,
            units = "cm"
          )
          
          # * VIP ----
          vip <- ropls::getVipVn(PLS)
          vip_select <- vip[vip > 1]
          dfaux_vip <-
            cbind(df[names(vip_select), 1:6], vip_select)
          colnames(dfaux_vip)[ncol(dfaux_vip)] <- "VIP"
          dfaux_vip <-
            dfaux_vip[order(dfaux_vip$VIP, decreasing = TRUE), ]
          df_diff_vip <-
            inner_join(dfPV, dfaux_vip)
          if (nrow(df_diff_vip) >= 1) {
            openxlsx::write.xlsx(
              df_diff_vip,
              paste(
                group_vs_biomarker_dir,
                "/",
                group1,
                "_vs_",
                group2,
                "_Diff_PLS-DA-VIP.xlsx",
                sep = ""
              )
            )
          }
        }
      }
      
      # OPLS-DA ----
      OPLS <-
        ropls::opls(
          dfaux2,
          groups$Group[which(groups$Group %in% c(group1, group2))],
          predI = 1,
          orthoI = NA,
          crossvalI = crossvalI_n,
          permI = 1000,
          scaleC = "standard",
          fig.pdfC = "none",
          info.txtC = "none"
        )
      # Drawing if PLS-DA runs successfully
      if (dim(OPLS@modelDF)[1] != 0) {
        if (OPLS@summaryDF["Total", "ort"] >= 1) {
          # * OPLS-DA plot ----
          dfaux_OPLS <- as.data.frame(OPLS@scoreMN) %>%
            cbind(., OPLS@orthoScoreMN) %>%
            mutate(Sample = row.names(.)) %>%
            left_join(groups[which(groups$Group %in% c(group1, group2)), ], by = c("Sample" = "Sample"))
          OPLSPlot <-
            ggplot(dfaux_OPLS, aes(
              x = p1,
              y = o1,
              color = Group
            )) +
            labs(
              x = paste(
                "Component 1 (",
                round(OPLS@modelDF[["R2X"]][1] * 100, 2),
                "%)",
                sep = ""
              ),
              y = paste(
                "Orthogonal Component 1 (",
                round(OPLS@modelDF[["R2X"]][2] * 100, 2),
                "%)",
                sep = ""
              ),
              # title = paste(group1, " vs ", group2, " - OPLS-DA plot", sep = ""),
              subtitle = paste("R2Y: ", OPLS@modelDF["sum", "R2Y(cum)"], ", Q2: ", OPLS@modelDF["sum", "Q2(cum)"], sep = "")
            ) +
            geom_point(size = 3) +
            scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(
              dfaux_OPLS$Group
            )) +
              1, "Set1")) +
            scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(
              dfaux_OPLS$Group
            )) +
              1, "Set1")) +
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
            theme(
              legend.text = element_text(size = font_size - 4, face = "plain"),
              legend.key.height = unit(0.05, "snpc"),
              legend.key.width = unit(0.05, "snpc")
            )
          if (all(table(groups$Group[which(groups$Group %in% c(group1, group2))]) >
                  3)) {
            OPLSPlot <- OPLSPlot + stat_ellipse(
              aes(fill = Group),
              geom = "polygon",
              alpha = 0.4,
              level = 0.95,
              color = "gray"
            )
          } else {
            OPLSPlot <-
              OPLSPlot + ggalt::geom_encircle(
                aes(fill = Group),
                expand = 0,
                spread = 1,
                s_shape = 1,
                alpha = 0.4
              )
          }
          ggsave(
            paste(
              group_vs_biomarker_dir,
              "/",
              group1,
              "_vs_",
              group2,
              "_OPLS-DA-Plot.pdf",
              sep = ""
            ),
            width = width,
            height = height,
            units = "cm"
          )
          
          # * Permutation testing plot ----
          dfaux_OPLSpt <-
            as.data.frame(OPLS@suppLs[["permMN"]]) %>%
            select("R2Y(cum)", "Q2(cum)", "sim") %>%
            pivot_longer(
              cols = c("R2Y(cum)", "Q2(cum)"),
              names_to = "group",
              values_to = "value"
            )
          dfaux_OPLSpt$group[dfaux_OPLSpt$group == "R2Y(cum)"] <-
            "R2"
          dfaux_OPLSpt$group[dfaux_OPLSpt$group == "Q2(cum)"] <-
            "Q2"
          R2YLM <- dfaux_OPLSpt %>%
            filter(group == "R2") %>%
            lm(formula = value ~ sim)
          Q2YLM <- dfaux_OPLSpt %>%
            filter(group == "Q2") %>%
            lm(formula = value ~ sim)
          OPLSPtPlot <- ggplot(dfaux_OPLSpt) +
            geom_point(aes(
              x = sim,
              y = value,
              color = group
            ), size = 3) +
            scale_color_manual(values = c("#A493FF", "#FFB2A5")) +
            geom_abline(
              intercept = Q2YLM$coefficients[[1]],
              slope = Q2YLM$coefficients[[2]],
              color = "#A493FF",
              lwd = 0.75
            ) +
            geom_abline(
              intercept = R2YLM$coefficients[[1]],
              slope = R2YLM$coefficients[[2]],
              color = "#FFB2A5",
              lwd = 0.75
            ) +
            labs(
              x = bquote(Similarity(y, y[perm])),
              y = "R2 & Q2 Value",
              # title = paste(
              #   group1,
              #   " vs ",
              #   group2,
              #   " - OPLS-DA Permutation testing plot",
              #   sep = ""
              # ),
              subtitle = paste(
                "R2:(0.0, ",
                round(R2YLM$coefficients[[1]], 2),
                "), Q2:(0.0, ",
                round(Q2YLM$coefficients[[1]], 2),
                ")",
                sep = ""
              )
            ) +
            geom_hline(aes(yintercept = 0),
                       linetype = 2,
                       lwd = 0.75) +
            geom_vline(aes(xintercept = 0),
                       linetype = 2,
                       lwd = 0.75) +
            theme(legend.title = element_blank()) +
            ggprism::theme_prism(
              base_size = font_size,
              base_line_size = 0.3,
              palette = "floral",
              base_family = "serif",
              base_fontface = "plain",
              border = TRUE
            ) +
            theme(
              legend.text = element_text(size = font_size - 4, face = "plain"),
              legend.key.height = unit(0.05, "snpc"),
              legend.key.width = unit(0.05, "snpc")
            )
          ggsave(
            paste(
              group_vs_biomarker_dir,
              "/",
              group1,
              "_vs_",
              group2,
              "_OPLS-DA-Pt-Plot.pdf",
              sep = ""
            ),
            width = width,
            height = height,
            units = "cm"
          )
          
          # * VIP ----
          ovip <- ropls::getVipVn(OPLS)
          ovip_select <- ovip[ovip > 1]
          dfaux_ovip <-
            cbind(df[names(ovip_select), 1:6], ovip_select)
          colnames(dfaux_ovip)[ncol(dfaux_ovip)] <- "VIP"
          dfaux_ovip <-
            dfaux_ovip[order(dfaux_ovip$VIP, decreasing = TRUE), ]
          df_diff_ovip <-
            inner_join(dfPV, dfaux_ovip)
          if (nrow(df_diff_ovip) >= 1) {
            openxlsx::write.xlsx(
              df_diff_ovip,
              paste(
                group_vs_biomarker_dir,
                "/",
                group1,
                "_vs_",
                group2,
                "_Diff_OPLS-DA-VIP.xlsx",
                sep = ""
              )
            )
          }
        }
      }
    }
  
  Group <- unique(groups$Group)
  
  # Analysing Differentials between Multiple Groups (>2) with PLS-DA
  if (length(Group) > 2) {
    dfaux_groups <- df %>%
      select(-(1:6)) %>%
      t()
    crossvalI_n <- if (nrow(dfaux_groups) > 7)
      7
    else
      nrow(dfaux_groups) - 2
    
    # PLS-DA ----
    PLS <-
      ropls::opls(
        dfaux_groups,
        groups$Group,
        crossvalI = crossvalI_n,
        permI = 1000,
        scaleC = "standard",
        fig.pdfC = "none",
        info.txtC = "none"
      )
    
    # Drawing if PLS-DA runs unsuccessfully
    if (dim(PLS@modelDF)[1] == 0) {
      PLS <-
        ropls::opls(
          dfaux_groups,
          groups$Group,
          predI = 2,
          crossvalI = crossvalI_n,
          permI = 1000,
          scaleC = "standard",
          fig.pdfC = "none",
          info.txtC = "none"
        )
    } else{
      # Ensure that PLS-DA results have a dimension of 2 after dimensionality reduction
      if (PLS@summaryDF["Total", "pre"] <= 1) {
        PLS <-
          ropls::opls(
            dfaux_groups,
            groups$Group,
            predI = 2,
            crossvalI = crossvalI_n,
            permI = 1000,
            scaleC = "standard",
            fig.pdfC = "none",
            info.txtC = "none"
          )
      }
    }
    
    # Drawing if PLS-DA runs successfully
    if (dim(PLS@modelDF)[1] != 0) {
      # Ensure that PLS-DA results have a dimension of 2 after dimensionality reduction
      if (PLS@summaryDF["Total", "pre"] > 1) {
        # * PLS-DA plot ----
        dfaux_PLS <- as.data.frame(PLS@scoreMN) %>%
          cbind(., PLS@orthoScoreMN) %>%
          mutate(Sample = row.names(.)) %>%
          left_join(groups, by = c("Sample" = "Sample"))
        PLSPlot <- ggplot(dfaux_PLS, aes(
          x = p1,
          y = p2,
          color = Group
        )) +
          labs(
            x = paste("Component 1 (", round(PLS@modelDF[["R2X"]][1] * 100, 2), "%)", sep = ""),
            y = paste("Component 2 (", round(PLS@modelDF[["R2X"]][2] * 100, 2), "%)", sep = ""),
            # title = "PLS-DA plot",
            subtitle = paste("R2Y: ", PLS@modelDF[nrow(PLS@modelDF), "R2Y(cum)"], ", Q2: ", PLS@modelDF[nrow(PLS@modelDF), "Q2(cum)"], sep = "")
          ) +
          geom_point(size = 3) +
          scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(
            dfaux_PLS$Group
          )) +
            1, "Set1")) +
          scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(
            dfaux_PLS$Group
          )) +
            1, "Set1")) +
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
          theme(
            legend.text = element_text(size = font_size - 4, face = "plain"),
            legend.key.height = unit(0.05, "snpc"),
            legend.key.width = unit(0.05, "snpc")
          )
        if (all(table(groups$Group) > 3)) {
          PLSPlot <- PLSPlot + stat_ellipse(
            aes(fill = Group),
            geom = "polygon",
            alpha = 0.4,
            level = 0.95,
            color = "gray"
          )
        } else {
          PLSPlot <- PLSPlot + ggalt::geom_encircle(
            aes(fill = Group),
            expand = 0,
            spread = 1,
            s_shape = 1,
            alpha = 0.4
          )
        }
        ggsave(
          paste(grouped_biomarker_dir, "/", "PLS-DA-Plot.pdf", sep = ""),
          width = width,
          height = height,
          units = "cm"
        )
        
        # * Permutation testing plot ----
        dfaux_PLSpt <- as.data.frame(PLS@suppLs[["permMN"]]) %>%
          select("R2Y(cum)", "Q2(cum)", "sim") %>%
          pivot_longer(
            cols = c("R2Y(cum)", "Q2(cum)"),
            names_to = "group",
            values_to = "value"
          )
        dfaux_PLSpt$group[dfaux_PLSpt$group == "R2Y(cum)"] <- "R2"
        dfaux_PLSpt$group[dfaux_PLSpt$group == "Q2(cum)"] <- "Q2"
        R2YLM <- dfaux_PLSpt %>%
          filter(group == "R2") %>%
          lm(formula = value ~ sim)
        Q2YLM <- dfaux_PLSpt %>%
          filter(group == "Q2") %>%
          lm(formula = value ~ sim)
        PLSPtPlot <- ggplot(dfaux_PLSpt) +
          geom_point(aes(
            x = sim,
            y = value,
            color = group
          ), size = 3) +
          scale_color_manual(values = c("#A493FF", "#FFB2A5")) +
          geom_abline(
            intercept = Q2YLM$coefficients[[1]],
            slope = Q2YLM$coefficients[[2]],
            color = "#A493FF",
            lwd = 0.75
          ) +
          geom_abline(
            intercept = R2YLM$coefficients[[1]],
            slope = R2YLM$coefficients[[2]],
            color = "#FFB2A5",
            lwd = 0.75
          ) +
          labs(
            x = bquote(Similarity(y, y[perm])),
            y = "R2 & Q2 Value",
            # title = "PLS-DA Permutation testing",
            subtitle = paste(
              "R2:(0.0, ",
              round(R2YLM$coefficients[[1]], 2),
              "), Q2:(0.0, ",
              round(Q2YLM$coefficients[[1]], 2),
              ")",
              sep = ""
            )
          ) +
          geom_hline(aes(yintercept = 0),
                     linetype = 2,
                     lwd = 0.75) +
          geom_vline(aes(xintercept = 0),
                     linetype = 2,
                     lwd = 0.75) +
          theme(legend.title = element_blank()) +
          ggprism::theme_prism(
            base_size = font_size,
            base_line_size = 0.3,
            palette = "floral",
            base_family = "serif",
            base_fontface = "plain",
            border = TRUE
          ) +
          theme(
            legend.text = element_text(size = font_size - 4, face = "plain"),
            legend.key.height = unit(0.05, "snpc"),
            legend.key.width = unit(0.05, "snpc")
          )
        ggsave(
          paste(
            grouped_biomarker_dir,
            "/",
            "PLS-DA-Pt-Plot.pdf",
            sep = ""
          ),
          width = width,
          height = height,
          units = "cm"
        )
        
        # * VIP ----
        vip <- ropls::getVipVn(PLS)
        vip_select <- vip[vip > 1]
        dfaux_vip <-
          cbind(df[names(vip_select), 1:6], vip_select)
        colnames(dfaux_vip)[ncol(dfaux_vip)] <- "VIP"
        dfaux_vip <-
          dfaux_vip[order(dfaux_vip$VIP, decreasing = TRUE), ]
        if (nrow(dfaux_vip) >= 1) {
          openxlsx::write.xlsx(dfaux_vip,
                               paste(grouped_biomarker_dir, "/", "PLS-DA-VIP.xlsx", sep = ""))
        }
      }
    }
  }
  
  # Analysing Differentials between two Groups with PLS-DA
  for (group1 in Group) {
    ano <- Group[-which(group1 == Group)]
    for (group2 in ano) {
      grouped_biomarker(
        df,
        groups,
        grouped_biomarker_dir,
        group1,
        group2,
        font_size = font_size,
        width = width,
        height = height
      )
    }
  }
  
  return_result$type <- "message"
  return_result$text_message <-
    "The biomarker function has been run!"
  return(return_result)
}