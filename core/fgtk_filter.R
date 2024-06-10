fgtk_filter <- function(compare_result_dir,
                        cut_ck = FALSE,
                        frequency_within_group_thr = 0.4,
                        frequency_within_overall_thr = 0.2) {
  # create a list of returned result
  return_result <- list()
  
  # Import the necessary libraries
  if (!("package:tidyverse" %in% search())) {
    library(tidyverse)
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
  
  # Save old results
  openxlsx::write.xlsx(groups,
                       paste0(compare_result_dir, "/", "sample_info_no_filtered.xlsx"))
  openxlsx::write.xlsx(df,
                       paste0(compare_result_dir, "/", "peak_no_filtered.xlsx"))
  
  # If there is only one duplicate group then report an error.
  if (any(table(groups$Group) < 2) |
      (length(unique(groups$Group)) < 2)) {
    return_result$type <- "error"
    return_result$text_message <-
      "There exists only one duplicate of one or more groups here, and it is not possible to run the function!!!!"
    return(return_result)
  }
  
  df[is.na(df)] <- 0
  
  # Delete the CK group columns and theirs compound row datas
  if (!is.logical(cut_ck)) {
    return_result$type <- "error"
    return_result$text_message <-
      "The 'cut_ck' parameter should be a boolean (True or False)."
    return(return_result)
  } else {
    if (cut_ck == TRUE) {
      if ("CK" %in% groups$Group) {
        # Find the samples with group CK.
        CKs <- groups$Sample[which(groups$Group == "CK")]
        df <-
          df[-which(apply(
            as.data.frame(df[, CKs]),
            MARGIN = 1,
            FUN = sum
          ) >
            0), ] %>%
          select(-all_of(CKs))
        groups <- groups[groups$Group != "CK", ]
      } else {
        return_result$type <- "error"
        return_result$text_message <-
          "The 'CK' group was not found in the 'groups' data."
        return(return_result)
      }
    }
  }
  
  # Calculate the frequency of having the compound in the overall sample
  # and save if it is greater than the threshold value
  sample_size <- ncol(df[, -(1:6)])
  frequency_within_overall <-
    apply(df[, -(1:6)], 1, function(row)
      sum(row > 0) / sample_size)
  retained_index <-
    which(frequency_within_overall > frequency_within_overall_thr)
  
  # Calculate the frequency of having the compound in the samples within each group
  # and save if it is greater than the threshold value
  retained_another_index <- c()
  for (i in seq(1, nrow(df))) {
    frequency_within_group <- c()
    for (group in unique(groups$Group)) {
      tmp <- df[i, groups$Sample[which(groups$Group == group)]]
      frequency_within_group <-
        append(frequency_within_group, sum(tmp > 0) / ncol(tmp))
    }
    if (sum(frequency_within_group > frequency_within_group_thr) > 0) {
      retained_another_index <- append(retained_another_index, i)
    }
  }
  
  df_result <- df[union(retained_index, retained_another_index), ]
  # Renumber the compounds and reorder according to retention time
  df_result <- df_result %>%
    arrange(RT) %>%
    mutate(Compound = paste0("Compound_", seq(1, nrow(.))))
  
  # Save new results
  openxlsx::write.xlsx(groups, paste0(compare_result_dir, "/", "sample_info.xlsx"))
  openxlsx::write.xlsx(df_result, paste0(compare_result_dir, "/", "peak.xlsx"))
  
  return_result$type <- "message"
  return_result$text_message <- "The filter function has been run!"
  return(return_result)
}