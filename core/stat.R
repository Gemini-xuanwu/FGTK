stat <- function(compare_result_dir,
                 calculate_percentage = TRUE,
                 calculate_type = "standard deviation") {
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
  
  # Renumber the compounds and reorder according to retention time
  df <- df %>%
    arrange(RT) %>%
    mutate(Compound = paste0("Compound_", seq(1, nrow(.))))
  
  ### If there is only one duplicate group or just have one group then only the percentage will be calculated.
  if (any(table(groups$Group) < 2) |
      (length(unique(groups$Group)) < 2)) {
    df[is.na(df)] <- 0
    # Calculation of the percentage of individual volatile
    df1 <- df[1:6]
    df1 <- cbind(df1, apply(df[-1:-6], 2, function(x) {
      x <- x / sum(x) * 100
    }))
    # Save calculate percentage results
    openxlsx::write.xlsx(df1,
                         paste0(
                           compare_result_dir,
                           "/",
                           "calculate_only_percentage.xlsx"
                         ))
    
    return_result$type <- "message"
    return_result$text_message <-
      "Since there is only one replicate in a group or just have one group, only percentages are calculated, not differences!"
    return(return_result)
  }
  
  df[is.na(df)] <- 0
  # Calculation of the percentage of individual volatile
  df1 <- df[1:6]
  df1 <- cbind(df1, apply(df[-1:-6], 2, function(x) {
    x <- x / sum(x) * 100
  }))
  # Save calculate percentage results
  openxlsx::write.xlsx(df1,
                       paste0(compare_result_dir, "/", "calculate_only_percentage.xlsx"))
  
  # Create new dataframe
  df2 <- df1[1:6]
  # create new columns of group and P value
  df2[, append(sort(unique(groups$Group)), "P")] <- NA
  for (i in seq(1, nrow(df1))) {
    if (calculate_percentage == TRUE) {
      tmp <- df1[i, -1:-6]
    } else{
      tmp <- df[i, -1:-6]
    }
    tmp <- t(tmp)
    colnames(tmp)[1] <- "Value"
    tmp <- as.data.frame(tmp)
    tmp$Group <- groups$Group
    
    # multifactor analysis of variance (ANOVA)
    aov <- aov(Value ~ Group, data = tmp)
    
    # Multiple comparisons and use LSD method
    aovH <- agricolae::LSD.test(aov, "Group")$groups
    colnames(aovH)[1:2] <- c("Mean", "Label")
    aovH$Group <- row.names(aovH)
    
    # Calculate standard deviation or standard error of mean (sd/sd or sem/plotrix::std.error)
    if (calculate_type == "standard deviation") {
      sds <- aggregate(Value ~ Group, data = tmp, sd)
    } else{
      sds <- aggregate(Value ~ Group, data = tmp, plotrix::std.error)
    }
    colnames(sds)[2] <- "sd"
    
    # Merge mean, sd, label
    if (calculate_percentage == TRUE) {
      new_data <- sds %>%
        inner_join(aovH, by = c(Group = "Group")) %>%
        mutate(all = paste0("(", round(Mean, 2), "±", round(sd, 2), "%", ")", Label))
    }
    else{
      new_data <- sds %>%
        inner_join(aovH, by = c(Group = "Group")) %>%
        mutate(all = paste0("(", round(Mean, 2), "±", round(sd, 2), ")", Label))
    }
    
    # rearrange
    new_data <- new_data[order(new_data[, "Group"]), ]
    
    # insert calculated value
    insert <- new_data$all
    insert <- append(insert, summary(aov)[[1]][1, 5])
    df2[i, -1:-6] <- insert
  }
  # Save results
  openxlsx::write.xlsx(df2, paste0(compare_result_dir, "/", "stat.xlsx"))
  
  return_result$type <- "message"
  return_result$text_message <- "The stat function has been run!"
  return(return_result)
}