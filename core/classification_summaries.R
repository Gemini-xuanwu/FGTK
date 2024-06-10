classification_summaries <- function(compare_result_dir, calculate_type = FALSE) {
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
        !file.exists(paste0(compare_result_dir, "/", "peak.xlsx")) |
        !file.exists(paste0(compare_result_dir, "/", "compound_info.xlsx"))) {
      return_result$type <- "error"
      return_result$text_message <-
        "The necessary files(sample_info.xlsx, peak.xlsx or compound_info.xlsx) do not exist, please check if the path is correct or re-run the comparison programme!"
      return(return_result)
    }
  }
  
  # Read the necessary files
  groups <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "sample_info.xlsx"))
  df <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "peak.xlsx"))
  compound_info <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "compound_info.xlsx"))
  
  # Check whether the Class column is in the compound_info.xlsx file
  if (!("Class" %in% colnames(compound_info))) {
    return_result$type <- "error"
    return_result$text_message <-
      "There is no Class column in the compound_info.xlsx file, that column requires you to write your own classified results for the compound!!!"
    return(return_result)
  }
  
  # Check whether the number of compounds in peak.xlsx is the same as in compound_info.xlsx
  if (nrow(df) != nrow(compound_info)) {
    return_result$type <- "error"
    return_result$text_message <-
      "The number of compounds in the peak.xlsx file does not match the number of compounds in the compound_info.xlsx file!"
    return(return_result)
  }
  
  # Count the number of classes for all compounds
  compound_classification_count <- count(compound_info, Class)
  
  # Count which compounds are which classes
  compound_class <- lapply(unique(compound_info$Class), function(x)
    which(compound_info$Class == x))
  names(compound_class) <- unique(compound_info$Class)
  
  # Calculate the absolute abundance of each class within each sample
  sample_class_sum <- lapply(compound_class, function(x) {
    df[x, ] %>%
      select(-1:-6) %>%
      colSums()
  })
  sample_class_sum_absolute <- bind_rows(sample_class_sum) %>%
    mutate(Class = names(sample_class_sum)) %>%
    select(Class, everything())
  
  # Calculate the relative abundance of each class within each sample
  sample_class_sum_relative <- cbind(sample_class_sum_absolute[1],
                                     apply(sample_class_sum_absolute[-1], 2, function(x) {
                                       x <- x / sum(x) * 100
                                     }))
  
  # Count the number of classes in each sample
  sample_class_count <- lapply(compound_class, function(x) {
    df[x, ] %>%
      select(-1:-6) %>%
      mutate(across(everything(), ~ ifelse(. > 0, 1, .))) %>%
      colSums()
  })
  sample_class_count <- bind_rows(sample_class_count) %>%
    mutate(Class = names(sample_class_count)) %>%
    select(Class, everything())
  
  # Calculate the absolute abundance of each class within each group
  group_class_sum_absolute <- sample_class_sum_absolute[1]
  group_class_sum_absolute[, append(sort(unique(groups$Group)), "P")] <- NA
  for (i in seq(1, nrow(sample_class_sum_absolute))) {
    tmp <- sample_class_sum_absolute[i, -1]
    tmp <- t(tmp)
    colnames(tmp)[1] <- "Value"
    tmp <- as.data.frame(tmp)
    tmp$Group <- groups$Group
    # multifactor analysis of variance (ANOVA)
    aov <- aov(Value ~ Group, data = tmp)
    
    # Multiple comparisons and use HSD method
    aovH <- agricolae::HSD.test(aov, "Group")$groups
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
    new_data = sds %>%
      inner_join(aovH, by = c(Group = "Group")) %>%
      mutate(all = paste0(round(Mean * 100, 5), "±", round(sd * 100, 5), Label))
    
    # rearrange
    new_data <- new_data[order(new_data[, "Group"]), ]
    
    # insert calculated value
    insert <- new_data$all
    insert <- append(insert, summary(aov)[[1]][1, 5])
    group_class_sum_absolute[i, -1] <- t(insert)
  }
  
  # Calculate the relative abundance of each class within each group
  group_class_sum_relative <- sample_class_sum_relative[1]
  group_class_sum_relative[, append(sort(unique(groups$Group)), "P")] <- NA
  for (i in seq(1, nrow(sample_class_sum_relative))) {
    tmp <- sample_class_sum_relative[i, -1]
    tmp <- t(tmp)
    colnames(tmp)[1] <- "Value"
    tmp <- as.data.frame(tmp)
    tmp$Group <- groups$Group
    # multifactor analysis of variance (ANOVA)
    aov <- aov(Value ~ Group, data = tmp)
    
    # Multiple comparisons and use HSD method
    aovH <- agricolae::HSD.test(aov, "Group")$groups
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
    new_data = sds %>%
      inner_join(aovH, by = c(Group = "Group")) %>%
      mutate(all = paste0(round(Mean, 5), "±", round(sd, 5), "%", Label))
    
    # rearrange
    new_data <- new_data[order(new_data[, "Group"]), ]
    
    # insert calculated value
    insert <- new_data$all
    insert <- append(insert, summary(aov)[[1]][1, 5])
    group_class_sum_relative[i, -1] <- t(insert)
  }
  
  # Count the number of classes in each group
  dftmp <- df[1:6]
  dftmp[, sort(unique(groups$Group))] <- NA
  for (i in seq(1, nrow(df))) {
    tmp <- df[i, -1:-6]
    tmp <- t(tmp)
    colnames(tmp)[1] <- "Value"
    tmp <- as.data.frame(tmp)
    tmp$Group <- groups$Group
    aggregate <- aggregate(Value ~ Group, data = tmp, FUN = mean)
    dftmp[i, -1:-6] <- t(aggregate$Value)
  }
  group_class_count <- lapply(compound_class, function(x) {
    dftmp[x, ] %>%
      select(-1:-6) %>%
      mutate(across(everything(), ~ ifelse(. > 0, 1, .))) %>%
      colSums()
  })
  group_class_count <- bind_rows(group_class_count) %>%
    mutate(Class = names(group_class_count)) %>%
    select(Class, everything())
  
  # Save seven dataframes to the same Excel file
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "all_count")
  openxlsx::writeData(wb, "all_count", compound_classification_count)
  
  openxlsx::addWorksheet(wb, "sample_count")
  openxlsx::writeData(wb, "sample_count", sample_class_count)
  
  openxlsx::addWorksheet(wb, "sample_absolute")
  openxlsx::writeData(wb, "sample_absolute", sample_class_sum_absolute)
  
  openxlsx::addWorksheet(wb, "sample_relative")
  openxlsx::writeData(wb, "sample_relative", sample_class_sum_relative)
  
  openxlsx::addWorksheet(wb, "group_count")
  openxlsx::writeData(wb, "group_count", group_class_count)
  
  openxlsx::addWorksheet(wb, "group_absolute")
  openxlsx::writeData(wb, "group_absolute", group_class_sum_absolute)
  
  openxlsx::addWorksheet(wb, "group_relative")
  openxlsx::writeData(wb, "group_relative", group_class_sum_relative)
  
  openxlsx::saveWorkbook(wb,
                         paste0(compare_result_dir, "/", "classification_summaries.xlsx"),
                         overwrite = TRUE)
  
  return_result$type <- "message"
  return_result$text_message <- "The classification summaries function has been run!"
  return(return_result)
}