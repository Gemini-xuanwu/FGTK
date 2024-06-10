compare_gcms_results <- function(input_dir_path,
                                 output_dir_path,
                                 min_quality = 60,
                                 max_rt = 25,
                                 within_thr = 0.005,
                                 between_thr = 0.010,
                                 min_comparative_quality = 0.5) {
  # create a list of returned result
  return_result <- list()
  
  # Import the necessary libraries
  if (!("package:tidyverse" %in% search())) {
    library(tidyverse)
  }
  
  # Check if the input directory exists
  if (!exists("input_dir_path")) {
    return_result$type <- "error"
    return_result$text_message <-
      "The input directory parameter does not set. Please provide the necessary parameter."
    return(return_result)
  } else {
    if (!dir.exists(input_dir_path)) {
      return_result$type <- "error"
      return_result$text_message <-
        "The input directory does not exist. Please check the path."
      return(return_result)
    }
  }
  
  # Check if the output directory exists
  if (!exists("output_dir_path")) {
    return_result$type <- "error"
    return_result$text_message <-
      "The output directory parameter does not set. Please provide the necessary parameter."
    return(return_result)
  } else {
    if (!dir.exists(output_dir_path)) {
      return_result$type <- "error"
      return_result$text_message <-
        "The output directory does not exist. Please check the path."
      return(return_result)
    }
  }
  
  # Create empty vectors of samples, paths, groups
  samples <- NULL
  paths <- NULL
  groups <- NULL
  
  # Get a list of subdirectories (group names)
  dirs <- list.dirs(input_dir_path,
                    full.names = F,
                    recursive = F)
  if (length(dirs) == 0) {
    return_result$type <- "error"
    return_result$text_message <-
      "There are no subdirectories in this directory, it does not meet the requirements of this function, please check the path."
    return(return_result)
  }
  # Get a list of data path, sample name and sample group
  for (dir in dirs) {
    dir_path <- paste0(input_dir_path, "/", dir)
    files <- list.files(path = dir_path, pattern = "(.xls)$")
    # Report an error if the xls file is not detected
    if (length(files) == 0) {
      return_result$type <- "error"
      return_result$text_message <-
        "No .xls files found in this directory. Please delete the empty folder so that it does not affect the next calculation."
      return(return_result)
    }
    samples_paths <- paste0(dir_path, "/", files)
    samples <- append(samples, sub(".xls$", "", files))
    paths <- append(paths, samples_paths)
    groups <- append(groups, rep(dir, times = length(files)))
  }
  group_xlsx <- data.frame(Sample = samples, Group = groups)
  
  df_samples <- data.frame()
  
  ### Read all datas and compare within data
  for (path in paths) {
    # Read a data
    df <-
      readxl::read_excel(path,
                         sheet = "LibRes",
                         skip = 8,
                         col_names = TRUE)
    if (dim(df)[2] != 14) {
      return_result$type <- "error"
      return_result$text_message <-
        "The agilent chemstation software did not successfully export the full xls file, please check the file!!!"
      return(return_result)
    }
    df <- df[, c(1, 2, 4, 9, 10, 11, 12)]
    names(df) <-
      c("Compound",
        "RT",
        "Area",
        "Hitname",
        "Quality",
        "Weight",
        "CAS")
    
    # Filling in the missing pieces of raw data
    df1 <- df
    for (i in seq(1, dim(df1)[1])) {
      if (is.na(df1[i, 1])) {
        df1[i, 1:3] <- df1[i - 1, 1:3]
      }
    }
    
    # Filter data by retention time and comparison result
    df2 <- df1
    df2 <- df2 %>%
      dplyr::filter(RT <= max_rt) %>%
      dplyr::filter(Quality >= min_quality)
    
    # Create a new column to indicate whether the compound has been extracted as a candidate compound
    df2$revised_id <- 0
    # Create a new list to store the candidate compounds for just one sample filtered in the following loop
    df_sample_list <- list()
    j <- 1
    # Rigorously distinguish compounds based on retention time at first
    for (i in seq(1, nrow(df2))) {
      if (df2$revised_id[i] != 1) {
        df_sample_list[[as.character(j)]] <- df2[df2$RT == df2$RT[i], ]
        df2$revised_id[df2$RT == df2$RT[i]] <- 1
        j <- j + 1
      }
    }
    
    # If the difference in retention time between the two compounds is less than 0.005min and
    # the probability of occurrence of the same compound in both results is more than 30 percent
    # of the probability of occurrence of the respective compounds,the two compounds are combined
    # into one compound.
    for (i in seq(1, length(df_sample_list) - 1)) {
      if (plyr::empty(df_sample_list[[i]]) == TRUE) {
        next
      } else {
        for (j in seq((i + 1), length(df_sample_list))) {
          if (plyr::empty(df_sample_list[[j]]) == TRUE) {
            next
          } else {
            if (abs(mean(unique(df_sample_list[[j]]$RT)) - mean(unique(df_sample_list[[i]]$RT))) <=
                within_thr) {
              alike <-
                intersect(unique(df_sample_list[[j]]$CAS),
                          unique(df_sample_list[[i]]$CAS))
              if (length(alike) > 0) {
                alike_j <-
                  length(which(df_sample_list[[j]]$"CAS" %in% alike)) / nrow(df_sample_list[[j]])
                alike_i <-
                  length(which(df_sample_list[[i]]$"CAS" %in% alike)) / nrow(df_sample_list[[i]])
                if ((alike_j >= min_comparative_quality) &
                    (alike_i >= min_comparative_quality)) {
                  df_sample_list[[i]] <-
                    rbind(df_sample_list[[i]], df_sample_list[[j]])
                  df_sample_list[[i]]$RT <-
                    mean(unique(df_sample_list[[i]]$RT))
                  df_sample_list[[i]]$Area <-
                    sum(unique(df_sample_list[[i]]$Area))
                  df_sample_list[[j]] <- data.frame()
                  names(df_sample_list[[j]]) <-
                    c("Compound",
                      "RT",
                      "Area",
                      "Hitname",
                      "Quality",
                      "Weight",
                      "CAS")
                } else {
                  next
                }
              } else {
                next
              }
            } else {
              next
            }
          }
        }
      }
    }
    
    # Collate data and put all data in another variable
    df_sample_result <- plyr::ldply(df_sample_list, data.frame)
    df_sample_result <-
      df_sample_result[c("Compound",
                         "RT",
                         "Area",
                         "Hitname",
                         "Quality",
                         "Weight",
                         "CAS")]
    df_sample_result$Sample <- samples[path == paths]
    df_sample_result$Group <- groups[path == paths]
    df_samples <- rbind(df_samples, df_sample_result)
  }
  
  ### compare between data
  # Ensure that certain variables are always numeric
  df_samples[, c(1, 2, 3, 5, 6)] <-
    sapply(df_samples[, c(1, 2, 3, 5, 6)], as.numeric)
  # Reset the column data to 0 in order to indicate whether the compound has been extracted as
  # a candidate compound
  df_samples$revised_id <- 0
  # Create a new list for storing dataframes grouped with samples and retention times.
  df_groups_list <- df_samples %>%
    dplyr::group_by(RT, Sample) %>%
    dplyr::do(vals = data.frame(.)) %>%
    select(vals) %>%
    lapply(function(x) {
      (x)
    })
  
  # Create a new list for storing comparison results
  df_sample_list <- list()
  j <- 1
  # If the difference in retention time between the two compounds is less than 0.015min and
  # the probability of occurrence of the same compound in both results is more than 30 percent of
  # the probability of occurrence of the respective compounds, the two compounds are
  # combined into one compound.
  while (any(unlist(lapply(df_groups_list$vals, function(x) {
    any(x$revised_id == 0)
  })) == TRUE)) {
    df_sample_list[[j]] <- data.frame()
    for (i in seq(1, length(df_groups_list$vals))) {
      if ((nrow(df_sample_list[[j]]) == 0) &
          any(df_groups_list$vals[[i]]$revised_id %in% 1)) {
        next
      } else if ((nrow(df_sample_list[[j]]) == 0) &
                 any(df_groups_list$vals[[i]]$revised_id %in% 0)) {
        df_sample_list[[j]] <-
          rbind(df_sample_list[[j]], df_groups_list$vals[[i]])
        df_groups_list$vals[[i]]$revised_id <- 1
      } else if ((nrow(df_sample_list[[j]]) != 0) &
                 any(df_groups_list$vals[[i]]$revised_id %in% 1)) {
        next
      } else if ((nrow(df_sample_list[[j]]) != 0) &
                 any(df_groups_list$vals[[i]]$revised_id %in% 0)) {
        if ((abs(df_sample_list[[j]]$RT[1] - df_groups_list$vals[[i]]$RT[1]) <= between_thr) &
            !(unique(df_groups_list$vals[[i]]$Sample) %in% unique(df_sample_list[[j]]$Sample))) {
          alike <-
            intersect(unique(df_groups_list$vals[[i]]$CAS),
                      unique(df_sample_list[[j]]$CAS))
          if (length(alike) > 0) {
            alike_i <-
              length(which(df_groups_list$vals[[i]]$"CAS" %in% alike)) / nrow(df_groups_list$vals[[i]])
            alike_j <-
              length(which(df_sample_list[[j]]$"CAS" %in% alike)) / nrow(df_sample_list[[j]])
            if ((alike_i >= min_comparative_quality) &
                (alike_j >= min_comparative_quality)) {
              df_sample_list[[j]] <-
                rbind(df_sample_list[[j]], df_groups_list$vals[[i]])
              df_sample_list[[j]]$RT <-
                mean(unique(df_sample_list[[j]]$RT))
              df_groups_list$vals[[i]]$revised_id <- 1
            } else {
              next
            }
          } else {
            next
          }
        } else {
          next
        }
      }
    }
    j <- j + 1
  }
  
  # Create a new list for storing filtered comparison results
  df_comparison_result <- list()
  j <- 1
  # Identify the compound according to their comparison results and convert row data to column
  # data for areas of different samples
  for (i in seq(1, length(df_sample_list))) {
    max_id <- aggregate(Quality ~ CAS, data = df_sample_list[[i]], max)
    mean_id <-
      aggregate(Quality ~ CAS, data = df_sample_list[[i]], mean)
    length_id <-
      aggregate(Quality ~ CAS, data = df_sample_list[[i]], length)
    max_equal <- which(max_id$Quality == max(max_id$Quality))
    if (length(max_equal) > 1) {
      max_mean_equal <-
        which(mean_id$Quality[max_equal] == max(mean_id$Quality[max_equal]))
      if (length(max_mean_equal) == 1) {
        identified_compound <- mean_id[max_equal[max_mean_equal], "CAS"]
      } else {
        final_equal <-
          which(length_id$Quality[max_equal[max_mean_equal]] == max(length_id$Quality[max_equal[max_mean_equal]]))
        identified_compound <-
          mean_id[max_equal[max_mean_equal[final_equal[1]]], "CAS"]
      }
    } else {
      identified_compound <- max_id[max_equal, "CAS"]
    }
    df_comparison_result[[j]] <-
      df_sample_list[[i]][which(df_sample_list[[i]]$CAS == identified_compound), ] %>%
      arrange(desc(Quality)) %>%
      head(1) %>%
      select(Compound, RT, Hitname, Quality, Weight, CAS)
    df_comparison_result[[j]][, samples] <- 0
    for (sample in samples) {
      if (sample %in% unique(df_sample_list[[i]]$Sample)) {
        df_comparison_result[[j]][, sample] <-
          sum(unique(df_sample_list[[i]][which(df_sample_list[[i]]$Sample ==
                                                 sample), "Area"]))
      }
    }
    j <- j + 1
  }
  df_data_result <- plyr::ldply(df_comparison_result)
  
  # Renumber the compounds and delete leading zeros in CAS and reorder according to retention time
  df_data_result <- df_data_result %>%
    arrange(RT) %>%
    mutate(Compound = paste0("Compound_", seq(1, nrow(.)))) %>%
    mutate(CAS = sub("^0+", "", CAS))
  
  # Save results
  openxlsx::write.xlsx(group_xlsx,
                       paste0(output_dir_path, "/", "sample_info.xlsx"))
  openxlsx::write.xlsx(df_data_result, paste0(output_dir_path, "/", "peak.xlsx"))
  
  return_result$type <- "message"
  return_result$text_message <-
    "The compare GC-MS results function has been run!"
  return(return_result)
}