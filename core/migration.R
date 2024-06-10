migration <- function(raw_dir_path, data_dir_path, file_type = "peak table") {
  # create a list of returned result
  return_result <- list()
  
  # Import the necessary libraries
  if (!("package:tidyverse" %in% search())) {
    library(tidyverse)
  }
  
  # Check if the raw data directory exists
  if (!exists("raw_dir_path")) {
    return_result$type <- "error"
    return_result$text_message <-
      "The raw data directory parameter does not set. Please provide the necessary parameter."
    return(return_result)
  } else {
    if (!dir.exists(raw_dir_path)) {
      return_result$type <- "error"
      return_result$text_message <-
        "The raw data directory does not exist. Please check the path."
      return(return_result)
    }
  }
  
  # Check if the migrated data directory exists
  if (!exists("data_dir_path")) {
    return_result$type <- "error"
    return_result$text_message <-
      "The migrated data directory parameter does not set. Please provide the necessary parameter."
    return(return_result)
  } else {
    if (!dir.exists(data_dir_path)) {
      return_result$type <- "error"
      return_result$text_message <-
        "The migrated data directory does not exist. Please check the path."
      return(return_result)
    }
  }
  
  # Get a list of subdirectories (group names)
  dirs <- list.dirs(raw_dir_path, full.names = F, recursive = F)
  # Identify the folder in which Agilent stores data
  pattern <- "\\.D"
  matching_indices <- grep(pattern, dirs)
  if (length(matching_indices) == 0) {
    return_result$type <- "error"
    return_result$text_message <-
      "The raw data directory does not any data.D!!!"
    return(return_result)
  }
  matching_dirs <- dirs[matching_indices]
  
  # Migrate xls files
  j <- 0
  for (dir in matching_dirs) {
    dir_path <- paste0(raw_dir_path, "/", dir)
    if (file_type == "peak table") {
      file_path <- file.path(dir_path, "MSRep.xls")
      copy_path <- paste0(data_dir_path, "/", gsub(pattern, "", dir), ".xls")
    }
    else{
      file_path <- file.path(dir_path, "CHROMTAB.CSV")
      copy_path <- paste0(data_dir_path, "/", gsub(pattern, "", dir), ".CSV")
    }
    if (file.exists(file_path)) {
      file.copy(file_path, copy_path, overwrite = TRUE)
      j <- j + 1
    }
  }
  
  # Check if the MSRep.xls exists
  if (j == 0) {
    return_result$type <- "error"
    return_result$text_message <-
      "No MSRep.xls or CHROMTAB.CSV in each data.D folder! Please use Agilent ChemStation to export the Integration Report and Spectral Library Search Report or export data to CSV file."
    return(return_result)
  }
  
  return_result$type <- "message"
  return_result$text_message <-
    "The migration function has been run!"
  return(return_result)
}