get_classyfire_info <- function(compare_result_dir) {
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
    if (!file.exists(paste0(compare_result_dir, "/", "compound_info.xlsx"))) {
      return_result$type <- "error"
      return_result$text_message <-
        "The necessary files(compound_info.xlsx) do not exist, please check if the path is correct or re-run the comparison programme!"
      return(return_result)
    }
  }
  
  # Check the network
  if (httr::GET("http://classyfire.wishartlab.com/")$status_code != 200) {
    return_result$type <- "error"
    return_result$text_message <-
      "The network is not connected or you cannot access the classyfire website, please check the network!"
    return(return_result)
  }
  
  # Read the necessary files
  df <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "compound_info.xlsx"))
  
  # Retrieve classifications through the classyfire website
  classifications_list <-
    purrr::map(unique(na.omit(df$InChIKey)), classyfireR::get_classification)
  
  # Create a new list for storing filtered classification results
  df_classifications <- list()
  j <- 1
  # Extract classification results
  for (i in seq(1, length(classifications_list))) {
    df_classifications[[j]] <- data.frame()
    df_one_classification <-
      as.data.frame(t(classyfireR::classification(classifications_list[[i]])))
    colnames(df_one_classification) <- df_one_classification[1, ]
    df_classifications[[j]] <- df_one_classification[2, ]
    df_classifications[[j]]$InChIKey <-
      classyfireR::meta(classifications_list[[i]])$inchikey %>% sub("InChIKey=", "", .)
    j <- j + 1
  }
  df_classifications <- plyr::ldply(df_classifications)
  
  df_compound_classifications <-
    dplyr::left_join(df , df_classifications, by = "InChIKey")
  
  # Save results
  openxlsx::write.xlsx(
    df_compound_classifications,
    paste0(
      compare_result_dir,
      "/",
      "compound_classyfire_classification.xlsx"
    )
  )
  
  return_result$type <- "message"
  return_result$text_message <-
    "The classyfire function has been run!"
  return(return_result)
}