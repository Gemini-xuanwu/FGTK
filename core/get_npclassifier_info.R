get_npclassifier_info <- function(compare_result_dir) {
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
  if (httr::GET("https://npclassifier.ucsd.edu/")$status_code != 200) {
    return_result$type <- "error"
    return_result$text_message <-
      "The network is not connected or you cannot access the NP Classifier website, please check the network!"
    return(return_result)
  }
  
  # Read the necessary files
  df <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "compound_info.xlsx"))
  
  # Retrieve classifications through the NP Classifier website
  df_classifications <- df %>%
    select(Compound, IsomericSMILES) %>%
    filter(!is.na(IsomericSMILES)) %>%
    rename(compound = Compound, smiles = IsomericSMILES) %>%
    chemodiv::NPCTable(.)
  
  # Merger results
  df_compound_classifications <-
    dplyr::left_join(df,
                     df_classifications,
                     by = c("Compound" = "compound", "IsomericSMILES" = "smiles"))
  
  # Save results
  openxlsx::write.xlsx(
    df_compound_classifications,
    paste0(
      compare_result_dir,
      "/",
      "compound_npclassifier_classification.xlsx"
    )
  )
  
  return_result$type <- "message"
  return_result$text_message <-
    "The NP Classifier function has been run!"
  return(return_result)
}