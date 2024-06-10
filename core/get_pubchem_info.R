get_pubchem_info <- function(compare_result_dir) {
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
    if (!file.exists(paste0(compare_result_dir, "/", "peak.xlsx"))) {
      return_result$type <- "error"
      return_result$text_message <-
        "The necessary files(peak.xlsx) do not exist, please check if the path is correct or re-run the comparison programme!"
      return(return_result)
    }
  }
  
  # Check the network
  if (httr::GET("https://pubchem.ncbi.nlm.nih.gov/")$status_code != 200) {
    return_result$type <- "error"
    return_result$text_message <-
      "The network is not connected or you cannot access the Pubchem website, please check the network!"
    return(return_result)
  }
  
  # Read the necessary files
  df <-
    openxlsx::read.xlsx(paste0(compare_result_dir, "/", "peak.xlsx"))[, 1:6]
  
  # Renumber the compounds and reorder according to retention time
  df <- df %>%
    arrange(RT) %>%
    mutate(Compound = paste0("Compound_", seq(1, nrow(.))))
  
  # Identify the Pubchem CID number
  cid <-
    webchem::get_cid(unique(df$Hitname), from = "name", match = "first")
  
  # Obtain compound information based on the Pubchem CID number
  info <-
    webchem::pc_prop(
      unique(na.omit(cid$cid)),
      properties = c(
        "IUPACName",
        "MolecularFormula",
        "MolecularWeight",
        "InChIKey",
        "IsomericSMILES"
      )
    )
  
  # Obtain compound synonym names based on the Pubchem CID number
  synonym <-
    webchem::pc_synonyms(unique(na.omit(cid$cid)), from = "cid", match = "all")
  # The first element saves as the common name and looks up the CAS number.
  syn_list <- list()
  for (i in seq(1, length(synonym))) {
    syn_list[[i]] <- data.frame(CID = NA,
                                CommonName = NA,
                                PubchemCAS = NA)
    syn_list[[i]]$CID <- names(synonym[i])
    cas_id <-
      which(do.call(data.frame, lapply(synonym[i], function(x)
        return(
          webchem::is.cas(x)
        )))[, 1])
    if (length(cas_id) == 0) {
      next
    } else {
      syn_list[[i]]$PubchemCAS <- synonym[[i]][cas_id[1]]
    }
    syn_list[[i]]$CommonName <- synonym[[i]][-cas_id][1]
  }
  syn <- plyr::ldply(syn_list)
  
  # Merge all information
  info$CID <- as.character(info$CID)
  syn$CID <- as.character(syn$CID)
  cid$cid <- as.character(cid$cid)
  info_syn <- dplyr::left_join(info, syn, by = "CID")
  cid_info_syn <-
    dplyr::left_join(cid , info_syn, by = c("cid" = "CID")) %>%
    unique(.)
  df_compound_info <-
    dplyr::left_join(df , cid_info_syn, by = c("Hitname" = "query"))
  
  # Save results
  openxlsx::write.xlsx(df_compound_info,
                       paste0(compare_result_dir, "/", "compound_info.xlsx"))
  
  return_result$type <- "message"
  return_result$text_message <- "The pubchem function has been run!"
  return(return_result)
}