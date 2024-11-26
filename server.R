# Import necessary functions
source("./core/migration.R")
source("./core/compare_gcms_results.R")
source("./core/fgtk_filter.R")
source("./core/get_pubchem_info.R")
source("./core/get_classyfire_info.R")
source("./core/get_npclassifier_info.R")
source("./core/classification_summaries.R")
source("./core/stat.R")
source("./core/multivariate_data_analysis.R")
source("./core/biomarker.R")
source("./core/plot_venn.R")
source("./core/plot_upset.R")
source("./core/plot_cluster.R")
source("./core/plot_2D_TIC.R")
source("./core/plot_3D_TIC.R")

# Import workflow functions
source("./tab/migration_server.R")
source("./tab/compare_gcms_results_server.R")
source("./tab/filter_server.R")
source("./tab/get_pubchem_info_server.R")
source("./tab/get_classyfire_info_server.R")
source("./tab/get_npclassifier_info_server.R")
source("./tab/classification_summaries_server.R")
source("./tab/stat_server.R")
source("./tab/multivariate_data_analysis_server.R")
source("./tab/biomarker_server.R")

# Import plot function
source("./tab/venn_server.R")
source("./tab/upset_server.R")
source("./tab/cluster_server.R")
source("./tab/2D_TIC_server.R")
source("./tab/3D_TIC_server.R")

function(input, output, session) {
  # migration server
  migration_server(input, output, session)
  # Compare GC-MS results server
  compare_gcms_results_server(input, output, session)
  # filter server
  filter_server(input, output, session)
  # get pubchem informations server
  get_pubchem_info_server(input, output, session)
  # get classyfire informations server
  get_classyfire_info_server(input, output, session)
  # get NP Classifier informations server
  get_npclassifier_info_server(input, output, session)
  # classification summaries server
  classification_summaries_server(input, output, session)
  # stat server
  stat_server(input, output, session)
  # multivariate data analysis server
  multivariate_data_analysis_server(input, output, session)
  # biomarker server
  biomarker_server(input, output, session)
  
  # venn server
  venn_server(input, output, session)
  # upset server
  upset_server(input, output, session)
  # cluster server
  cluster_server(input, output, session)
  # 2D TIC server
  TIC_2D_server(input, output, session)
  # 3D TIC server
  TIC_3D_server(input, output, session)
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}
