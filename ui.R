# How to import packages listed in req.txt
req <- scan(file.path(dirname(getwd()), "req.txt"), character(), quiet = T)
invisible(lapply(req, library, character.only = T))

# Import necessary packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyDirectoryInput)

# Import workflow functions
source("./tab/migration_ui.R")
source("./tab/compare_gcms_results_ui.R")
source("./tab/filter_ui.R")
source("./tab/get_pubchem_info_ui.R")
source("./tab/get_classyfire_info_ui.R")
source("./tab/get_npclassifier_info_ui.R")
source("./tab/classification_summaries_ui.R")
source("./tab/stat_ui.R")
source("./tab/multivariate_data_analysis_ui.R")
source("./tab/biomarker_ui.R")

# Import plot function
source("./tab/venn_ui.R")
source("./tab/upset_ui.R")
source("./tab/cluster_ui.R")
source("./tab/2D_TIC_ui.R")
source("./tab/3D_TIC_ui.R")

# Header setting
header <-
  dashboardHeader(title = "FGTK", titleWidth = 210)

# Sidebar setting
sidebar <- dashboardSidebar(
  width = 210,
  minified = FALSE,
  sidebarMenu(
    # Searching box
    # sidebarSearchForm(
    #     textId = "searchText",
    #     buttonId = "searchButton",
    #     label = "Search..."
    # ),
    # Readme
    menuItem(
      "Readme",
      tabName = "readme",
      icon = icon("readme"),
      badgeLabel = "Read it !!!",
      badgeColor = "green"
    ),
    # Workflows
    menuItem(
      "Workflows",
      icon = icon("dragon"),
      menuSubItem(
        "Migration",
        tabName = "migration",
        icon = icon("file-import")
      ),
      menuSubItem(
        "Compare GC-MS results",
        tabName = "compare_gcms_results",
        icon = icon("code-compare")
      ),
      menuSubItem("Filter", tabName = "filter", icon = icon("filter")),
      menuSubItem("Pubchem", tabName = "get_pubchem_info", icon = icon("atom")),
      menuSubItem(
        "Classyfire",
        tabName = "get_classyfire_info",
        icon = icon("code-branch")
      ),
      menuSubItem(
        "NP Classifier",
        tabName = "get_npclassifier_info",
        icon = icon("code-branch")
      ),
      menuSubItem(
        "Classification summaries",
        tabName = "classification_summaries",
        icon = icon("chart-pie")
      ),
      menuSubItem(
        "Stat",
        tabName = "stat",
        icon = icon("arrow-down-a-z")
      ),
      menuSubItem(
        "Multivariate data analysis",
        tabName = "multivariate_data_analysis",
        icon = icon("circle-half-stroke")
      ),
      menuSubItem(
        "Biomarker",
        tabName = "biomarker",
        icon = icon("ranking-star")
      )
    ),
    # Plots
    menuItem(
      "Plots",
      tabName = "plots",
      icon = icon("pencil"),
      menuSubItem("Venn", tabName = "venn", icon = icon("clone")),
      menuSubItem("Upset", tabName = "upset", icon = icon("chart-simple")),
      menuSubItem(
        "Cluster",
        tabName = "cluster",
        icon = icon("table-cells")
      ),
      menuSubItem("2D TIC", tabName = "2D_TIC", icon = icon("2")),
      menuSubItem("3D TIC", tabName = "3D_TIC", icon = icon("3"))
    )
  )
)

# Ensure that path is accessible in the application
shiny::addResourcePath("html", "./readme/")

# Tab content setting
body <- dashboardBody(
  # Set font family
  # tags$head(tags$style(
  #     HTML('
  #     body {
  #       font-family: "Times New Roman", sans-serif;
  #     }
  #   ')
  # )),
  tabItems(
    # Readme ui
    tabItem(tabName = "readme", #includeHTML("html/readme.html")
            fluidPage(
              tags$iframe(
                src = "html/readme.html",
                width = "100%",
                height = "750px"
              )
            )),
    # Migration ui
    tabItem(tabName = "migration", migration_ui),
    # Compare GC-MS results ui
    tabItem(tabName = "compare_gcms_results", compare_gcms_results_ui),
    # filter ui
    tabItem(tabName = "filter", filter_ui),
    # get pubchem informations ui
    tabItem(tabName = "get_pubchem_info", get_pubchem_info_ui),
    # get classyfire informations ui
    tabItem(tabName = "get_classyfire_info", get_classyfire_info_ui),
    # get NP Classifier informations ui
    tabItem(tabName = "get_npclassifier_info", get_npclassifier_info_ui),
    # classification summaries ui
    tabItem(tabName = "classification_summaries", classification_summaries_ui),
    # stat ui
    tabItem(tabName = "stat", stat_ui),
    # multivariate data analysis ui
    tabItem(tabName = "multivariate_data_analysis", multivariate_data_analysis_ui),
    # biomarker ui
    tabItem(tabName = "biomarker", biomarker_ui),
    # venn ui
    tabItem(tabName = "venn", venn_ui),
    # upset ui
    tabItem(tabName = "upset", upset_ui),
    # cluster ui
    tabItem(tabName = "cluster", cluster_ui),
    # 2D TIC ui
    tabItem(tabName = "2D_TIC", TIC_2D_ui),
    # 3D TIC ui
    tabItem(tabName = "3D_TIC", TIC_3D_ui)
  )
)


# Run app
dashboardPage(
  header,
  sidebar,
  body,
  controlbar = dashboardControlbar(collapsed = TRUE, skinSelector()),
  title = "FGTK"
)