cluster_ui <- fluidPage(
  h2("This function is still in beta!!!", style = "color: red;"),
  box(
    title = "Folder path & settings",
    width = NULL,
    status = "info",
    fluidRow(column(
      12,
      # compare_result_dir setting
      directoryInput(
        "cluster_compare_result_dir",
        label = "compare result folder:",
        value = paste0(dirname("~"), "/", "Desktop")
      )
    )),
    fluidRow(
      column(3, selectInput(
        "cluster_type", "type:", c("sample", "compound"), multiple = FALSE
      )),
      column(
        3,
        selectInput(
          "cluster_corr_method",
          "correlation method:",
          c("pearson", "spearman"),
          multiple = FALSE
        )
      ),
      column(
        3,
        numericInput(
          "cluster_cell_width",
          label = "cell width(pt):",
          value = 30,
          min = 10,
          max = 50
        )
      ),
      column(
        3,
        numericInput(
          "cluster_cell_height",
          label = "cell height(pt):",
          value = 30,
          min = 10,
          max = 50
        )
      )
    ),
    fluidRow(
      column(
        3,
        selectInput(
          "cluster_row_names_side",
          "row names side:",
          c("right", "left"),
          multiple = FALSE
        )
      ),
      column(
        3,
        selectInput(
          "cluster_column_names_side",
          "column names side:",
          c("bottom", "top"),
          multiple = FALSE
        )
      ),
      column(
        3,
        checkboxInput("cluster_rows", "Whether or not to cluster the rows", value = TRUE)
      ),
      column(
        3,
        checkboxInput("cluster_columns", "whether or not to cluster the columns", value = TRUE)
      )
    ),
    fluidRow(
      column(
        3,
        checkboxInput("cluster_show_row_tree", "Whether or not to show row tree", value = TRUE)
      ),
      column(
        3,
        checkboxInput(
          "cluster_show_column_tree",
          "whether or not to show column tree",
          value = TRUE
        )
      ),
      column(
        3,
        selectInput(
          "cluster_row_tree_side",
          "row tree side:",
          c("left", "right"),
          multiple = FALSE
        )
      ),
      column(
        3,
        selectInput(
          "cluster_column_tree_side",
          "column tree side:",
          c("top", "bottom"),
          multiple = FALSE
        )
      )
    ),
    fluidRow(
      column(
        3,
        selectInput(
          "cluster_legend_side",
          "legend side:",
          c("right", "left", "top", "bottom"),
          multiple = FALSE
        )
      ),
      column(
        3,
        selectInput(
          "cluster_legend_direction",
          "legend direction:",
          c("vertical", "horizontal"),
          multiple = FALSE
        )
      ),
      column(
        3,
        numericInput(
          "cluster_legend_height",
          label = "legend height(cm):",
          value = 4,
          min = 2,
          max = 12
        )
      ),
      column(
        3,
        selectInput(
          "cluster_legend_title_position",
          "legend title position:",
          c("topleft", "topcenter", "leftcenter", "lefttop"),
          multiple = FALSE
        )
      )
    ),
    fluidRow(
      column(
        3,
        numericInput(
          "cluster_font_size",
          label = "font size:",
          value = 12,
          min = 6,
          max = 18
        )
      ),
      column(
        3,
        numericInput(
          "cluster_legend_font_size",
          label = "legend font size:",
          value = 12,
          min = 6,
          max = 18
        )
      ),
      column(
        3,
        numericInput(
          "cluster_label_font_size",
          label = "label font size:",
          value = 10,
          min = 6,
          max = 14
        )
      ),
      column(
        3,
        checkboxInput("cluster_show_label", "whether to show label", value = TRUE)
      )
    ),
    actionButton("plot_cluster", "Plot", icon = icon("gear", class = "fa-spin"))
  ),
  box(
    title = "Result",
    width = NULL,
    status = "success",
    tabBox(
      height = "100%",
      width = "100%",
      tabPanel(
        "Cluster diagram",
        plotOutput("cluster_plot", width = "100%", height = "450px"),
        box(
          title = "Save settings",
          width = NULL,
          status = "warning",
          fluidRow(column(
            6,
            numericInput(
              "cluster_width",
              label = "cluster diagram width(cm):",
              value = 7,
              min = 4,
              max = 20
            )
          ), column(
            6,
            numericInput(
              "cluster_height",
              label = "cluster diagram height(cm):",
              value = 7,
              min = 4,
              max = 20
            )
          )),
          radioButtons(
            inputId = "cluster_filetype",
            label = "Choose file type to save:",
            inline = TRUE,
            choices = list("pdf", "png")
          ),
          actionButton(
            "save_cluster_plot",
            "Save to compare result folder",
            icon = icon("download")
          )
        )
      ),
      tabPanel(
        "Data table",
        div(
          style = 'overflow-x: scroll',
          DT::dataTableOutput("cluster_data_table", width = "100%")
        ),
        br(),
        actionButton(
          "save_cluster_df",
          "Save to compare result folder",
          icon = icon("download")
        )
      )
    )
  )
)