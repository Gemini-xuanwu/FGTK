heatmap_ui <- fluidPage(
  h2("This function is still in beta!!!", style = "color: red;"),
  box(
    title = "Folder path & settings",
    width = NULL,
    status = "info",
    fluidRow(column(
      12,
      # compare_result_dir setting
      directoryInput(
        "heatmap_compare_result_dir",
        label = "compare result folder:",
        value = paste0(dirname("~"), "/", "Desktop")
      )
    )),
    fluidRow(
      column(
        3,
        numericInput(
          "heatmap_cell_width",
          label = "cell width(pt):",
          value = 30,
          min = 10,
          max = 50
        )
      ),
      column(
        3,
        numericInput(
          "heatmap_cell_height",
          label = "cell height(pt):",
          value = 30,
          min = 10,
          max = 50
        )
      ),
      column(
        3,
        checkboxInput("heatmap_rows", "Whether or not to cluster the rows", value = FALSE)
      ),
      column(
        3,
        checkboxInput("heatmap_columns", "whether or not to cluster the columns", value = FALSE)
      )
    ),
    fluidRow(
      column(
        3,
        checkboxInput("heatmap_show_row_tree", "Whether or not to show row tree", value = TRUE)
      ),
      column(
        3,
        checkboxInput(
          "heatmap_show_column_tree",
          "whether or not to show column tree",
          value = TRUE
        )
      ),
      column(
        3,
        selectInput(
          "heatmap_row_tree_side",
          "row tree side:",
          c("left", "right"),
          multiple = FALSE
        )
      ),
      column(
        3,
        selectInput(
          "heatmap_column_tree_side",
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
          "heatmap_row_names_side",
          "row names side:",
          c("left", "right"),
          multiple = FALSE
        )
      ),
      column(
        3,
        selectInput(
          "heatmap_column_names_side",
          "column names side:",
          c("bottom", "top"),
          multiple = FALSE
        )
      ),
      column(
        3,
        numericInput(
          "heatmap_font_size",
          label = "font size:",
          value = 12,
          min = 6,
          max = 18
        )
      ),
      column(
        3,
        numericInput(
          "heatmap_legend_font_size",
          label = "legend font size:",
          value = 12,
          min = 6,
          max = 18
        )
      ),
    ),
    actionButton("plot_heatmap", "Plot", icon = icon("gear", class = "fa-spin"))
  ),
  box(
    title = "Result",
    width = NULL,
    status = "success",
    tabBox(
      height = "100%",
      width = "100%",
      tabPanel(
        "Heatmap diagram",
        plotOutput("heatmap_plot", width = "100%", height = "450px"),
        box(
          title = "Save settings",
          width = NULL,
          status = "warning",
          fluidRow(column(
            6,
            numericInput(
              "heatmap_width",
              label = "heatmap diagram width(cm):",
              value = 7,
              min = 4,
              max = 20
            )
          ), column(
            6,
            numericInput(
              "heatmap_height",
              label = "heatmap diagram height(cm):",
              value = 7,
              min = 4,
              max = 20
            )
          )),
          radioButtons(
            inputId = "heatmap_filetype",
            label = "Choose file type to save:",
            inline = TRUE,
            choices = list("pdf", "png")
          ),
          actionButton(
            "save_heatmap_plot",
            "Save to compare result folder",
            icon = icon("download")
          )
        )
      ),
      tabPanel(
        "Data table",
        div(
          style = 'overflow-x: scroll',
          DT::dataTableOutput("heatmap_data_table", width = "100%")
        ),
        br(),
        actionButton(
          "save_heatmap_df",
          "Save to compare result folder",
          icon = icon("download")
        )
      )
    )
  )
)