cluster_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$cluster_compare_result_dir
               },
               handlerExpr = {
                 if (input$cluster_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "cluster_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "cluster_compare_result_dir", value = path)
                 }
               })
  
  # create a reactiveVal to store result
  cluster_data <- reactiveVal(NULL)
  
  # run plot_cluster function
  observeEvent(input$plot_cluster, {
    cluster_data(NULL)
    
    cluster_result <- plot_cluster(
      compare_result_dir = readDirectoryInput(session, "cluster_compare_result_dir"),
      type = input$cluster_type,
      corr_method = input$cluster_corr_method,
      cell_width = input$cluster_cell_width,
      cell_height = input$cluster_cell_height,
      row_names_side = input$cluster_row_names_side,
      column_names_side = input$cluster_column_names_side,
      cluster_rows = input$cluster_rows,
      cluster_columns = input$cluster_columns,
      show_row_tree = input$cluster_show_row_tree,
      show_column_tree = input$cluster_show_column_tree,
      row_tree_side = input$cluster_row_tree_side,
      column_tree_side = input$cluster_column_tree_side,
      legend_side = input$cluster_legend_side,
      legend_direction = input$cluster_legend_direction,
      legend_height = input$cluster_legend_height,
      legend_title_position = input$cluster_legend_title_position,
      font_size = input$cluster_font_size,
      legend_font_size = input$cluster_legend_font_size,
      label_font_size = input$cluster_label_font_size,
      show_label = input$cluster_show_label
    )
    
    cluster_data(cluster_result)
    
    output$cluster_plot <- renderPlot({
      cluster_data()$plot
    })
    
    output$cluster_data_table <- DT::renderDT({
      DT::datatable(cluster_data()$df)
    })
  })
  
  # Save cluster Diagram to compare result folder
  observeEvent(input$save_cluster_plot, {
    # Make sure no graphics devices are turned on
    while (dev.cur() > 1) {
      dev.off()
    }
    
    if (input$cluster_filetype == "pdf") {
      pdf(
        paste0(
          readDirectoryInput(session, "cluster_compare_result_dir"),
          "/cluster-",
          input$cluster_type,
          "-",
          input$cluster_corr_method,
          ".pdf"
        ),
        width = input$cluster_width,
        height = input$cluster_height
      )
    }
    else if (input$cluster_filetype == "png") {
      png(
        paste0(
          readDirectoryInput(session, "cluster_compare_result_dir"),
          "/cluster-",
          input$cluster_type,
          "-",
          input$cluster_corr_method,
          ".png"
        ),
        res = 600,
        units = "cm",
        width = input$cluster_width,
        height = input$cluster_height
      )
    }
    else{
      pdf(
        paste0(
          readDirectoryInput(session, "cluster_compare_result_dir"),
          "/cluster-",
          input$cluster_type,
          "-",
          input$cluster_corr_method,
          ".pdf"
        ),
        width = input$cluster_width,
        height = input$cluster_height
      )
    }
    ComplexHeatmap::draw(cluster_data()$plot)
    dev.off()
  })
  
  observeEvent(input$save_cluster_df, {
    openxlsx::write.xlsx(
      cluster_data()$df,
      paste0(
        readDirectoryInput(session, "cluster_compare_result_dir"),
        "/cluster-",
        input$cluster_type,
        "-",
        input$cluster_corr_method,
        ".xlsx"
      )
    )
  })
}