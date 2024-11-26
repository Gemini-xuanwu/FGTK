heatmap_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$heatmap_compare_result_dir
               },
               handlerExpr = {
                 if (input$heatmap_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "heatmap_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "heatmap_compare_result_dir", value = path)
                 }
               })
  
  # create a reactiveVal to store result
  heatmap_data <- reactiveVal(NULL)
  
  # run plot_heatmap function
  observeEvent(input$plot_heatmap, {
    heatmap_data(NULL)
    
    heatmap_result <- plot_heatmap(
      compare_result_dir = readDirectoryInput(session, "heatmap_compare_result_dir"),
      cell_width = input$heatmap_cell_width,
      cell_height = input$heatmap_cell_height,
      cluster_rows = input$heatmap_rows,
      cluster_columns = input$heatmap_columns,
      show_row_tree = input$heatmap_show_row_tree,
      show_column_tree = input$heatmap_show_column_tree,
      row_tree_side = input$heatmap_row_tree_side,
      column_tree_side = input$heatmap_column_tree_side,
      row_names_side = input$heatmap_row_names_side,
      column_names_side = input$heatmap_column_names_side,
      font_size = input$heatmap_font_size,
      legend_font_size = input$heatmap_legend_font_size
    )
    
    heatmap_data(heatmap_result)
    
    output$heatmap_plot <- renderPlot({
      heatmap_data()$plot
    })
    
    output$heatmap_data_table <- DT::renderDT({
      DT::datatable(heatmap_data()$df)
    })
  })
  
  # Save heatmap Diagram to compare result folder
  observeEvent(input$save_heatmap_plot, {
    # Make sure no graphics devices are turned on
    while (dev.cur() > 1) {
      dev.off()
    }
    
    if (input$heatmap_filetype == "pdf") {
      pdf(
        paste0(
          readDirectoryInput(session, "heatmap_compare_result_dir"),
          "/heatmap",
          ".pdf"
        ),
        width = input$heatmap_width,
        height = input$heatmap_height
      )
    }
    else if (input$heatmap_filetype == "png") {
      png(
        paste0(
          readDirectoryInput(session, "heatmap_compare_result_dir"),
          "/heatmap",
          ".png"
        ),
        res = 600,
        units = "cm",
        width = input$heatmap_width,
        height = input$heatmap_height
      )
    }
    else{
      pdf(
        paste0(
          readDirectoryInput(session, "heatmap_compare_result_dir"),
          "/heatmap",
          ".pdf"
        ),
        width = input$heatmap_width,
        height = input$heatmap_height
      )
    }
    ComplexHeatmap::draw(heatmap_data()$plot)
    dev.off()
  })
  
  observeEvent(input$save_heatmap_df, {
    openxlsx::write.xlsx(
      heatmap_data()$df,
      paste0(
        readDirectoryInput(session, "heatmap_compare_result_dir"),
        "/heatmap-z-score",
        ".xlsx"
      )
    )
  })
}