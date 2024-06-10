upset_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$upset_compare_result_dir
               },
               handlerExpr = {
                 if (input$upset_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "upset_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "upset_compare_result_dir", value = path)
                 }
               })
  
  # run plot_upset function
  observeEvent(input$plot_upset, {
    output$upset_plot <- renderPlot({
      plot_upset(
        compare_result_dir = readDirectoryInput(session, "upset_compare_result_dir"),
        max_intersections = input$max_intersections,
        set_size_position = input$set_size_position,
        intersection_matrix_height_ratio = input$intersection_matrix_height_ratio,
        set_size_width_ratio = input$set_size_width_ratio,
        sort_intersections_by = input$sort_intersections_by,
        sort_intersections = input$sort_intersections,
        font_size = input$upset_font_size,
        annotate_font_size = input$upset_annotate_font_size
      )
    })
  })
  
  # Save upset Diagram to compare result folder
  observeEvent(input$save_upset, {
    plot_upset(
      compare_result_dir = readDirectoryInput(session, "upset_compare_result_dir"),
      max_intersections = input$max_intersections,
      set_size_position = input$set_size_position,
      intersection_matrix_height_ratio = input$intersection_matrix_height_ratio,
      set_size_width_ratio = input$set_size_width_ratio,
      sort_intersections_by = input$sort_intersections_by,
      sort_intersections = input$sort_intersections,
      font_size = input$upset_font_size,
      annotate_font_size = input$upset_annotate_font_size
    )
    if (input$upset_filetype == "pdf") {
      ggsave(
        paste0(
          readDirectoryInput(session, "upset_compare_result_dir"),
          "/upset.pdf"
        ),
        width = input$upset_width,
        height = input$upset_height
      )
    }
    else if (input$upset_filetype == "png") {
      ggsave(
        paste0(
          readDirectoryInput(session, "upset_compare_result_dir"),
          "/upset.png"
        ),
        dpi = 600,
        width = input$upset_width,
        height = input$upset_height
      )
    }
    else{
      ggsave(
        paste0(
          readDirectoryInput(session, "upset_compare_result_dir"),
          "/upset.pdf"
        ),
        width = input$upset_width,
        height = input$upset_height
      )
    }
  })
}