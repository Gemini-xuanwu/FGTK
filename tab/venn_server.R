venn_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$venn_compare_result_dir
               },
               handlerExpr = {
                 if (input$venn_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "venn_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "venn_compare_result_dir", value = path)
                 }
               })
  
  # run plot_venn function
  observeEvent(input$plot_venn, {
    output$venn_plot <- renderPlot({
      plot_venn(
        compare_result_dir = readDirectoryInput(session, "venn_compare_result_dir"),
        group_label_font_size = input$group_label_font_size,
        number_label_font_size = input$number_label_font_size
      )
    })
  })
  
  # Save venn Diagram to compare result folder
  observeEvent(input$save_venn, {
    plot_venn(
      compare_result_dir = readDirectoryInput(session, "venn_compare_result_dir"),
      group_label_font_size = input$group_label_font_size,
      number_label_font_size = input$number_label_font_size
    )
    if (input$venn_filetype == "pdf")
      ggsave(paste0(
        readDirectoryInput(session, "venn_compare_result_dir"),
        "/venn.pdf"
      ))
    else if (input$venn_filetype == "png")
      ggsave(paste0(
        readDirectoryInput(session, "venn_compare_result_dir"),
        "/venn.png"
      ), dpi = 600)
    else
      ggsave(paste0(
        readDirectoryInput(session, "venn_compare_result_dir"),
        "/venn.pdf"
      ))
  })
}