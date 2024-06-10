compare_gcms_results_server <- function(input, output, session) {
  # input_dir_path setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$input_dir_path
               },
               handlerExpr = {
                 if (input$input_dir_path > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "input_dir_path"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "input_dir_path", value = path)
                 }
               })
  # output_dir_path setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$output_dir_path
               },
               handlerExpr = {
                 if (input$output_dir_path > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "output_dir_path"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "output_dir_path", value = path)
                 }
               })
  
  # Create a list of stored notifications
  compare_notification_list <- list()
  
  # run compare_gcms_results function
  observeEvent(input$run_compare_gcms_results, {
    # Show function starts running
    compare_notification_start <- showNotification(
      "The compare GC-MS results function starts running!",
      type = "default",
      duration = 10
    )
    compare_notification_list <<- c(compare_notification_list, compare_notification_start)
  })
  
  # run compare_gcms_results function
  observeEvent(input$run_compare_gcms_results, {
    compare_result <- compare_gcms_results(
      input_dir_path = readDirectoryInput(session, "input_dir_path"),
      output_dir_path = readDirectoryInput(session, "output_dir_path"),
      min_quality = input$min_quality,
      max_rt = input$max_rt,
      within_thr = input$within_thr,
      between_thr = input$between_thr,
      min_comparative_quality = input$min_comparative_quality
    )
    
    # Show function completion and error messages
    compare_notification_return_result <- showNotification(compare_result$text_message,
                                                           type = compare_result$type,
                                                           duration = 10)
    compare_notification_list <<- c(compare_notification_list,
                                    compare_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_compare_notifications, {
    # Remove notification messages one by one
    for (notification_alone in compare_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    compare_notification_list <<- list()
  })
}