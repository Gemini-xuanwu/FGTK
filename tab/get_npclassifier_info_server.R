get_npclassifier_info_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$npclassifier_compare_result_dir
               },
               handlerExpr = {
                 if (input$npclassifier_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "npclassifier_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "npclassifier_compare_result_dir", value = path)
                 }
               })
  
  # Create a list of stored notifications
  npclassifier_notification_list <- list()
  
  # run get_npclassifier_info function
  observeEvent(input$run_get_npclassifier_info, {
    # Show function starts running
    npclassifier_notification_start <- showNotification(
      "The NP Classifier function starts running!",
      type = "default",
      duration = 10
    )
    npclassifier_notification_list <<- c(npclassifier_notification_list,
                                         npclassifier_notification_start)
  })
  
  # run get_npclassifier_info function
  observeEvent(input$run_get_npclassifier_info, {
    get_npclassifier_info_result <- get_npclassifier_info(compare_result_dir =
                                                            readDirectoryInput(session, "npclassifier_compare_result_dir"))
    
    # Show function completion and error messages
    npclassifier_notification_return_result <- showNotification(
      get_npclassifier_info_result$text_message,
      type = get_npclassifier_info_result$type,
      duration = 10
    )
    npclassifier_notification_list <<- c(npclassifier_notification_list,
                                         npclassifier_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_npclassifier_notifications, {
    # Remove notification messages one by one
    for (notification_alone in npclassifier_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    npclassifier_notification_list <<- list()
  })
}