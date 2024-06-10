get_classyfire_info_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$classyfire_compare_result_dir
               },
               handlerExpr = {
                 if (input$classyfire_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "classyfire_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "classyfire_compare_result_dir", value = path)
                 }
               })
  
  # Create a list of stored notifications
  classyfire_notification_list <- list()
  
  # run get_classyfire_info function
  observeEvent(input$run_get_classyfire_info, {
    # Show function starts running
    classyfire_notification_start <- showNotification("The classyfire function starts running!",
                                                      type = "default",
                                                      duration = 10)
    classyfire_notification_list <<- c(classyfire_notification_list,
                                       classyfire_notification_start)
  })
  
  # run get_classyfire_info function
  observeEvent(input$run_get_classyfire_info, {
    get_classyfire_info_result <- get_classyfire_info(compare_result_dir = readDirectoryInput(session, "classyfire_compare_result_dir"))
    
    # Show function completion and error messages
    classyfire_notification_return_result <- showNotification(
      get_classyfire_info_result$text_message,
      type = get_classyfire_info_result$type,
      duration = 10
    )
    classyfire_notification_list <<- c(classyfire_notification_list,
                                       classyfire_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_classyfire_notifications, {
    # Remove notification messages one by one
    for (notification_alone in classyfire_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    classyfire_notification_list <<- list()
  })
}