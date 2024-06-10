get_pubchem_info_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$pubchem_compare_result_dir
               },
               handlerExpr = {
                 if (input$pubchem_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "pubchem_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "pubchem_compare_result_dir", value = path)
                 }
               })
  
  # Create a list of stored notifications
  pubchem_notification_list <- list()
  
  # run get_pubchem_info function
  observeEvent(input$run_get_pubchem_info, {
    # Show function starts running
    pubchem_notification_start <- showNotification("The pubchem function starts running!",
                                                   type = "default",
                                                   duration = 10)
    pubchem_notification_list <<- c(pubchem_notification_list, pubchem_notification_start)
  })
  
  # run get_pubchem_info function
  observeEvent(input$run_get_pubchem_info, {
    get_pubchem_info_result <- get_pubchem_info(compare_result_dir = readDirectoryInput(session, "pubchem_compare_result_dir"))
    
    # Show function completion and error messages
    pubchem_notification_return_result <- showNotification(
      get_pubchem_info_result$text_message,
      type = get_pubchem_info_result$type,
      duration = 10
    )
    pubchem_notification_list <<- c(pubchem_notification_list,
                                    pubchem_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_pubchem_notifications, {
    # Remove notification messages one by one
    for (notification_alone in pubchem_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    pubchem_notification_list <<- list()
  })
}