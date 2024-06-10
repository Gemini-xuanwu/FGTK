migration_server <- function(input, output, session) {
  # migration_raw_dir_path setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$migration_raw_dir_path
               },
               handlerExpr = {
                 if (input$migration_raw_dir_path > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "migration_raw_dir_path"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "migration_raw_dir_path", value = path)
                 }
               })
  
  # migration_data_dir_path setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$migration_data_dir_path
               },
               handlerExpr = {
                 if (input$migration_data_dir_path > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "migration_data_dir_path"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "migration_data_dir_path", value = path)
                 }
               })
  
  # Create a list of stored notifications
  migration_notification_list <- list()
  
  # run migration function
  observeEvent(input$run_migration, {
    # Show function starts running
    migration_notification_start <- showNotification("The migration function starts running!",
                                                     type = "default",
                                                     duration = 10)
    migration_notification_list <<- c(migration_notification_list,
                                      migration_notification_start)
  })
  
  # run migration function
  observeEvent(input$run_migration, {
    migration_result <- migration(
      raw_dir_path = readDirectoryInput(session, "migration_raw_dir_path"),
      data_dir_path = readDirectoryInput(session, "migration_data_dir_path"),
      file_type = input$migration_file_type
    )
    
    # Show function completion and error messages
    migration_notification_return_result <- showNotification(migration_result$text_message,
                                                             type = migration_result$type,
                                                             duration = 10)
    migration_notification_list <<- c(migration_notification_list,
                                      migration_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_migration_notifications, {
    # Remove notification messages one by one
    for (notification_alone in migration_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    migration_notification_list <<- list()
  })
}