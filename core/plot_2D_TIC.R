plot_2D_TIC <- function(curve_dir_path,
                        calculate_percentage = FALSE,
                        line_width = 0.5,
                        font_size = 14,
                        x_offset = 0,
                        y_offset = 0,
                        x_min = NULL,
                        x_max = NULL,
                        y_min = NULL,
                        y_max = NULL) {
  # Import the necessary libraries
  if (!("package:tidyverse" %in% search())) {
    library(tidyverse)
  }
  if (!("package:ggplot2" %in% search())) {
    library(ggplot2)
  }
  
  # Check if the curve directory exists
  if (!exists("curve_dir_path")) {
    stop("The curve directory parameter does not set. Please provide the necessary parameter.")
  } else {
    if (!dir.exists(curve_dir_path)) {
      stop("The curve directory does not exist. Please check the path.")
    }
  }
  
  # Create empty vectors of samples, paths, groups, x_offsets, y_offsets
  samples <- NULL
  paths <- NULL
  groups <- NULL
  x_offsets <- NULL
  y_offsets <- NULL
  
  # Get a list of subdirectories (group names)
  dirs <- list.dirs(curve_dir_path,
                    full.names = F,
                    recursive = F)
  # Get a list of data path, sample name and sample group
  for (dir in dirs) {
    dir_path <- paste0(curve_dir_path, "/", dir)
    files <- list.files(path = dir_path, pattern = "(.CSV)$")
    # Report an error if the CSV file is not detected
    if (length(files) == 0) {
      stop(
        "No .CSV files found in this directory. Please delete the empty folder so that it does",
        "not affect the next calculation."
      )
    }
    samples_paths <- paste0(dir_path, "/", files)
    samples <- append(samples, sub(".CSV$", "", files))
    paths <- append(paths, samples_paths)
    groups <- append(groups, rep(dir, times = length(files)))
    x_offsets <- append(x_offsets, rep(x_offset * (length(unique(
      groups
    )) - 1), times = length(files)))
    y_offsets <- append(y_offsets, rep(y_offset * (length(unique(
      groups
    )) - 1), times = length(files)))
  }
  group_xlsx <- data.frame(
    sample = samples,
    group = groups,
    x_offset = x_offsets,
    y_offset = y_offsets
  )
  
  df_samples <- data.frame()
  ### Read all datas
  for (i in seq(1, length(paths))) {
    # Read a data
    df <- data.frame(All = scan(paths[i], what = "", sep = "\t")[-1:-3]) %>%
      mutate(
        Time = str_sub(All, 1, str_locate(All, ",")[, "start"] - 1),
        Height = str_sub(All, str_locate(All, ",")[, "start"] + 1, -1)
      ) %>%
      select(-1)
    colnames(df)[2] <- samples[i]
    df <- df %>% mutate(across(c(1, 2), as.numeric))
    # Integrate all datas
    if (ncol(df_samples) == 0) {
      df_samples <- df
    } else{
      df_samples <- full_join(df_samples, df, by = "Time")
    }
  }
  
  # Calculate relative abundance
  if (calculate_percentage == TRUE) {
    df_samples <- cbind(df_samples[1], apply(df_samples[-1], 2, function(x) {
      x <- x / max(x, na.rm = TRUE) * 100
    }))
  }
  
  df_melt <- reshape2::melt(df_samples, id = "Time") %>%
    arrange(Time) %>%
    na.omit()
  df_last <- left_join(df_melt, group_xlsx, by = c("variable" = "sample")) %>%
    mutate(time = Time + x_offset, abundance = value + y_offset)
  
  TIC_plot <- ggplot(data = df_last) +
    geom_line(aes(
      x = time,
      y = abundance,
      group = variable,
      color = group
    ),
    linewidth = line_width) +
    scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(df_last$group)), "Set1")) +
    ggprism::theme_prism(
      base_size = font_size,
      base_line_size = 0.3,
      palette = "floral",
      base_family = "serif",
      base_fontface = "plain",
      border = T
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = "inside",
      legend.justification = c("right", "top"),
      legend.text = element_text(hjust = 0),
      # legend.title = element_text(hjust = 0.5),
      legend.background = element_rect(
        colour = "black",
        fill = NA,
        size = 0.3
      )
    )
  if (calculate_percentage == TRUE) {
    TIC_plot <- TIC_plot +
      labs(x = "Retention time (min)", y = "Relative Abundance (%)")
  } else{
    TIC_plot <- TIC_plot +
      labs(x = "Retention time (min)", y = "Abundance")
  }
  if (is.null(x_min) |
      is.null(x_max)) {
    TIC_plot <- TIC_plot + scale_x_continuous(limits = c(floor(min(df_last$time)), ceiling(max(df_last$time))),
                                              expand = c(0, 0))
  } else{
    TIC_plot <- TIC_plot + scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0))
  }
  if (is.null(y_min) | is.null(y_max)) {
    if (calculate_percentage == TRUE) {
      TIC_plot <- TIC_plot + scale_y_continuous(limits = c(floor(min(
        df_last$abundance
      )), ceiling(max(
        df_last$abundance
      ))),
      expand = c(0, 0))
    } else{
      TIC_plot <- TIC_plot + scale_y_continuous(
        limits = c(floor(min(
          df_last$abundance
        )), ceiling(max(
          df_last$abundance
        ))),
        expand = c(0, 0),
        labels = scales::label_scientific(digits = 1)
      )
    }
  } else{
    if (calculate_percentage == TRUE) {
      TIC_plot <- TIC_plot + scale_y_continuous(limits = c(y_min, y_max),
                                                expand = c(0, 0))
    } else{
      TIC_plot <- TIC_plot + scale_y_continuous(
        limits = c(y_min, y_max),
        expand = c(0, 0),
        labels = scales::label_scientific(digits = 1)
      )
    }
  }
  
  result <- list(plot = TIC_plot, df = df_samples)
  return(result)
}