#' Custom theme
figure_theme <- theme(# Title
                      plot.title  = element_text(face = "bold", size = 30),
                      # Subtitle
                      plot.subtitle = element_text(size = 24),
                      # Caption
                      plot.caption = element_text(face = "italic"),
                      # Axis-x
                      axis.title.x = element_text(face = "bold", size = 20),
                      axis.text.x = element_text(face = "bold", size = 15),
                      axis.ticks.x = element_line(linewidth = 0.2),
                      axis.line.x = element_line(),
                      # Grid x-axis
                      panel.grid.minor.x = element_line(),
                      panel.grid.major.x = element_line(),
                      # Axis-y
                      axis.title.y = element_text(size = 20),
                      axis.text.y = element_text(size = 15),
                      axis.ticks.y = element_line(linewidth = 0.2),
                      axis.line.y = element_line(),
                      # Grid x-axis
                      panel.grid.minor.y = element_line(),
                      panel.grid.major.y = element_line(),
                      # Legend
                      legend.title = element_text(face = "bold", size = 25),
                      legend.text = element_text(face = "italic", size = 20),
                      legend.box.background = element_rect(),
                      #
                      panel.border = element_blank(),
                      panel.spacing = element_blank(),
                      strip.background = element_rect(colour = "black", fill = "white"),
                      panel.background = element_blank(),
                      strip.text = element_text(angle = 0, face = "bold", size = 15))
