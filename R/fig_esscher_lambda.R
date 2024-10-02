#' @examples
#' fig_esscher_lambda(esscher_h, solarr::CAMS_data)
fig_esscher_lambda <- function(esscher_h, CAMS_data){
  bind_rows(CAMS_data) %>%
    select(place, lat) %>%
    unique() %>%
    left_join(bind_cols(place = names(esscher_h), bind_rows(esscher_h)), by = "place") %>%
    arrange(desc(lambda)) %>%
    ggplot()+
    geom_smooth(aes(lat, lambda), se = FALSE, method = "lm")+
    geom_point(aes(lat, lambda), size = 4) +
    theme_bw()+
    labs(x = "Latitude", y = TeX("$\\theta$"))+
    theme(legend.position = "none",
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.subtitle = element_text(size = 24),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15, angle = 0),
          axis.ticks = element_line(size = 0.4),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "black", linetype = "dashed", linewidth = 0.1),
          panel.grid.major.x = element_blank())
}

#fig <- fig_esscher_lambda(esscher_h, solarr::CAMS_data)
#ggsave(filename = paste0("/Users/macbook/Desktop/project-solar 2/paper/fig/", "fig-esscher-lambda", ".png"),
#       plot = fig, dpi = 300, width = 15, height = 10)
