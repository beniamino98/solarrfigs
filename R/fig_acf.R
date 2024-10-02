#' Plot the autocorrelation function of the first 3 moments
#'
#' @param ut
#' @param lag_max
#' @param label
#' @param caption
#' @param ci
#' @param ci_color
#'
#' @examples
#' fig_acf(rnorm(5000), label = "\\tilde{Y}_t", ci_color = "black", lag_max = 100)
#'
#' @rdname fig_acf
#' @name fig_acf
#' @export
fig_acf <- function(x, lag_max = 400, label = "x", caption = NULL, ci = 0.05, ci_color = "blue"){

  # residuals
  lag.index <- seq.default(1, lag_max, 1)
  x_breaks <- seq.default(1, lag_max, length.out = 5)

  # Autocorrelation ut
  acf_x <- acf(x, lag.max = lag_max, plot = FALSE)
  acf_x_bounds <- qnorm(1 - ci/2)/sqrt(acf_x$n.used)
  plot_1 <- ggplot() +
    geom_segment(aes(x = lag.index, xend = lag.index,
                     y = acf_x$acf[,,1][-1],
                     yend = 0)) +
    geom_point(aes(x = lag.index, y = acf_x$acf[,,1][-1]), size = 0.2) +
    geom_line(aes(lag.index, 0)) +
    geom_line(aes(lag.index, acf_x_bounds), color = ci_color,
              linetype = "dashed") +
    geom_line(aes(lag.index, -acf_x_bounds), color = ci_color,
              linetype = "dashed") +
    scale_x_continuous(breaks = x_breaks,
                       labels = round(x_breaks)) +
    labs(x = NULL, y = TeX(paste0("$acf(", label, ")$"))) +
    theme_bw()

  # Autocorrelation ut^2
  acf_x2 <- acf(x^2, lag.max = lag_max, plot = FALSE)
  acf_x2_bounds <- qnorm(1 - ci/2)/sqrt(acf_x2$n.used)
  plot_2 <- ggplot() +
    geom_segment(aes(x = lag.index, xend = lag.index,
                     y = acf_x2$acf[,,1][-1], yend = 0)) +
    geom_point(aes(x = lag.index, y = acf_x2$acf[,,1][-1]), size = 0.2) +
    geom_line(aes(lag.index, 0)) +
    geom_line(aes(lag.index, acf_x2_bounds), color = ci_color,
              linetype = "dashed") +
    geom_line(aes(lag.index, -acf_x2_bounds), color = ci_color,
              linetype = "dashed") +
    scale_x_continuous(breaks = x_breaks,
                       labels = round(x_breaks)) +
    labs(x = NULL, y = TeX(paste0("$acf(", label, "^{2})$"))) +
    theme_bw()

  # Autocorrelation ut^3
  acf_x3 <- acf(x^3, lag.max = lag_max, plot = FALSE)
  acf_x3_bounds <- qnorm(1 - ci/2)/sqrt(acf_x3$n.used)
  plot_3 <- ggplot() +
    geom_segment(aes(x = lag.index, xend = lag.index,
                     y = acf_x3$acf[,,1][-1], yend = 0)) +
    geom_point(aes(x = lag.index, y = acf_x3$acf[,,1][-1]), size = 0.2) +
    geom_line(aes(lag.index, 0)) +
    geom_line(aes(lag.index, acf_x3_bounds), color = ci_color,
              linetype = "dashed") +
    geom_line(aes(lag.index, -acf_x3_bounds), color = ci_color,
              linetype = "dashed") +
    scale_x_continuous(breaks = x_breaks,
                       labels = round(x_breaks)) +
    labs(x = NULL, y = TeX(paste0("$acf(", label, "^{3})$")), caption = caption) +
    theme_bw()

  gridExtra::grid.arrange(plot_1, plot_2, plot_3, ncol = 1)
}

