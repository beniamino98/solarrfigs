#' Plot the option price for every month under different measures.
#'
#' @examples
#' payoffs <- SampleData$payoffs
#' fig_option_premium_month(payoffs, type = "scenarios")
#' fig_option_premium_month(payoffs, type = "model")
#' fig_option_premium_month(payoffs, type = "scenarios", put = FALSE)
#' fig_option_premium_month(payoffs, type = "model", put = FALSE)
#' @rdname fig_option_premium_month
#' @name fig_option_premium_month
#' @export
fig_option_premium_month <- function(payoffs, type = "scenarios", put = TRUE, tick = 1, subtitle = NULL){

  type <- match.arg(type, choices = c(scenarios = "scenarios", model = "model"))
  option_type = ifelse(put, "put", "call")

  payoffs <- solarOption_structure(payoffs, type, put)

  payoffs[[option_type]][[type]]$structured$payoff_month %>%
    ggplot()+
    geom_line(aes(Month, tick*premium), size = 1.2)+
    geom_line(aes(Month, tick*premium_Qdw), color = "red", linetype = "dashed")+
    geom_line(aes(Month, tick*premium_Qup), color = "green", linetype = "dashed")+
    geom_line(aes(Month, tick*premium_P), color = "blue")+
    geom_line(aes(Month, tick*premium_Q), color = "orange")+
    geom_line(aes(Month, tick*premium_Qr), color = "magenta")+
    theme_bw()+
    scale_y_continuous(breaks = seq(5, 100, 5), labels = paste0(round(seq(5, 100, 5), 2), " €"))+
    scale_x_continuous(breaks = seq(2, 12, 2), labels = lubridate::month(seq(2, 12, 2), label = TRUE))+
    labs(x = "Month", y = "Option price", subtitle = subtitle)+
    theme(legend.position = "none")
}
