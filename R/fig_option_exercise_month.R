#' Figure with monthly probability of exercise under different measures
#'
#' @param payoff object of the class `solarOptionPayoff`
#' @param type character, type of computation. Can be `model` or `scenarios`.
#' @param put logical, when `TRUE` the exercise probability refers to a PUT contract otherwise to a CALL.
#' @param limits numeric vector, limits for y-axis.
#' @param subtitle character, plot subtitle.
#' @examples
#' payoffs <- SampleData$payoffs
#' type = "model"
#' put = TRUE
#' limits = c(0.27, 0.58)
#' subtitle = NULL
#' fig_option_exercise_month(payoffs, type = "scenarios", put = TRUE, limits = c(0.27, 0.68), subtitle = NULL)
#' fig_option_exercise_month(payoffs, type = "model", put = TRUE, limits = c(0.27, 0.58), subtitle = NULL)
#' fig_option_exercise_month(payoffs, type = "scenarios", put = FALSE, limits = c(0.27, 0.78), subtitle = NULL)
#' fig_option_exercise_month(payoffs, type = "model", put = FALSE, limits = c(0.27, 0.78), subtitle = NULL)
#' @rdname fig_option_exercise_month
#' @examples
fig_option_exercise_month <- function(payoff, type = "scenarios", put = TRUE, limits = c(0.2, 0.65), subtitle = NULL){

  # ****************************************************************************
  # Match type of option
  option_type <- ifelse(put, "put", "call")
  # Match type of computations
  type <- match.arg(type, choices = c(scenarios = "scenarios", model = "model"))
  # Structure the payoff
  payoff <- solarOption_structure(payoff, type = type, put = put)
  # Dataset used for creating the figure
  df_plot <- payoff[[option_type]][[type]]$structured$payoff_month
  # ****************************************************************************
  # Figure
  df_plot %>%
    ggplot()+
    geom_line(aes(Month, exercise), size = 1.2)+
    geom_line(aes(Month, exercise_P), color = "blue")+
    geom_line(aes(Month, exercise_Q), color = "orange")+
    # geom_line(aes(Month, exercise_Qr), color = "magenta")+
    geom_line(aes(Month, exercise_Qdw), color = "red", linetype = "dashed")+
    geom_line(aes(Month, exercise_Qup), color = "green", linetype = "dashed")+
    theme_bw()+
    scale_x_continuous(breaks = seq(2, 12, 2), labels = lubridate::month(seq(2, 12, 2), label = TRUE))+
    scale_y_continuous(breaks = seq(0.1, 0.9, 0.05), limits = limits,
                       labels = paste0(round(seq(0.1, 0.9, 0.05)*100, 2), "%"))+
    labs(x = "Month", y = "Probability of exercise", subtitle = subtitle)+
    theme(legend.position = "none")
}

