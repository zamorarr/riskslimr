plot.lcpa_fit <- function(model_fit, new_data, ...) {
  cal <- eval_cal.lcpa_fit(model_fit, new_data, grouped = TRUE)
  cal_val <- eval_cal.lcpa_fit(model_fit, new_data, grouped = FALSE)
  cal %>%
    ggplot2::ggplot(ggplot2::aes(x = predicted, y = observed)) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = "Risk Calibration",
      subtitle = sprintf("calibration error: %0.3f", cal_val),
      x = "predicted risk",
      y = "observed risk") +
    ggplot2::theme_minimal()
}
