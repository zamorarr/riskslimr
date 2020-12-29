#' @export
plot.lcpa_fit <- function(model_fit, new_data, ...) {
  cal <- eval_cal.lcpa_fit(model_fit, new_data, grouped = TRUE)
  cal_val <- eval_cal.lcpa_fit(model_fit, new_data, grouped = FALSE)
  auc_val <- eval_auc.lcpa_fit(model_fit, new_data)

  # calibration plot
  p1 <- cal %>%
    ggplot2::ggplot(ggplot2::aes(x = predicted, y = observed)) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(size = n)) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = "Risk Calibration",
      subtitle = sprintf("calibration error: %0.3f", cal_val),
      x = "predicted risk",
      y = "observed risk") +
    ggplot2::theme_minimal()

  # prediction plot
  y_name <- response_name_from_formula(r$formula)
  p2 <- augment.lcpa_fit(r, new_data) %>%
    ggplot2::ggplot(ggplot2::aes(x = score)) +
    ggplot2::geom_histogram(
      ggplot2::aes(fill = factor(.data[[y_name]])),
      bins = 30,
      color = "white",
      position = "identity",
      #position = "dodge2",
      alpha = 0.5) +
    ggplot2::scale_fill_discrete(y_name) +
    ggplot2::labs(title = "Predictions vs Score", subtitle = sprintf("auc: %0.3f", auc_val)) +
    ggplot2::theme_minimal()

  # return plots
  gridExtra::grid.arrange(p1, p2)
}
