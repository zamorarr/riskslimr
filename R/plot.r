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

  # precision-recall plot
  thresholds <- seq(0, 1, 0.1)
  df_prec_rec <- tibble::tibble(
    threshold = thresholds,
    precision = eval_precision(r, new_data, thresholds),
    recall = eval_recall(r, new_data, thresholds)
    )

  p2 <- ggplot2::ggplot(df_prec_rec, ggplot2::aes(x = precision, y = recall, color = thresholds)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_color_viridis_c("threshold") +
    ggplot2::labs(title = "Precision vs Recall", subtitle = sprintf("auc: %0.3f", auc_val)) +
    ggplot2::theme_minimal()

  # prediction plot
  y_name <- response_name_from_formula(r$formula)
  p3 <- augment.lcpa_fit(r, new_data) %>%
    ggplot2::ggplot(ggplot2::aes(x = score)) +
    ggplot2::geom_histogram(
      ggplot2::aes(fill = factor(.data[[y_name]])),
      bins = 30,
      color = "white",
      position = "identity",
      #position = "dodge2",
      alpha = 0.5) +
    ggplot2::scale_fill_discrete(y_name) +
    ggplot2::labs(title = "Score Distribution") +
    ggplot2::theme_minimal()

  # return plots
  gridExtra::grid.arrange(p1, p2, p3)
}
