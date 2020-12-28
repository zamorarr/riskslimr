predict_score <- function(lambda, x) {
  as.vector(x %*% lambda)
}

predict_auc <- function(lambda, x, y) {
  ind_pos <- which(y > 0)
  ind_neg <- which(y <= 0)

  n_pos <- length(ind_pos)
  n_neg <- length(ind_neg)

  s <- predict_score(lambda, x)

  auc <- vapply(ind_pos, function(i) sum(s[i] > s[ind_neg]), integer(1))
  auc <- sum(auc)
  auc/(n_pos*n_neg)
}

predict_auc2 <- function(s, y) {
  ind_pos <- which(y > 0)
  ind_neg <- which(y <= 0)

  n_pos <- length(ind_pos)
  n_neg <- length(ind_neg)

  auc <- vapply(ind_pos, function(i) sum(s[i] > s[ind_neg]), integer(1))
  auc <- sum(auc)
  auc/(n_pos*n_neg)
}

predict_auc3 <- function(lambda, df, formula) {
  x <- model.matrix(formula, df)
  colnames(x)[1] <- ""
  y <- df[[all.vars(formula)[1]]]
  predict_auc(lambda, x, y)
}

predict_risk <- function(lambda, x) {
  score <- predict_score(lambda, x)
  u <- 1 + exp(-score)
  as.vector(1/u)
}

eval_calibration <- function(lambda, x, y, has_plot = TRUE) {
  score <- predict_score(lambda, x)

  # determine if we need to bin
  needs_bins <- length(unique(score)) > 10
  if (needs_bins) {
    breaks <- seq(0, 1, 0.1)
  } else {
    breaks <- NULL
  }

  df <- tibble::tibble(
    predicted = predict_risk(lambda, x),
    predicted_bins = if (needs_bins) cut(predicted, breaks) else score,
    y = as.integer(y[,1] > 0)
    ) %>%
    dplyr::group_by(predicted_bins) %>%
    dplyr::mutate(observed = mean(y), cal = abs(predicted - observed))

  if (has_plot) {
    p <- df %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = predicted, y = observed)) +
      ggplot2::geom_abline(linetype = "dashed") +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(x = "predicted risk", y = "observed risk") +
      ggplot2::theme_minimal()

    print(p)
  }

  df %>%
    dplyr::summarize(cal = sum(cal), n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(cal = sum(cal)/sum(n)) %>%
    dplyr::pull(cal)
}
