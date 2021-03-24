#' Evaluation Metrics
#'
#' @param model_fit model fit from \code{lcpa}
#' @param new_data data frame
#' @param threshold vector of values to use as threshold cutoffs
#' @export
eval_auc <- function(model_fit, new_data, ...) UseMethod("eval_auc")

#' @export
eval_auc.lcpa_fit <- function(model_fit, new_data, ...) {
  # get response variable
  formula <- model_fit$formula
  y <- response_var_from_data(new_data, formula)

  ind_pos <- which(y > 0)
  ind_neg <- which(y <= 0)

  n_pos <- length(ind_pos)
  n_neg <- length(ind_neg)

  s <- predict.lcpa_fit(model_fit, new_data, type = "score")

  auc <- vapply(ind_pos, function(i) sum(s[i] > s[ind_neg]), integer(1))
  auc <- sum(auc)

  # integer overflow for n_pos*n_neg?
  #auc/(n_pos*n_neg)
  exp(log(auc) - log(n_pos) - log(n_neg))
}

#' @rdname eval_auc
#' @export
#' @importFrom rlang .data
eval_cal <- function(model_fit, new_data, grouped = FALSE, ...) UseMethod("eval_cal")

#' @export
eval_cal.lcpa_fit <- function(model_fit, new_data, grouped = FALSE, ...) {
  score <- predict.lcpa_fit(model_fit, new_data)
  prob <- score_to_prob(score)
  y <- response_var_from_data(new_data, model_fit$formula)

  # determine if we need to bin
  needs_bins <- length(unique(score)) > 10
  if (needs_bins) {
    breaks <- seq(0, 1, 0.1)
    score_breaks <- c(-Inf, -3, -2, -1, 0, 1, 2, 3, Inf)
    #print(breaks)
    #breaks[1] <- score_to_prob(min(score))
    #breaks[length(breaks)] <- score_to_prob(max(score))
    #print(breaks)
  } else {
    breaks <- NULL
  }

  predicted <- score_to_prob(score)
  score_bins <- if (needs_bins) cut(score, score_breaks, right = FALSE) else score

  df <- tibble::tibble("predicted" = predicted, "score_bins" = score_bins, "y" = as.integer(y > 0))
  df <- df %>%
    dplyr::group_by(.data$score_bins) %>%
    dplyr::mutate(
      "observed" = mean(.data$y),
      "cal" = abs(predicted - observed)) %>%
    dplyr::summarize(
      "predicted" = mean(predicted),
      "observed" = mean(observed),
      "cal" = sum(cal),
      "n" = dplyr::n()) %>%
    dplyr::ungroup()

  if (!grouped) {
    df %>%
      dplyr::summarize(cal = sum(cal)/sum(n)) %>%
      dplyr::pull(cal)
  } else {
    #dplyr::ungroup(df)
    df %>%
      dplyr::mutate(cal = cal/n) # cal = abs(predicted - observed)
  }
}


#' @rdname eval_auc
#' @export
eval_precision <- function(model_fit, new_data, threshold = 0.5, ...) UseMethod("eval_precision")

#' @export
eval_precision.lcpa_fit <- function(model_fit, new_data, threshold = 0.5, ...) {
  y_actual <- response_var_from_data(new_data, model_fit$formula)

  probs <- predict.lcpa_fit(model_fit, new_data, type = "response")

  vapply(threshold, function(r) {
    y_est <- 2L*(probs >= r) - 1L

    # precision is "pct correct of the ones you predict to be 1"
    idx <- which(y_est == 1)
    n <- length(idx)

    if (n == 0) return(1)

    n_correct <- sum(y_actual[idx] == 1)
    n_correct/n
  }, double(1))

}


#' @rdname eval_auc
#' @export
eval_recall <- function(model_fit, new_data, threshold = 0.5, ...) UseMethod("eval_recall")

#' @export
eval_recall.lcpa_fit <- function(model_fit, new_data, threshold = 0.5, ...) {
  y_actual <- response_var_from_data(new_data, model_fit$formula)

  probs <- predict.lcpa_fit(model_fit, new_data, type = "response")

  vapply(threshold, function(r) {
    y_est <- 2L*(probs >= r) - 1L

    # recall is "pct of actual 1s you returned as 1"
    idx <- which(y_actual == 1)
    n <- length(idx)
    n_correct <- sum(y_est[idx] == 1)
    n_correct/n
  }, FUN.VALUE = double(1))

}


#' @rdname eval_auc
#' @export
eval_accuracy <- function(model_fit, new_data, threshold = 0.5, ...) UseMethod("eval_accuracy")

#' @export
eval_accuracy.lcpa_fit <- function(model_fit, new_data, threshold = 0.5, ...) {
  y_actual <- response_var_from_data(new_data, model_fit$formula)
  probs <- predict.lcpa_fit(model_fit, new_data, type = "response")

  vapply(threshold, function(r) {
    y_est <- 2L*(probs >= r) - 1L
    mean(y_est == y_actual)
  }, double(1))
}

#' @rdname eval_auc
#' @export
eval_logloss <- function(model_fit, new_data) UseMethod("eval_logloss")

#' @export
eval_logloss.lcpa_fit <- function(model_fit, new_data) {
  score <- predict.lcpa_fit(model_fit, new_data)
  prob <- score_to_prob(score)
  y <- response_var_from_data(new_data, model_fit$formula)

  -mean(ifelse(y > 0, log(prob), log(1- prob)))
}
