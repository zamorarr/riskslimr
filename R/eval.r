#' @export
eval_auc <- function(model_fit, new_data, ...) UseMethod("eval_auc")
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
  auc/(n_pos*n_neg)
}

#' @export
eval_cal <- function(model_fit, new_data, grouped = FALSE, ...) UseMethod("eval_cal")
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

  df <- tibble::tibble(
    predicted = score_to_prob(score),
    score_bins = if (needs_bins) cut(score, score_breaks, right = FALSE) else score,
    #predicted_bins = if (needs_bins) cut(predicted, breaks) else score,
    #score_bins = if (needs_bins) paste0(
    #  "(",
    #  ceiling(prob_to_score(as.numeric(stringr::str_match(predicted_bins, "^\\((.*),")[,2]))),
    #  ",",
    #  floor(prob_to_score(as.numeric(stringr::str_match(predicted_bins, ",(.*)\\]$")[,2]))),
    #  "]") else score,
    y = as.integer(y > 0)
  ) %>%
    dplyr::group_by(score_bins) %>%
    dplyr::mutate(observed = mean(y), cal = abs(predicted - observed)) %>%
    dplyr::summarize(predicted = mean(predicted), observed = mean(observed), cal = sum(cal), n = dplyr::n()) %>%
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
