#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment

#' @importFrom generics tidy
#' @export
generics::tidy

#' @export
tidy.lcpa_fit <- function(x, ...) {
  vars <- x$vars
  lambda <- x$lambda
  tibble::tibble(var = vars, lambda = lambda)
}


#' @export
glance.lcpa_fit <- function(x, ...) {
  tibble::tibble(
    objective_value = x$objective_value,
    auc_train = x$auc_train,
    cal_train = x$cal_train,
    auc_test = if (is.null(x$auc_test)) NA_real_ else x$auc_test,
    cal_test = if (is.null(x$cal_test)) NA_real_ else x$cal_test,
    R = x$R,
    R_max = x$R_max,
    solution = x$solution
  )
}

#' @export
augment.lcpa_fit <- function(x, new_data, ...) {
  results <- tibble::tibble(
    score = predict.lcpa_fit(x, new_data, type = "score"),
    prob = 1/(1 + exp(-score))
  )

  dplyr::bind_cols(
    new_data,
    results
  )
  #results
}
