# follow these conventions:
# https://tidymodels.github.io/model-implementation-principles/index.html

compute_lcpa <- function(x, y, R_max = 3L) {
  # do lcpa
  r <- lcpa_cpp(x, y, R_max)

  # add solution string
  lambda <- r$lambda
  lambda_name <- colnames(x)
  idx <- which(r$alpha > 0 & abs(r$lambda) > 1E-6)

  lambda <- lambda[idx]
  lambda_name <- lambda_name[idx]

  s <- paste(paste(lambda, lambda_name, sep = "*"), collapse = " + ")
  s <- gsub("\\* ", "\\ ", s)
  s <- gsub("\\+ \\-", "\\- ", s)
  s <- gsub("\\*\\[intercept\\]", "", s)
  r$solution <- s

  # more stuff
  r$R <- length(idx)
  r$R_max <- R_max
  r$lambda <- lambda
  r$vars <- lambda_name
  r$alpha <- NULL

  # return data frame
  #r <- tibble::as_tibble(r)
  structure(r, class = c("lcpa_fit", class(r)))
}

#' Lattice Cutting Plane Algorithm
#' @export
lcpa <- function(x, ...) UseMethod("lcpa")

#' @rdname lcpa
#' @export
lcpa.default <- function(x, ...) {
  stop("lcpa only implemented for data frames ")
}

#' @rdname lcpa
#' @export
lcpa.data.frame <- function(df, formula, R_max = NULL, ...) {
  # build feature matrix
  x <- feature_matrix_from_data(df, formula)

  # build response variable
  y <- response_var_from_data(df, formula)

  # determine R_max if not provided
  if (is.null(R_max)) {
    R_max <- ncols(x)
  }

  # do lcpa
  r <- compute_lcpa(x, y, R_max)

  # add some useful info
  r$formula <- formula
  r$auc_train <- eval_auc.lcpa_fit(r, df)
  r$cal_train <- eval_cal.lcpa_fit(r, df)

  # return result
  r
}

print.lcpa_fit <- function(x, ...) {
  cat("<lcpa fit>\n")
  str(x, 1, give.attr = FALSE)
}

lcpa_cv <- function(df, formula, R_max, v = 5) {
  folds <- rsample::vfold_cv(df, v)
  purrr::map(folds$splits, function(split) {
    fold_train <- rsample::analysis(split)
    fold_test <- rsample::assessment(split)

    # run lcpa
    r <- lcpa(fold_train, formula, R_max)

    # add auc eval
    r$auc_test <- eval_auc(r, fold_test)
    r$cal_test <- eval_cal(r, fold_test)

    # return data frame
    r
  })
}
