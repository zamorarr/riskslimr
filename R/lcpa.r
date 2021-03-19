# follow these conventions:
# https://tidymodels.github.io/model-implementation-principles/index.html

compute_lcpa <- function(x, y, R_max = 3L, time_limit = 60, logfile = random_logfile(), engine = c("cplex", "glpk")) {
  engine <- match.arg(engine)
  # do lcpa
  logfile <- normalizePath(logfile, mustWork = FALSE)
  cat(sprintf("writing solver log to %s\n", logfile))

  if (identical(engine, "cplex")) {
    cat("using cplex\n")
    r <- lcpa_cpp(x, y, logfile, R_max, time_limit)
  } else {
    cat("using glpk\n")
    r <- lcpa_glpk(x, y, R_max, time_limit)
  }


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
lcpa.data.frame <- function(df, formula, R_max = NULL, time_limit = 60, logfile = random_logfile(), engine = c("cplex", "glpk"), ...) {
  engine <- match.arg(engine)

  # build feature matrix
  x <- feature_matrix_from_data(df, formula)

  # build response variable
  y <- response_var_from_data(df, formula)

  # determine R_max if not provided
  if (is.null(R_max)) {
    R_max <- ncol(x)
  } else {
    R_max <- as.integer(R_max)
  }

  # do lcpa
  r <- compute_lcpa(x, y, R_max, time_limit, logfile, engine = engine)

  # add some useful info
  r$formula <- formula
  r$auc_train <- eval_auc.lcpa_fit(r, df)
  r$cal_train <- eval_cal.lcpa_fit(r, df)

  # return result
  r
}

#' @export
print.lcpa_fit <- function(x, ...) {
  cat("<lcpa fit>\n")
  str(x, 1, give.attr = FALSE)
}

#' @export
summary.lcpa_fit <- function(object, ...) {
  # fit variables
  vars <- object$vars
  vars_intercept <- "[intercept]"
  vars_no_intercept <- vars[vars != vars_intercept]

  lambda <- object$lambda
  lambda_intercept <- lambda[vars == vars_intercept]
  if (length(lambda_intercept) == 0) {
    lambda_intercept <- 0
  }

  lambda_no_intercept <- lambda[vars != vars_intercept]

  ord <- order(lambda_no_intercept, decreasing = TRUE)
  vars_no_intercept <- vars_no_intercept[ord]
  lambda_no_intercept <- lambda_no_intercept[ord]

  padding <- max(nchar(vars_no_intercept)) + 10
  adding <- rep("| + .....", length(vars_no_intercept))
  adding[1] <- "|   ....."

  # features table
  s_vars <- paste(
    stringr::str_pad(vars_no_intercept, padding, "right"),
    stringr::str_pad(lambda_no_intercept, 3),
    adding
  )

  s_break <- paste(rep("-", nchar(s_vars[1])), collapse = "")
  s_vars <- paste(s_vars, collapse = "\n")
  s_total = paste(
    stringr::str_pad("TOTAL", padding, "right"),
    "    | = ....."
  )

  # risk table
  score_seq <- -3:3
  scores <- score_seq - lambda_intercept
  probs <- sprintf("%s%%", 100*round(score_to_prob(score_seq),2))
  s_scores <- paste(stringr::str_pad(scores, 4, "both"), collapse = "|")
  s_probs <- paste(stringr::str_pad(probs, 4, "both"), collapse = "|")

  # combine and print
  s <- paste(s_vars, s_break, s_total, "", "Risk Table:", s_scores, s_probs, sep = "\n")
  cat(s)
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
