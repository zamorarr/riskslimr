predict.lcpa_fit <- function(model_fit, new_data, type = c("score", "response"), ...) {
  # type of prediction
  type <- match.arg(type)

  # fit variables
  vars <- model_fit$vars
  vars_intercept <- "[intercept]"
  vars_no_intercept <- vars[vars != vars_intercept]

  lambda <- model_fit$lambda
  lambda_intercept <- lambda[vars == vars_intercept]
  lambda_no_intercept <- lambda[vars != vars_intercept]

  # check that all vars are in new_data
  stopifnot(identical(intersect(vars_no_intercept, colnames(new_data)), vars_no_intercept))

  # get column subset
  new_data <- new_data[vars_no_intercept]

  # calculate score
  if (length(lambda_no_intercept) > 0) {
    score <- mapply(function(a,b) a*b, new_data, lambda_no_intercept, SIMPLIFY = FALSE)
    score <- Reduce(`+`, score, 0)
  } else {
    score <- rep(0L, nrow(new_data))
  }


  # what about intercept?
  has_intercept <- length(lambda_intercept) > 0
  if (has_intercept) score <- score + lambda_intercept

  # return score
  if (identical(type, "response")) {
    score_to_prob(score)
  } else {
    score
  }
}
