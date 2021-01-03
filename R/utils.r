feature_matrix_from_data <- function(df, formula) {
  x <- model.matrix(formula, df)
  colnames(x)[1] <- "[intercept]" # intercept name
  x
}

response_var_from_data <- function(df, formula) {
  y_name <- response_name_from_formula(formula)
  y <- df[[y_name]]
  stopifnot(all(y %in% c(-1L, 1L)))
  y
}

response_name_from_formula <- function(formula) {
  all.vars(formula)[1]
}

score_to_prob <- function(score) {
  1/(1 + exp(-score))
}

prob_to_score <- function(prob) {
  -log(1/prob - 1)
}

random_logfile <- function() {
  tempfile(pattern = "riskslimr-", fileext = ".log")
}
