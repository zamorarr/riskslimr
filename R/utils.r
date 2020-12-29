show_constraints <- function(model) {
  x <- ompr::extract_constraints(model)
  vars <- ompr::variable_keys(model)

  cat("constraints:\n")
  for (i in 1:nrow(x$matrix)) {
    r <- x$matrix[i,]
    ind <- which(r != 0)
    lhs <- paste(sprintf("%s * %s", r[ind], vars[ind]), collapse = " + ")
    lhs <- gsub("\\+ -1 \\*", "-", lhs)
    lhs <- gsub("1 \\* ", "", lhs)

    eqn <- paste(lhs, x$sense[i], x$rhs[i])
    cat("  ", eqn, "\n")
  }
  #x$matrix
  #x$sense
  #x$rhs
}

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
