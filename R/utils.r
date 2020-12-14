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
