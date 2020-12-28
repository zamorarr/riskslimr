#lcpa <- function(x, y, R_max = 3L) {
lcpa <- function(df, formula, R_max = 3L) {
  # add intercept to x
  #x <- as.matrix(x)
  #x <- cbind(1, x)

  x <- model.matrix(formula, df)
  colnames(x)[1] <- ""
  y <- df[[all.vars(formula)[1]]]

  # do lcpa
  r <- lcpa_cpp(x, y, R_max)

  # add solution string
  lambda <- r$lambda
  lambda_name <- colnames(x)
  idx <- which(r$alpha > 0)
  s <- paste(paste(lambda[idx], lambda_name[idx], sep = "*"), collapse = " + ")
  s <- gsub("\\* ", "\\ ", s)
  s <- gsub("\\+ \\-", "\\- ", s)
  r$solution <- s
  r$R <- sum(r$alpha)
  r$R_max <- R_max


  # return data frame
  r$alpha <- list(r$alpha)
  r$lambda <- list(r$lambda)
  tibble::as_tibble(r)
}

lcpa_cv <- function(df, formula, R_max, v = 5) {
  folds <- rsample::vfold_cv(df, v)
  purrr::map_dfr(folds$splits, function(split) {
    fold_train <- rsample::analysis(split)
    fold_test <- rsample::assessment(split)

    # run lcpa
    r <- lcpa(fold_train, formula, R_max)

    # add auc eval
    r$auc_train <- predict_auc3(r$lambda[[1]], fold_train, formula)
    r$auc_test <- predict_auc3(r$lambda[[1]], fold_test, formula)

    # return data frame
    r
  })
}
