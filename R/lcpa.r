lcpa <- function(x, y, R_max = 3L) {
  # add intercept to x
  x <- as.matrix(x)
  x <- cbind(1, x)
  lcpa_cpp(x, y, R_max)
}
