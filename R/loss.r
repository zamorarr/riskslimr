loss_generator <- function(x, y) {
  # should we normalize x and y?

  # note y is {-1,1} not {0,1} as usual
  # https://stats.stackexchange.com/questions/286516/the-correct-loss-function-for-logistic-regression

  # compute intermediate variable
  #z  <- x * y # element-wise with recycling { n x d}
  z <- matrix(as.vector(x) * as.vector(y), nrow = nrow(x))
  d <- ncol(x)

  # define compute loss function
  compute_loss <- function(lambda) {
    stopifnot(identical(length(lambda), d))
    mean(log(1 + exp(-z %*% lambda)))
  }

  # return function
  compute_loss
}

loss_grad_generator <- function(x, y) {
  # should we normalize x and y?

  # compute intermediate variable
  #z <- x * y
  z <- matrix(as.vector(x) * as.vector(y), nrow = nrow(x))
  d <- ncol(x)

  # define compute loss gradient function
  compute_loss_grad <- function(lambda) {
    stopifnot(identical(length(lambda), d))

    a <- -z # {n x d}
    b <- 1 + exp(z %*% lambda) # {n x 1}
    vapply(1:d, function(j) mean(a[,j]/b), double(1))
  }

  # return function
  compute_loss_grad
}
