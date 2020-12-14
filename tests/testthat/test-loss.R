test_that("compute_loss works with one data point", {
  x <- matrix(c(1, 5), nrow = 1)
  y <- matrix(-1, nrow = 1)

  f <- loss_generator(x, y)
  g <- loss_grad_generator(x, y)

  lambda <- c(2, 1)
  expect_equal(f(lambda), log(1 + exp(7)))
  expect_equal(g(lambda), c(1,5)/(1 + exp(-7)))
})


test_that("compute_loss_generator works", {
  x <- seq(-5, 5, by = 0.01)
  y <- 2 + 5*x + rnorm(length(x), 0, 1)
  x <- cbind(1, x)
  y <- matrix(2L*as.integer(y >= 0) - 1L, ncol = 1)

  # trying to find lambda such that the loss function is minimized
  # y_hat ~ x*lambda

  # the optimal value for lambda is 5
  # we expect f to have a minimum at 5 and g to be close to zero at 5
  f <- loss_generator(x, y)
  g <- loss_grad_generator(x, y)

  f_actual <- f(c(0,0))
  g_actual <- g(c(0,0))
  f_expected <- log(2)
  g_expected <- unname(apply(x, 2, function(col) -mean(col * y)/2))

  expect_equal(f_actual, f_expected)
  expect_equal(g_actual, g_expected)
})

