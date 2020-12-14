#' Cutting Plane Algorithm
#'
#' @param x n x d matrix of features
#' @param y n x 1 matrix of classification labels {-1, 1}
#'
#' @importFrom ompr sum_expr add_constraint colwise
cpa <- function(x, y, ...) {

  y <- as.integer(y)
  if (identical(sort(unique(y)), c(0L, 1L))) {
    message("converting {0,1} class labels to {-1,1}\n")
    y[y < 1] <- -1L
  }

  if (!is.matrix(y)) {
    y <- matrix(y, ncol = 1)
  }


  # loss functions based on data
  loss <- loss_generator(x, y)
  loss_grad <- loss_grad_generator(x, y)

  # number of features
  d <- ncol(x)

  # build model
  c0 <- 1E-8
  V_min <- 0
  V_max <- Inf
  model <- model_cpa(d, c0 = c0, ...)

  # initialize params
  k <- 0L
  k_max <- 100
  eps_stop <- 1E-5
  eps <- Inf

  #print(ompr::variable_keys(model))
  while(TRUE) {
    # check solution status
    #print(ompr::extract_constraints(model))
    #cat("==============\n")
    #show_constraints(model)

    # get solution
    result <- ompr::solve_model(model, ompr.roi::with_ROI("glpk"))
    L_k <- unname(ompr::get_solution(result, L))
    lambda_k <- ompr::get_solution(result, lambda[i])$value
    alpha_k <- ompr::get_solution(result, alpha[i])$value

    # update objective bounds
    R_k <- sum(alpha_k > 0)
    V_min <- L_k + c0*R_k # set V_min using approximate loss
    V_k <- loss(lambda_k) + c0*R_k # set V_max using actual loss

    if (V_k < V_max) {
      V_max <- V_k
      lambda_best <- lambda_k
    }

    # store solutions
    cat(sprintf("[%s] optimality gap: %.01f%% solution: %s\n", k, 100*eps, paste(lambda_k, collapse = ", ")))

    # check termination conditions
    k <- k + 1L
    if (k > k_max) break

    eps <- 1 - V_min/V_max
    if (eps <= eps_stop) break

    # switch bounds
    #cat(sprintf("Vmin = %s   Vmax = %s\n", V_min, V_max))
    model <- ompr::set_bounds(model, V, lb = V_min, ub = V_max)

    # compute cut parameters
    loss_approx <- loss(lambda_k)
    loss_grad_approx <- loss_grad(lambda_k)
    q <- sum(loss_grad_approx * lambda_k)

    # add loss cut constraint
    model <- add_constraint(
      model,
      L >= loss_approx + sum_expr(colwise(loss_grad_approx[j]) * lambda[j], j = 1:d) - q
    )
  }

  #return(result)
  lambda <- lambda_k * alpha_k
  cat("=======================\n")
  cat(sprintf("optimality gap: %.01f%%\n", 100*eps))
  cat(sprintf("calibration error: %0.01f%%\n", 100*eval_calibration(lambda, x, y, has_plot = TRUE)))
  cat(sprintf("auc: %.02f\n", predict_auc(lambda, x, y)))
  return(lambda)
}

model_cpa <- function(d, R_min = 1, R_max = d, intercept_range = c(-10, 10), coef_range = c(-5, 5), c0 = 1E-8) {
  #d <- 10 # size of coefs
  intercept_min <- min(intercept_range)
  intercept_max <- max(intercept_range)
  coef_min <- min(coef_range)
  coef_max <- max(coef_range)

  # l0-norm of lambda (number of non-zero entries)
  #R_min <- 1L
  #R_max <- d

  # loss min-max range
  L_min = 0
  L_max = Inf

  # objective function min-max
  V_min = 0
  V_max = Inf

  # initialize model
  ompr::MILPModel() %>%
    # l0 indicator variables
    ompr::add_variable(alpha[j], j = 1:d,  type = "binary") %>%

    # coefficient bounds
    #ompr::add_variable(lambda[j], j = 1:d, type = "integer", lb = coef_min, ub = coef_max) %>%
    ompr::add_variable(lambda[j], j = 1:d, type = "integer") %>%

    # bounds on l0-norm
    ompr::add_variable(R, type = "integer", lb = R_min, ub = R_max) %>%

    # bounds on loss value
    ompr::add_variable(L, type = "continuous", lb = L_min, ub = L_max) %>%

    # bounds on objective value
    ompr::add_variable(V, type = "continuous", lb = V_min, ub = V_max) %>%

    # l0 indicator constraints
    ompr::add_constraint(lambda[j] >= intercept_min*alpha[j], j = 1) %>%
    ompr::add_constraint(lambda[j] <= intercept_max*alpha[j], j = 1) %>%
    ompr::add_constraint(lambda[j] >= coef_min*alpha[j], j = 2:d) %>%
    ompr::add_constraint(lambda[j] <= coef_max*alpha[j], j = 2:d) %>%

    # l0-norm
    ompr::add_constraint(R == sum_expr(alpha[j], j = 1:d)) %>%

    # cut constraint (initial loss boundary is just 0)
    ompr::add_constraint(L >= 0) %>%

    # objective
    ompr::add_constraint(V == L + c0*R) %>%
    ompr::set_objective(V, sense = "min")
}
