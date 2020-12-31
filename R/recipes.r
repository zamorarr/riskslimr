#' @export
step_discretize_floor <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  by = 1,
  skip = FALSE,
  id = recipes::rand_id("discretize_floor")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_discretize_floor_new(
      terms = terms,
      trained = trained,
      role = role,
      by = by,
      breaks = NULL,
      skip = skip,
      id = id
    )
  )
}

step_discretize_floor_new <- function(terms, role, trained, by, breaks, skip, id) {
  recipes::step(
    subclass = "discretize_floor",
    terms = terms,
    role = role,
    trained = trained,
    by = by,
    breaks = breaks,
    skip = skip,
    id = id
  )
}

#' Round down to nearest n
#'
#' @param x numeric vector
#' @param n scalar integer
floor_n <- function(x, n = 1L) as.integer(n*floor(x/n))

prep.step_discretize_floor <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)

  # do something
  breaks <- purrr::map(training[, col_names],  ~ sort(unique(floor_n(.x, n = x$by))))

  # return the updated object
  step_discretize_floor_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    by = x$by,
    breaks = breaks,
    skip = x$skip,
    id = x$id
  )
}

bake.step_discretize_floor <- function(object, new_data, ...) {
  vars <- names(object$breaks)
  new_data[, vars] <- purrr::map_dfc(vars, function(nm) {
    x <- new_data[[nm]]
    b <- object$breaks[[nm]]
    cut(x, c(b, Inf), labels = paste(nm, b, sep = "_"), right = FALSE, ordered_result = FALSE)
    #cut(x, c(b, Inf), labels = b, right = FALSE, ordered_result = FALSE)
  })

  #print(new_data[,vars])

  tibble::tibble(new_data)
}

#' @export
step_discretize2 <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  filter = NULL,
  num_breaks = 4,
  breaks = NULL,
  skip = FALSE,
  id = recipes::rand_id("discretize2")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- recipes::ellipse_check(...)

  # capture filter expression
  #if (!is.null(filter)) filter <- rlang::enquos(filter)
  filter <- rlang::enquos(filter)


  recipes::add_step(
    recipe,
    step_discretize2_new(
      terms = terms,
      trained = trained,
      role = role,
      filter = filter,
      num_breaks = num_breaks,
      breaks - breaks,
      skip = skip,
      id = id
    )
  )
}

step_discretize2_new <- function(terms, role, trained, filter, num_breaks, breaks, skip, id) {
  recipes::step(
    subclass = "discretize2",
    terms = terms,
    role = role,
    trained = trained,
    filter = filter,
    num_breaks = num_breaks,
    breaks = breaks,
    skip = skip,
    id = id
  )
}


prep.step_discretize2 <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  #recipes::check_type(training[, col_names])

  # do something
  new_data <- training
  if (!is.null(x$filter)) new_data <- dplyr::filter(new_data, !!!x$filter)

  breaks <- lapply(new_data[, col_names], function(d) {
    b <- unname(quantile(d, seq(0, 1, length.out = x$num_breaks + 1L)))
    b <- sort(unique(b))
    b
  })
  names(breaks) <- col_names
  print(breaks)

  # return the updated object
  step_discretize2_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    filter = x$filter,
    num_breaks = x$num_breaks,
    breaks = breaks,
    skip = x$skip,
    id = x$id
  )
}

bake.step_discretize2 <- function(object, new_data, ...) {
  vars <- names(object$breaks)
  new_data[, vars] <- purrr::map_dfc(vars, function(nm) {
    x <- new_data[[nm]]
    b <- object$breaks[[nm]]
    cut(x, c(-Inf, b, Inf), labels = paste(nm, c("neginf", b), sep = "_"), right = FALSE, ordered_result = FALSE)
    #cut(x, c(b, Inf), labels = b, right = FALSE, ordered_result = FALSE)
  })

  #print(new_data[,vars])

  tibble::tibble(new_data)
}

#' @export
step_dummy_threshold <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  thresholds = 0,
  threshold_dir = c(">=", ">", "==", "<", "<="),
  skip = FALSE,
  id = recipes::rand_id("dummy_threshold")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- recipes::ellipse_check(...)

  threshold_dir <- match.arg(threshold_dir)

  recipes::add_step(
    recipe,
    step_dummy_threshold_new(
      terms = terms,
      trained = trained,
      role = role,
      thresholds = thresholds,
      threshold_dir = threshold_dir,
      vars = NULL,
      skip = skip,
      id = id
    )
  )
}

step_dummy_threshold_new <- function(terms, trained, role, thresholds, threshold_dir, vars, skip, id) {
  recipes::step(
    subclass = "dummy_threshold",
    terms = terms,
    trained = trained,
    role = role,
    thresholds = thresholds,
    threshold_dir = threshold_dir,
    vars = vars,
    skip = skip,
    id = id
  )
}

prep.step_dummy_threshold <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)

  # return the updated object
  step_dummy_threshold_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    thresholds = x$thresholds,
    threshold_dir = x$threshold_dir,
    vars = col_names,
    skip = x$skip,
    id = x$id
  )
}

bake.step_dummy_threshold <- function(object, new_data, ...) {
  vars <- object$vars
  thresholds <- object$thresholds
  threshold_dir <- object$threshold_dir
  threshold_dir_name <- switch(
    threshold_dir,
    ">=" = "gte",
    ">" = "gt",
    "==" = "eq",
    "<" = "lt",
    "<=" = "lte"
    )

  # add new columns
  compare <- rlang::as_function(threshold_dir)
  for (var in  vars) {
    for (r in thresholds) {
      nm <- paste(var, threshold_dir_name, r, sep = "_")
      new_data[[nm]] <- as.integer(compare(new_data[[var]], r))
    }

    # remove original column
    new_data[[var]] <- NULL
  }


  new_data
}

