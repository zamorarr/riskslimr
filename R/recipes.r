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
