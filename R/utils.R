power_normalize_vector <- function(xs, power) {
  normalize_vector(xs ^ power)
}

normalize_vector <- function(xs) {
  updated <- xs / sum(xs)
  stopifnot(all.equal(sum(updated), 1))
  updated
}

equalize_vector <- function(xs) {
  n <- length(xs)
  rep(1 / n, n)
}

randomize_weights <- function(weight_matrix, lower, upper) {
  n <- length(weight_matrix)
  limits <- range(c(lower, upper))
  draws <- runif(n, min = limits[1], max = limits[2])
  matrix(draws, nrow = nrow(weight_matrix), ncol = ncol(weight_matrix))
}

activate_input <- function(xs, to_activate) {
  xs <- zero_along(xs)
  xs[to_activate] <- 1
  normalize_vector(xs)
}

zero_along <- function(xs) {
  rep(0, length(xs))
}
