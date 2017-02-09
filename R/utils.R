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
  draws <- stats::runif(n, min = limits[1], max = limits[2])
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


# Create a logical matrix `m` where each `m[rows[i], cols[i]]` is `TRUE`. `m`
# has the same dimension as the template `matrix`.
create_matrix_mask <- function(matrix, rows, cols) {
  # Store the number of rows to create
  n_rows <- nrow(matrix)
  create_one_column <- function(curr_rows) create_column_mask(n_rows, curr_rows)

  # Create each column and combine into a matrix
  mask <- Map(create_one_column, rows) %>%
    unlist() %>%
    matrix(nrow = nrow(matrix))

  # Sanity test
  stopifnot(apply(mask, 2, which) == rows)

  mask
}

# Create a logical vector `x` of length `nrows` where `x[true_rows]` is `TRUE`
create_column_mask <- function(nrows, true_rows) {
  col <- logical(nrows)
  col[true_rows] <- TRUE
  col
}

compute_weight_entropy <- function(matrix) {
  # Convert weights to probabilities
  weight_probs <- matrix / sum(matrix)
  # Entropy: -Σ(Pj * log2(Pj))
  entropy <- -1 * sum(weight_probs * log2(weight_probs))
  entropy
}
