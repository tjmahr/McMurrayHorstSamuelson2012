library(magrittr)

randomize_weights <- function(weight_matrix, lower, upper) {
  n <- length(weight_matrix)
  draws <- runif(n, lower, upper)
  matrix(draws, nrow = nrow(weight_matrix), ncol = ncol(weight_matrix))
}

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

# auditory = auditory + m.fbT*auditory .* (decision*aud2dec');
compute_feedback <- function(input_xs, hidden_xs, weights, scale = 1) {
  feedback <- as.vector(t(weights) %*% hidden_xs) * input_xs
  feedback * scale
}

compute_delta_weights <- function(input_xs, hidden_xs, weights, scale = 1) {
  growth <- outer(hidden_xs, input_xs) * (1 - weights)
  decay1 <- outer(1 - hidden_xs, input_xs) * (weights)
  decay2 <- outer(hidden_xs, 1 - input_xs) * (weights)
  scale * (growth - .5 * decay1 - .5 * decay2)
}

# Manual way to check above
compute_delta_weights2 <- function(input_xs, hidden_xs, weights, scale = 1) {
  deltas <- weights

  for (i in seq_along(input_xs)) {
    input_x <- input_xs[i]

    for (h in seq_along(hidden_xs)) {
      input_h <- hidden_xs[h]
      w <- weights[h, i]

      growth <- input_x * input_h * (1 - w)
      decay1 <- .5 * (1 - input_x) * input_h * w
      decay2 <- .5 * input_x * (1 - input_h) * w
      deltas[h, i] <- growth - decay1 - decay2
    }
  }
  deltas * scale
}


settings <- list(
  n_wordforms = 35,
  n_images = 35,
  n_lexicon = 500,
  weight_init_min = 0,
  weight_init_max = .5,
  ff_temp = .01,
  fb_temp = 2,
  inhib_lexicon = 2,
  inhib_wordform = 1.05,
  inhib_image = 1.05,
  learning_rate = .0005,
  settle_stability = 1e-12
)




create_network <- function(n_wordforms, n_images, n_lexicon, weight_init_min,
                           weight_init_max, ff_temp, fb_temp, inhib_lexicon,
                           inhib_wordform, inhib_image, learning_rate,
                           settle_stability) {
  layer_wordform <- numeric(n_wordforms)
  layer_image <- numeric(n_images)
  layer_lexicon <- equalize_vector(numeric(n_lexicon))

  weight_image_lexicon <- layer_lexicon %o% layer_image
  weight_wordform_lexicon <- layer_lexicon %o% layer_wordform

  weight_image_lexicon <- randomize_weights(
    weight_image_lexicon,
    weight_init_min,
    weight_init_max)

  weight_wordform_lexicon <- randomize_weights(
    weight_wordform_lexicon,
    weight_init_min,
    weight_init_max)

  bundle <- list(
    layers = list(
      wordform = layer_wordform,
      image = layer_image,
      lexicon = layer_lexicon),
    weights = list(
      image_lexicon = weight_image_lexicon,
      wordform_lexicon = weight_wordform_lexicon
    ),
    params = list(
      learning_rate = learning_rate,
      inhib_wordform = inhib_wordform,
      inhib_image = inhib_image,
      inhib_lexicon = inhib_lexicon,
      ff_temp = ff_temp,
      fb_temp = fb_temp,
      settle_stability = settle_stability
    ),
    history = list(
      cycles = 0,
      change_in_lexical_activation = 0)
  )

  structure(bundle, class = c("mhs_net", "list"))
}

print.mhs_net <- function(x, ...) {
  str(x, ...)
}

set_active_wordform <- function(network, wordform) {
  network$layers$wordform[wordform] <- 1
  network$layers$wordform <- normalize_vector(network$layers$wordform)
  network
}

set_active_image <- function(network, image) {
  network$layers$image[image] <- 1
  network$layers$image <- normalize_vector(network$layers$image)
  network
}

run_cycle <- function(network) {
  # Collect input and normalize
  input_visual <- network$weights$image_lexicon %*% network$layers$image
  input_audio <- network$weights$wordform_lexicon %*% network$layers$wordform
  input_total <- input_visual + input_audio
  update_amount <- network$params$ff_temp * input_total

  last_state <- network$layers$lexicon
  network$layers$lexicon <- (network$layers$lexicon + update_amount) %>%
    power_normalize_vector(network$params$inhib_lexicon) %>%
    as.vector()

  # Need to determine if network has settled
  change <- network$layers$lexicon - last_state
  network$history$change_in_lexical_activation <- sum(change ^ 2)

  # Feedback to inputs
  fb_image <- compute_feedback(
    input_xs = network$layers$image,
    hidden_xs = network$layers$lexicon,
    weights = network$weights$image_lexicon,
    scale = network$params$fb_temp)

  fb_wordform <- compute_feedback(
    input_xs = network$layers$wordform,
    hidden_xs = network$layers$lexicon,
    weights = network$weights$wordform_lexicon,
    scale = network$params$fb_temp)

  network$layers$image <- (network$layers$image + fb_image) %>%
    power_normalize_vector(network$params$inhib_image)

  network$layers$wordform <- (network$layers$wordform + fb_wordform) %>%
    power_normalize_vector(network$params$inhib_wordform)

  # Hebbian learning. Not sure yet if matrix multiplication in the function is
  # correct.
  d1 <- compute_delta_weights(
    input_xs = network$layers$image,
    hidden_xs = network$layers$lexicon,
    weights = network$weights$image_lexicon,
    scale = network$params$learning_rate)

  d2 <- compute_delta_weights(
    input_xs = network$layers$wordform,
    hidden_xs = network$layers$lexicon,
    weights = network$weights$wordform_lexicon,
    scale = network$params$learning_rate)

  network$weights$image_lexicon <- network$weights$image_lexicon + d1
  network$weights$wordform_lexicon <- network$weights$wordform_lexicon + d2

  network$history$cycles <- network$history$cycles + 1

  network
}

cycle_network <- function(network, n_cycles) {
  stopifnot(n_cycles > 0)
  while (n_cycles > 0 ) {
    network <- run_cycle(network)
    n_cycles <- n_cycles - 1
  }

  network
}

active_word <- 1
active_images <- c(1, 2, 4, 6)

n <- do.call(create_network, settings)

n <- n %>%
  set_active_image(active_images) %>%
  set_active_wordform(active_word)

n <- n %>%
  cycle_network(10000)
n




# 1.84e-11 <settle_stability
#
# %>%
#   set_active_image(c(7, 8, 10)) %>%
#   set_active_wordform(7) %>%
#   cycle_network(50)
#

round(n$weights$image_lexicon, 2)
round(n$weights$wordform_lexicon, 2)
round(n$layers$image, 2)
round(n$layers$wordform, 2)

n$layers$lexicon

