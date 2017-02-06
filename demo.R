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
  rep(1 / length(xs), length(xs))
}

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

settings <- list(
  n_wordforms = 20,
  n_images = 18,
  n_lexicon = 35,
  weight_init_min = 0,
  weight_init_max = .5,
  ff_temp = 1,
  fb_temp = 1,
  inhib_lexicon = 2,
  inhib_wordform = 1.4,
  inhib_image = 1.4,
  learning_rate = .0005
)

create_network <- function(n_wordforms, n_images, n_lexicon, weight_init_min,
                           weight_init_max, ff_temp, fb_temp, inhib_lexicon,
                           inhib_wordform, inhib_image, learning_rate) {
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
      inhib_lexicon = inhib_lex,
      ff_temp = ff_temp,
      fb_temp = fb_temp
    ),
    history = list(
      cycles = 0)
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
  network$weights$image_lexicon
  network$weights$wordform_lexicon
  network$layers$image
  network$layers$wordform
  network$layers$lexicon
  network$params$ff_temp

  input_visual <- network$weights$image_lexicon %*% network$layers$image
  input_audio <- network$weights$wordform_lexicon %*% network$layers$wordform
  input_total <- input_visual + input_audio
  update_amount <- network$params$ff_temp * input_total

  network$layers$lexicon <- (network$layers$lexicon + update_amount) %>%
    power_normalize_vector(network$params$inhib_lexicon) %>%
    as.vector()

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

n <- do.call(create_network, settings) %>%
  set_active_image(active_images) %>%
  set_active_wordform(active_word)

n <- n %>%
  cycle_network(1000) %>%
  set_active_image(c(7, 8, 10)) %>%
  set_active_wordform(7) %>%
  cycle_network(1000) %>%
  set_active_image(c(1, 2, 3, 7, 10)) %>%
  set_active_wordform(10) %>%
  cycle_network(1000) %>%
  set_active_image(c(7, 8, 10)) %>%
  set_active_wordform(7) %>%
  cycle_network(1000)

round(n$weights$image_lexicon, 2)
round(n$weights$wordform_lexicon, 2)

network$layers$lexicon














# # Old pre-network object code
#
# # Collect input and normalize
# visual_input <- weight_image_lexicon %*% layer_image
# audio_input <- weight_wordform_lexicon %*% layer_wordform
# total_input <- visual_input + audio_input
# layer_lexicon <- layer_lexicon + ff_temp * total_input
# layer_lexicon <- as.vector(power_normalize_vector(layer_lexicon, inhib_lex))
#
# # Feedback to inputs
# fb_image <- as.vector(t(weight_image_lexicon) %*% layer_lexicon) * layer_image
# layer_image <- layer_image + fb_temp * fb_image
# layer_image <- power_normalize_vector(layer_image, inhib_image)
#
# fb_wordform <- as.vector(t(weight_wordform_lexicon) %*% layer_lexicon)  * layer_wordform
# layer_wordform <- layer_wordform + fb_temp * fb_wordform
# layer_wordform <- power_normalize_vector(layer_wordform, inhib_wordform)
#
#
# # Hebbian learning. Not sure yet if matrix multiplication in the function is
# # correct.
# d1 <- compute_delta_weights(layer_image, layer_lexicon, weight_image_lexicon)
# d2 <- compute_delta_weights(layer_wordform, layer_lexicon, weight_wordform_lexicon)
#
# weight_image_lexicon <- weight_image_lexicon + learning_rate * d1
# weight_wordform_lexicon <- weight_wordform_lexicon + learning_rate * d2
#





