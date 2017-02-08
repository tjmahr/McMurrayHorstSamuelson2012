
#' @export
create_network_from_list <- function(settings) {
  do.call(create_network, settings)
}

#' @export
create_network <- function(n_wordforms, n_images, n_lexicon, weight_init_min,
                           weight_init_max, ff_temp, fb_temp, inhib_lexicon,
                           inhib_wordform, inhib_image, learning_rate,
                           settle_stability, referential_ambiguity) {
  # Initialize layers
  layer_wordform <- numeric(n_wordforms)
  layer_image <- numeric(n_images)
  layer_lexicon <- equalize_vector(numeric(n_lexicon))

  # Initialize weights
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

  # Create the network object
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
      referential_ambiguity = referential_ambiguity,
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
      settled = FALSE,
      epochs = 0,
      epoch_history = numeric(0))
  )

  structure(bundle, class = c("mhs_net", "list"))
}


#' @export
print.mhs_net <- function(x, ...) {
  utils::str(x, ...)
}





#' @export
set_random_input <- function(network) {
  words <- seq_along(network$layers$wordform)
  target_word <- sample(words, 1)

  images <- seq_along(network$layers$image)
  draws <- stats::runif(images) <= network$params$referential_ambiguity
  competitors <- images[draws]

  active_images <- unique(c(target_word, competitors))

  network %>%
    set_active_wordform(target_word) %>%
    set_active_image(active_images)
}

#' @export
set_afc_input <- function(network, target_word, n_foils) {
  images <- seq_along(network$layers$image)
  alternatives <- images[-target_word]
  foils <- sample(alternatives, size = n_foils)

  choices <- c(target_word, foils)

  network %>%
    set_active_wordform(target_word) %>%
    set_active_image(choices)
}

#' @export
set_active_wordform <- function(network, wordform) {
  newer <- activate_input(network$layers$wordform, wordform)
  network$layers$wordform <- newer
  network
}

#' @export
set_active_image <- function(network, image) {
  newer <- activate_input(network$layers$image, image)
  network$layers$image <- newer
  network
}

#' @export
rest_lexicon <- function(network) {
  network$layers$lexicon <- equalize_vector(network$layers$lexicon)
  network
}

#' @export
run_cycle <- function(network) {
  last_state <- network$layers$lexicon

  # Collect input and normalize
  input_visual <- network$weights$image_lexicon %*% network$layers$image
  input_audio <- network$weights$wordform_lexicon %*% network$layers$wordform
  input_total <- as.vector(input_visual + input_audio)
  update_amount <- network$params$ff_temp * input_total

  network$layers$lexicon <- (network$layers$lexicon + update_amount) %>%
    power_normalize_vector(network$params$inhib_lexicon)

  # Determine if network has settled
  change <- network$layers$lexicon - last_state
  network$history$settled <- sum(change ^ 2) < network$params$settle_stability

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

#' @export
cycle_network <- function(network, n_cycles) {
  stopifnot(n_cycles > 0)
  while (n_cycles > 0 ) {
    network <- run_cycle(network)
    n_cycles <- n_cycles - 1
  }

  network
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

#' @export
run_epoch <- function(network) {
  network <- rest_lexicon(network)
  network$history$settled <- FALSE

  starting_cycles <- network$history$cycles

  while (!network$history$settled) {
    network <- run_cycle(network)
  }

  curr_cycles <- network$history$cycles - starting_cycles
  prev_cycles <- network$history$epoch_history

  network$history$epochs <- network$history$epochs + 1
  network$history$epoch_history <- c(prev_cycles, curr_cycles)
  network
}


