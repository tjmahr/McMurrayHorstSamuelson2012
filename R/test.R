#' @export
run_afc_battery <- function(network, n_foils) {
  test_name <- paste0(1 + n_foils, "AFC")

  start <- network$history$epochs
  network <- rest_lexicon(network)

  # Pre-allocate space for results
  n_words <- length(network$layers$wordform)
  targets <- numeric(n_words)
  correct <- logical(n_words)
  activations <- numeric(n_words)
  rts <- numeric(n_words)

  for (target in seq_len(n_words)) {
    # Use a fresh version of network for each test
    test_network <- network %>%
      set_afc_input(target, n_foils) %>%
      run_epoch()

    # Store results
    targets[target] <- target
    rts[target] <- utils::tail(test_network$history$epoch_history, 1)
    guess <- which.max(test_network$layers$image)
    activations[target] <- test_network$layers$image[guess]
    correct[target] <- guess == target
  }

  # Return a data-frame
  results <- tibble::tibble(
    Word = targets,
    RT = rts,
    Activation = activations,
    Correct = correct,
    Test = test_name,
    PreviousEpochs = start)

  results <- results[c("Test", "PreviousEpochs", "Word",
                       "RT", "Activation", "Correct")]
  results
}


#' @export
run_repeated_afc_battery <- function(network, n_foils, n_trials, criterion) {
  test_name <- paste0(1 + n_foils, "AFC", "-", criterion, "/", n_trials)

  # Run multiple AFC trials
  afcs <- replicate(
    n = n_trials,
    expr = run_afc_battery(network, n_trials),
    simplify = FALSE)

  # Aggregate number correct by target word
  results <- bind_rows(afcs) %>%
    mutate_(Test = ~ test_name) %>%
    group_by_("Test", "PreviousEpochs", "Word") %>%
    summarise_(NumCorrect = ~ sum(Correct)) %>%
    mutate_(Correct = ~ NumCorrect >= criterion) %>%
    ungroup() %>%
    select_(~ -NumCorrect)

  tibble::as_tibble(results)
}







#' @export
run_word_production_battery <- function(network) {
  test_name <- paste0("WordProduction")

  start <- network$history$epochs
  network <- rest_lexicon(network)

  # Pre-allocate space for results
  n_words <- length(network$layers$wordform)
  targets <- numeric(n_words)
  correct <- logical(n_words)
  activations <- numeric(n_words)
  rts <- numeric(n_words)

  for (target_image in seq_len(n_words)) {
    # Use a fresh version of network for each test
    test_network <- network %>%
      set_word_production_input(target_image) %>%
      run_epoch()

    # Store results
    targets[target_image] <- target_image
    rts[target_image] <- utils::tail(test_network$history$epoch_history, 1)
    guess <- which.max(test_network$layers$wordform)
    activations[target_image] <- test_network$layers$wordform[guess]
    correct[target_image] <- guess == target_image
  }

  # Return a data-frame
  results <- tibble::tibble(
    Word = targets,
    RT = rts,
    Activation = activations,
    Correct = correct,
    Test = test_name,
    PreviousEpochs = start)

  results <- results[c("Test", "PreviousEpochs", "Word",
                       "RT", "Activation", "Correct")]
  results
}


#' @export
analyze_weights <- function(network) {
  test_name <- "WeightAnalysis"

  # Find strongest weight for each input node
  wordforms <- apply(network$weights$wordform_lexicon, MARGIN = 2, which.max)
  images <- apply(network$weights$image_lexicon, MARGIN = 2, which.max)

  # A word-object mapping has been learned if the same hidden unit is activated
  # by a wordform-image pair.
  matches <- wordforms[wordforms == images]

  # Don't count repetitions, i.e., where two word-object mappings have the same
  # lexical item.
  words_known <- length(unique(matches))

  tibble::tibble(
    Test = test_name,
    PreviousEpochs = network$history$epochs,
    WordObjectMappings = length(matches),
    WordsKnown = words_known
  )
}


#' @export
analyze_weight_strengths <- function(network) {
  test_name <- "ConnectionAnalysis"

  wordforms <- seq_len(ncol(network$weights$wordform_lexicon))
  images <- seq_len(ncol(network$weights$image_lexicon))

  # Find most-connected lexical entry for each wordform-image input pair
  input_wts <- network$weights$wordform_lexicon + network$weights$image_lexicon
  most_activated_words <- apply(input_wts, MARGIN = 2, which.max)

  # Identify the strongest auditory and visual connections for each input
  is_strongest_auditory <- create_matrix_mask(
    network$weights$wordform_lexicon,
    most_activated_words,
    seq_along(wordforms))

  is_strongest_visual <- create_matrix_mask(
    network$weights$image_lexicon,
    most_activated_words,
    seq_along(images))

  strongest_auditory <- network$weights$wordform_lexicon[is_strongest_auditory]
  strongest_visual <- network$weights$image_lexicon[is_strongest_visual]

  # The irrelevant connections
  weaker_auditory <- network$weights$wordform_lexicon[!is_strongest_auditory]
  weaker_visual <- network$weights$image_lexicon[!is_strongest_visual]

  # Entropy of weight matrix
  entropy_aud <- compute_weight_entropy(network$weights$wordform_lexicon)
  entropy_vis <- compute_weight_entropy(network$weights$image_lexicon)

  results <- tibble::tribble(
    ~ Relevance,  ~ Stat,    ~ InputType, ~ Value,
    "Correct",    "Mean",    "Auditory",  mean(strongest_auditory),
    "Correct",    "Mean",    "Visual",    mean(strongest_visual),
    "Correct",    "Max",     "Auditory",  max(strongest_auditory),
    "Correct",    "Max",     "Visual",    max(strongest_visual),

    "Irrelevant", "Mean",    "Auditory",  mean(weaker_auditory),
    "Irrelevant", "Mean",    "Visual",    mean(weaker_visual),
    "Irrelevant", "Max",     "Auditory",  max(weaker_auditory),
    "Irrelevant", "Max",     "Visual",    max(weaker_visual),

    "All",        "Entropy", "Auditory",  entropy_aud,
    "All",        "Entropy", "Visual",    entropy_vis
  )

  results %>%
    mutate(
      TestName = test_name,
      PreviousEpochs = network$history$epochs) %>%
    select_(~ TestName, ~ PreviousEpochs, ~ everything())
}


# # Vectorized look up of many elements from a given matrix
# map_extract <- function(matrix, rows, cols) {
#   unlist(Map(function(i, j) matrix[i, j], i = rows, j = cols))
# }
