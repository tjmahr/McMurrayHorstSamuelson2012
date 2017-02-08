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
