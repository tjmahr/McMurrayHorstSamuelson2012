#' @export
run_afc_battery <- function(network, n_foils, test_name = paste0(1 + n_foils, "AFC")) {
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
    test_network <-  network %>%
      set_afc_input(target, n_foils) %>%
      run_epoch()

    # Store results
    targets[target] <- target
    rts[target] <- utils::tail(test_network$history$epoch_history, 1)
    guess <- which.max(test_network$layers$image)
    activations[target] <- test_network$layers$image[guess]
    correct[target] <- guess == target
  }

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
