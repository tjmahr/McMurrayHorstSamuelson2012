library(magrittr)
library(McMurrayHorstSamuelson2012)

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
  settle_stability = 1e-12,
  referential_ambiguity = .5
)

network <- do.call(create_network, settings)

network <- network %>%
  set_random_input() %>%
  run_epoch()

network <- network %>%
  set_random_input() %>%
  run_epoch()

n <- 998
tests <- dplyr::data_frame()

while (n > 0) {
  network <- network %>%
    set_random_input() %>%
    run_epoch()
  n <- n - 1

  if (n %% 50 == 0) {
    test_3afc <- run_afc_battery(network, n_foils = 2)
    test_10afc <- run_afc_battery(network, n_foils = 9)
    tests <- dplyr::bind_rows(tests, test_3afc, test_10afc)
  }
}






# tests <- dplyr::bind_rows(test_3afc, test_10afc)

library(ggplot2)
library(dplyr)

words_known <- tests %>%
  group_by(Test, PreviousEpochs) %>%
  summarize(Words = sum(Correct)) %>%
  ungroup()

ggplot(words_known) +
  aes(x = PreviousEpochs, y = Words, color = Test) +
  geom_point() +
  geom_line()
