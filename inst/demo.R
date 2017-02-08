library(magrittr)
library(McMurrayHorstSamuelson2012)
library(ggplot2)
library(dplyr)

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

# Create a network from a list
network <- create_network_from_list(settings)

# Run some training trials
network <- network %>%
  set_random_input() %>%
  run_epoch()

network <- network %>%
  set_random_input() %>%
  run_epoch()

# Run a bunch more with periodic testing
n <- 998
n_whole <- 998
tests <- data_frame()
wt_tests <- data_frame()


while (n > 0) {
  network <- network %>%
    set_random_input() %>%
    run_epoch()
  n <- n - 1

  if (n %% 50 == 0) {
    message("Testing on iteration ", n_whole - n)
    test_3afc <- run_afc_battery(network, n_foils = 2)
    test_10afc <- run_afc_battery(network, n_foils = 9)
    tests <- bind_rows(tests, test_3afc, test_10afc)

    wts <- analyze_weights(network)
    wt_tests <- bind_rows(wt_tests, wts)
  }
}

words_known <- tests %>%
  group_by(Test, PreviousEpochs) %>%
  summarize(Words = sum(Correct)) %>%
  ungroup()

ggplot(words_known) +
  aes(x = PreviousEpochs, y = Words, color = Test) +
  geom_point() +
  geom_line()

n <- 10000
n_whole <- 10000

while (n > 0) {
  network <- network %>%
    set_random_input() %>%
    run_epoch()
  n <- n - 1

  if (n %% 50 == 0) {
    message("Testing on iteration ", n_whole - n)
    test_3afc <- run_afc_battery(network, n_foils = 2)
    test_10afc <- run_afc_battery(network, n_foils = 9)
    tests <- dplyr::bind_rows(tests, test_3afc, test_10afc)
  }
}

words_known <- tests %>%
  group_by(Test, PreviousEpochs) %>%
  summarize(Words = sum(Correct)) %>%
  ungroup()

ggplot(words_known) +
  aes(x = PreviousEpochs, y = Words, color = Test) +
  geom_line() +
  labs(x = "Epochs of training", y = "Words known", color = NULL) +
  ylim(c(0, 35)) +
  theme_grey(base_size = 10) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))

ggsave(filename = "inst/demo_fig.png", last_plot(), width = 4, height = 3, dpi = 300)

saveRDS(network, file = "inst/demo_network.rds")
readr::write_csv(tests, "inst/demo_tests.csv")
