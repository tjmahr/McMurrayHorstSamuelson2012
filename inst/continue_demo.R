# Test of performing additional epochs from a saved network

library(magrittr)
library(dplyr)
library(ggplot2)
library(McMurrayHorstSamuelson2012)

network <- readRDS("inst/demo_network.rds")
tests <- readr::read_csv("inst/demo_tests.csv")

n_whole <- 10000
n <- 10000

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
        legend.key = element_rect(fill = NA, color = NA)) +
  stat_smooth(se = FALSE)

ggsave(filename = "inst/demo_fig.png", last_plot(), width = 4, height = 3, dpi = 300)

saveRDS(network, file = "inst/demo_network.rds")
readr::write_csv(tests, "inst/demo_tests.csv")
