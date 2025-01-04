library("readr")
library("stringr")

dat <- read_lines("input04.txt")

nums <- str_remove_all(dat, "Card +\\d+: +") %>%
  str_split(" \\| +") %>%
  lapply(function(x) lapply(str_split(x, " +"), as.numeric))

winning <- lapply(nums, `[[`, 1)
cards <- lapply(nums, `[[`, 2)

## Part 1
score <- function(card, winning) {
  any(card %in% winning) * 2^(sum(card %in% winning) - 1)
}

sum(mapply(score, cards, winning))

## Part 2
copies <- rep(1, length(cards))

score2 <- function(copies, i, cards, winning) {
  n_copies <- copies[[i]]
  n_new <- sum(cards[[i]] %in% winning[[i]])
  if (n_new > 0) {
    copies[(i+1):(i+n_new)] <- copies[(i+1):(i+n_new)] + n_copies
  }
  copies
}

for (i in seq_along(copies)) {
  copies <- score2(copies, i, cards, winning)
}

sum(copies)
