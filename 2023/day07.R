library("readr")
library("stringr")
library("tidyverse")

factorize <- function(x, jokers_wild = FALSE) {
  if (jokers_wild) {
    levels <- c("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")
  } else {
    levels <-  c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
  }
  factor(
    x,
    levels = levels,
    ordered = TRUE
  )
}

hand_type <- function(hand, jokers_wild = FALSE) {
  if (jokers_wild && !all(hand == "J"))  {
    counts <- table(hand[hand != "J"])
    strongest_alternative <- max(factorize(names(counts[counts == max(counts)])))
    hand[hand == "J"] <- as.character(strongest_alternative)
  }
  if (table(hand)[1] == 5) {
    type <- "5kind"
  } else if (4 %in% table(hand)) {
    type <- "4kind"
  } else if (3 %in% table(hand) & length(table(hand)) == 2) {
    type <- "fullhouse"
  } else if (3 %in% table(hand)) {
    type <- "3kind"
  } else if (sum(table(hand) == 2) == 2) {
    type <- "twopair"
  } else if (sum(table(hand) == 2) == 1){
    type <- "onepair"
  } else {
    type <- "highcard"
  }
  factor(type, levels = c("highcard", "onepair", "twopair", "3kind", "fullhouse", "4kind", "5kind"))
}

get_winnings <- function(jokers_wild = FALSE) {
  dat <- read_lines("input07.txt") %>%
    as_tibble() %>%
    separate_wider_delim(value, " ", names = c("hand", "bid")) %>%
    mutate(hand = str_split(hand, "")) %>%
    rowwise() %>%
    mutate(hand_type = hand_type(hand, jokers_wild = jokers_wild)) %>%
    mutate(hand = list(setNames(hand, 1:5))) %>%
    unnest_wider(hand) %>%
    mutate(
      across(
        1:5,
        \(x) factorize(x, jokers_wild = jokers_wild)
      )
    ) %>%
    arrange(hand_type, across(1:5)) %>%
    mutate(rank = row_number()) %>%
    mutate(score = rank * as.numeric(bid))

  sum(dat$score)
}

## Part 1
get_winnings(jokers_wild = FALSE)

## Part 2
get_winnings(jokers_wild = TRUE)
