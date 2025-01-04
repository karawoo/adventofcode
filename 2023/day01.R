library("readr")
library("stringr")
library("tidyverse")

dat <- read_lines("input01.txt")

## Part 1
str_extract_all(dat, "[[:digit:]]") %>%
  vapply(
    function(x) as.numeric(paste(x[1], x[length(x)], sep = "")),
    double(1)
  ) %>%
  sum()

## Part 2
char_to_num <- function(x) {
  case_when(
    x == "one" ~ "1",
    x == "two" ~ "2",
    x == "three" ~ "3",
    x == "four" ~ "4",
    x == "five" ~ "5",
    x == "six" ~ "6",
    x == "seven" ~ "7",
    x == "eight" ~ "8",
    x == "nine" ~ "9",
    TRUE ~ x
  )
}

str_match_all(
  dat,
  "(?=([[:digit:]]|one|two|three|four|five|six|seven|eight|nine))"
) %>%
  lapply(function(x) x[, 2]) %>%
  lapply(char_to_num) %>%
  vapply(function(x) as.numeric(paste0(x[1], x[length(x)])), numeric(1)) %>%
  sum()
