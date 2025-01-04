library("readr")
library("stringr")
library("purrr")
library("dplyr")

dat <- read_lines("input02.txt") %>%
  str_replace("Game \\d+: ", "") %>%
  strsplit("; ")

get_counts <- function(x) {
  data.frame(
    green = str_match(x, "(\\d+)[^\\d]+green")[, 2],
    blue = str_match(x, "(\\d+)[^\\d]+blue")[, 2],
    red = str_match(x, "(\\d+)[^\\d]+red")[, 2]
  )
}

counts <- map_dfr(dat, get_counts, .id = "game") %>%
  mutate(across(everything(), ~ as.numeric(.x)))

## Part 1
counts %>%
  mutate(across(green:red, ~ ifelse(is.na(.x), 0, .x))) %>%
  mutate(
    possible = case_when(
      red <= 12 & green <= 13 & blue <= 14 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(game) %>%
  summarize(possible = all(possible)) %>%
  filter(possible == TRUE) %>%
  summarize(sum(game))

## Part 2
counts %>%
  mutate(across(green:red, ~ ifelse(is.na(.x), 1, .x))) %>%
  group_by(game) %>%
  summarize(power = max(green) * max(red) * max(blue)) %>%
  summarize(sum(power))
