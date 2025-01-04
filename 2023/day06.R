library("readr")
library("stringr")

dat <- read_lines("input06.txt") %>%
  str_extract_all("\\d+") %>%
  lapply(as.numeric) %>%
  setNames(c("time", "duration"))

race_result <- function(duration) {
  hold_time <- 0:duration
  move_time <- duration - hold_time
  hold_time * move_time
}

get_winning_times <- function(poss_results, record) {
  sum(poss_results > record)
}

get_answer <- function(dat) {
  poss_results <- lapply(dat[["time"]], race_result)
  prod(mapply(get_winning_times, poss_results, dat[["duration"]]))
}

## Part 1
get_answer(dat)

## Part 2
dat <- lapply(dat, function(x) as.numeric(paste0(x, collapse = "")))
get_answer(dat)
