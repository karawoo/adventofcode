library("readr")
library("purrr")

dat <- read_file("input01.txt")

# Part 1

sums <- strsplit(dat, "\n\n")[[1]] %>%
  map(~ sum(as.numeric(strsplit(.x, "\n")[[1]]), na.rm = TRUE)) %>%
  unlist()

max(sums)

# Part 2

sum(tail(sort(sums), 3))
