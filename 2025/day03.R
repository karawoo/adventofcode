library("purrr")
options(scipen = 9999)

dat <- readLines("input03.txt") |>
  strsplit("")

joltage <- function(bank, n) {
  if (n > 0) {
    candidates <- head(bank, length(bank) - n + 1)
    i <- which.max(candidates)
    paste0(bank[i], joltage(bank[(i + 1):length(bank)], n = n - 1))
  }
}

## part 1

map_dbl(dat, ~ as.numeric(joltage(.x, n = 2))) |>
  sum()

## part 2

map_dbl(dat, ~ as.numeric(joltage(.x, n = 12))) |>
  sum()
