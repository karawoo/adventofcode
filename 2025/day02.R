library("stringr")
library("purrr")

dat <- readLines("input02.txt")

# discard any that have more than length/2 unique digits
candidate <- function(val) {
  val_str <- str_split(val, "")[[1]]
  length(unique(val_str)) <= (length(val_str) / 2)
}

to_check <- str_split(dat, ",")[[1]] |>
  str_split("-") |>
  map(~ seq(.x[1], .x[2])) |>
  unlist() |>
  keep(candidate)

## part 1

invalid1 <- function(x) {
  x <- str_split(x, "")[[1]]
  if (identical(head(x, length(x) / 2), tail(x, length(x) / 2))) {
    as.numeric(paste0(x, collapse = ""))
  } else {
    0
  }
}

map_dbl(to_check, invalid1) |>
  sum()

## part 2

## divide up string into pieces of length n
split_group_size <- function(string, n) {
  x <- str_split(string, "")[[1]]
  groups <- ceiling(seq_along(x) / n)
  split(x, groups) |>
    map(paste0, collapse = "")
}

invalid2 <- function(x) {
  for (i in seq_len(nchar(x)) / 2) {
    pieces <- split_group_size(x, i)
    if (length(unique(unlist(pieces))) == 1) {
      return(sum(as.numeric(x)))
    }
  }
  0
}

## it's slowww
map_dbl(to_check, invalid2) |>
  sum()
