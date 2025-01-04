library("tidyverse")
dat <- readLines("input03.txt")
split <- strsplit(dat, "")

# Part 1

find_match <- function(x) {
  comp1 <- head(x, length(x) / 2)
  unique(comp1[comp1 %in% tail(x, length(x) / 2)])
}

get_priority <- function(x) {
  priority <- which(letters == x)
  if (length(priority) == 0) {
    priority <- which(LETTERS == x) + 26
  }
  priority
}

map(split, find_match) %>%
  map(get_priority) %>%
  unlist() %>%
  sum()

# Part 2

seq(1, length(split), by = 3) %>%
  map(function(x) split[x:(x+2)]) %>%
  map(function(x) intersect(intersect(x[[1]], x[[2]]), x[[3]])) %>%
  map(get_priority) %>%
  unlist() %>%
  sum()
