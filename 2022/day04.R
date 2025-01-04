library("tidyverse")

dat <- readLines("input04.txt")
pairs <- str_split(dat, "(,|-)") %>%
  map(as.numeric)

# Part 1

contains <- function(x) {
  (x[1] <= x[3] & x[2] >= x[4]) | (x[3] <= x[1] & x[4] >= x[2])
}

pairs %>%
  map_lgl(contains) %>%
  sum()

# Part 2

overlaps <- function(x) {
  length(intersect(x[1]:x[2], x[3]:x[4])) >= 1
}

pairs  %>%
  map_lgl(overlaps) %>%
  sum()
