library("readr")
library("stringr")
library("purrr")
library("tidyr")

dat <- read_lines("input03.txt")

## Part 1

non_alnum <- unique(unlist(str_extract_all(dat, "[^0-9a-z\\.]")))

mat <- matrix(
  unlist(strsplit(dat, "")),
  ncol = nchar(dat[[1]]),
  byrow = TRUE
)

num_locations <- str_locate_all(dat, "[[:digit:]]+")
num_values <- str_extract_all(dat, "[[:digit:]]+")

get_neighbors <- function(x, row, mat) {
  mat[max(0, (row-1)):min(nrow(mat), (row+1)), (max(1, min(x)-1)):(min(ncol(mat), max(x)+1))]
}

is_symbol_adjacent <- function(x, row, mat) {
  neighbors <- apply(
    x,
    MARGIN = 1,
    get_neighbors,
    row = row,
    mat = mat,
    simplify = FALSE
  )

  map_lgl(neighbors, ~ any(str_detect(., "\\*|-|\\$|@|=|#|\\+|/|%|&")))
}

part_number_indices <- imap(
  num_locations,
  is_symbol_adjacent,
  mat = mat
)

part_numbers <- mapply(
  function(x, y) as.numeric(x[y]),
  num_values,
  part_number_indices
)

sum(unlist(part_numbers))

## Part 2
