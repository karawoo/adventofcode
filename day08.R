library("readr")
library("tidyverse")

dat <- read_fwf(
  "input08.txt",
  col_positions = fwf_widths(rep(1, nchar(readLines("input08.txt", n = 1))))
) |>
  as.matrix()

in_bounds <- function(y, max) {
  all(y > 0) && all(y <= max)
}

add_antinodes <- function(pos, spacing, max) {
  added <- pos
  if (in_bounds(pos + spacing, max = max)) {
    added <- rbind(added, add_antinodes(pos + spacing, spacing, max))
  }
  added
}

get_antinodes <- function(freq, dat, part = 1) {
  x <- which(dat == freq, arr.ind = TRUE)
  antinodes <- matrix(nrow = 0, ncol = 2)

  for (i in seq_len(nrow(x))) {
    for (j in seq_len(nrow(x))) {
      if (!all(x[j, ] == x[i, ])) {
        spacing <- x[j, ] - x[i, ]
        a1 <- x[i, ] - spacing
        a2 <- x[j, ] + spacing
        if (part == 2) {
          a1 <- add_antinodes(a1, -spacing, max(dim(dat)))
          a2 <- add_antinodes(a2, spacing, max(dim(dat)))
        }
        antinodes <- rbind(antinodes, rbind(a1, a2))
      }
    }
  }
  if (part == 1 || nrow(x) == 1) {
    return(as_tibble(unique(antinodes)))
  } else {
    return(as_tibble(rbind(unique(antinodes), x)))
  }
}

get_all_antinodes <- function(dat, part = 1) {
  freqs <- setdiff(unique(as.vector(dat)), ".")
  map_dfr(freqs, get_antinodes, dat = dat, part = part) |>
    unique() |>
    filter(if_all(everything(), ~ .x > 0)) |>
    filter(if_all(everything(), ~ .x <= max(dim(dat))))
}

## part 1
nrow(get_all_antinodes(dat, part = 1))

## part 2
nrow(get_all_antinodes(dat, part = 2))
