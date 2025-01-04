library("readr")
library("stringr")
library("purrr")

dat <- read_file("input13.txt")

patterns <- str_split(dat, "\n\n")[[1]] %>%
  lapply(function(x) str_split(x, "\n")[[1]]) %>%
  lapply(function(x) x[x != ""])

## Part 1

has_reflection <- function(pattern) {
  potential_ref_pts <- which(pattern[-length(pattern)] == pattern[-1])
  for (i in potential_ref_pts) {
    if (identical(subset_pattern(i, pattern), rev(subset_pattern(i, pattern)))) {
      return(i)
    }
  }
  return(FALSE)
}

subset_pattern <- function(pt, pattern) {
  if (pt > length(pattern) / 2) {
    tail(pattern, (length(pattern) - pt) * 2)
  } else {
    head(pattern, pt * 2)
  }
}

test_pattern <- function(pattern) {
  horiz_reflection <- has_reflection(pattern)
  if (!isFALSE(horiz_reflection)) {
    return(horiz_reflection * 100)
  } else {
    pattern_mat <- str_split(pattern, "", simplify = TRUE)
    t_pat <- apply(pattern_mat, 2, paste0, collapse = "")
    vertical_reflection <- has_reflection(t_pat)
    if (!isFALSE(vertical_reflection)) {
      return(vertical_reflection)
    }
  }
  0
}

results <- lapply(patterns, test_pattern)
sum(vapply(patterns, test_pattern, numeric(1)))
