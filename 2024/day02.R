library("purrr")
library("stringr")

dat <- readLines("input02.txt") %>%
  str_extract_all("\\d+") %>%
  map(as.numeric)

safe <- function(x, dampener = FALSE) {
  d <- diff(x)
  if (length(unique(sign(d))) > 1 || max(abs(d)) > 3 || min(abs(d)) < 1) {
    if (dampener) {
      return(any(map_lgl(seq_along(x), \(i) safe(x[-i]))))
    } else {
      return(FALSE)
    }
  }
  TRUE
}

## part 1
sum(map_lgl(dat, safe))

## part 2
sum(map_lgl(dat, safe, dampener = TRUE))
