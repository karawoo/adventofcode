library("readr")
library("stringr")

dat <- read_lines("input09.txt") %>%
  str_split(" ") %>%
  lapply(as.numeric)

get_next_val <- function(dat, dir = "forward") {
  i <- 1
  diffs <- list(dat)
  while (!all(diffs[[i]] == 0)) {
    i <- i + 1
    diffs[[i]] <- diff(diffs[[i - 1]])
  }
  diffs <- rev(diffs)
  for (i in 2:length(diffs)) {
    if (dir == "forward") {
      diffs[[i]] <- c(diffs[[i]], diffs[[i]][length(diffs[[i]])] + tail(diffs[[i - 1]], 1))
    } else {
      diffs[[i]] <- c(diffs[[i]][1] - head(diffs[[i - 1]], 1), diffs[[i]])
    }
  }

  if (dir == "forward") {
    return(tail(diffs[[length(diffs)]], 1))
  } else {
    return(head(diffs[[length(diffs)]], 1))
  }
}

sum(vapply(dat, get_next_val, numeric(1), dir = "forward"))
sum(vapply(dat, get_next_val, numeric(1), dir = "backward"))
