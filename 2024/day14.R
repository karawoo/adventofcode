library("readr")
library("stringr")
library("tidyverse")
library("DescTools")

dat <- read_delim("input14.txt", " ", col_names = c("pos", "vel"))

starts <- str_extract_all(dat$pos, "\\d+") |>
  map(as.numeric)

velocities <- str_extract_all(dat$vel, "-?\\d+") |>
  map(as.numeric)

dims <- c(101, 103)

find_robots <- function(starts, velocities, dims, times = 100) {
  end_pos <- map2(
    starts,
    velocities,
    \(x, y, dims, times) (x + y * times) %% dims,
    dims = dims,
    times = times
  )
  positions <- do.call(rbind, end_pos)
  colnames(positions) <- c("x", "y")
  as_tibble(positions)
}

safety_factor <- function(positions, dims) {
  positions |>
  mutate(
    quad = case_when(
      x < (dims[1] - 1) / 2 & y < (dims[2] - 1) / 2 ~ 1,
      x < (dims[1] - 1) / 2 & y > (dims[2] - 1) / 2 ~ 2,
      x > (dims[1] - 1) / 2 & y < (dims[2] - 1) / 2 ~ 3,
      x > (dims[1] - 1) / 2 & y > (dims[2] - 1) / 2 ~ 4,
      )
  ) |>
    filter(!is.na(quad)) |>
    count(quad) |>
    pull(n) |>
    prod()
}

## part 1
find_robots(starts, velocities, dims) |>
  safety_factor(dims)

## part 2 - find tree by finding grid with lowest entropy in the first 10,000
## seconds
create_grid <- function(starts, velocities, dims, times) {
  mat <- matrix(0, nrow = 103, ncol = 101)
  pos <- as.matrix(find_robots(starts, velocities, dims, times))
  for (i in seq_len(nrow(pos))) {
    mat[pos[i, "y"], pos[i, "x"]] <- mat[pos[i, "y"], pos[i, "x"]] + 1
  }
  mat
}

find_tree <- function(starts, velocities, dims, attempts = 10000) {
  entropies <- vector(mode = "double", length = attempts)
  for (i in seq_len(attempts)) {
    g <- create_grid(starts, velocities, dims, times = i)
    entropies[i] <- MutInf(g)
  }
  which.min(entropies)
}

find_tree(starts, velocities, dims, attempts = 10000)
