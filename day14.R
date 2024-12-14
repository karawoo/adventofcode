library("readr")
library("stringr")
library("tidyverse")
library("R6")

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
    times = 100
  )
  do.call(rbind, end_pos) |>
    as_tibble(.name_repair = "unique") |>
    rename(x = "...1", y = "...2")
}

## part 1
find_robots(starts, velocities, dims) |>
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
