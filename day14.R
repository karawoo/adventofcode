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

Robot <- R6Class(
  "Robot",
  private = list(
    vel = NULL,
    dims = NULL,
    one_move = function() {
      pos <- self$pos + private$vel
      for (i in seq_along(pos)) {
        if (pos[i] > private$dims[i] - 1) {
          pos[i] <- pos[i] - private$dims[i]
        } else if (pos[i] < 0) {
          pos[i] <- private$dims[i]  + pos[i]
        }
      }
      self$pos <- pos
    }
  ),
  public = list(
    pos = NULL,
    initialize = function(start, vel, dims) {
      self$pos <- start
      private$vel <- vel
      private$dims <- dims
    },
    move = function(times) {
      replicate(times, private$one_move())
      self$pos
    }
  )
)

find_robot <- function(start, vel, dims, times = 100) {
  r <- Robot$new(start, vel, dims)
  r$move(times)
}

find_robots <- function(starts, velocities, dims, times = 100) {
  end_pos <- end_pos <- map2(
    starts,
    velocities,
    find_robot,
    dims = dims,
    times = 100
  )
  do.call(rbind, end_pos) |>
    as_tibble() |>
    rename(x = V1, y = V2)
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
