library("tidyverse")

file <- "input04.txt"

dat <- read_fwf(
  file,
  col_positions = fwf_widths(rep(1, nchar(readLines(file, n = 1))))
) |>
  as.matrix()

oob <- function(x, dat) {
  x == 0 | x > max(dim(dat)) # assumes square matrix
}

get_neighbors <- function(row, col, dat) {
  x <- row
  y <- col

  possible <- list(
    tl = c(row - 1, col - 1),
    t = c(row - 1, col),
    tr = c(row - 1, col + 1),
    l = c(row, col - 1),
    r = c(row, col + 1),
    bl = c(row + 1, col - 1),
    b = c(row + 1, col),
    br = c(row + 1, col + 1)
  )

  neighbors <- do.call(rbind, possible)
  colnames(neighbors) <- c("row", "col")
  neighbors[!oob(neighbors[, 1], dat) & !oob(neighbors[, 2], dat), ]
}

is_movable <- function(neighbors, dat) {
  sum(dat[as.matrix(neighbors)] == "@") < 4
}

rolls <- which(dat == "@")
rolls_xy <- arrayInd(rolls, dim(dat))
neighbors <- apply(rolls_xy, 1, function(x) get_neighbors(x[1], x[2], dat))

## part 1

neighbors |>
  map_lgl(is_movable, dat) |>
  sum()

## part 2

clear <- function(rolls, neighbors, dat) {
  moved <- neighbors |>
    map_lgl(is_movable, dat)

  dat[rolls[moved]] <- "."

  if (any(moved)) {
    clear(rolls[!moved], neighbors[!moved], dat)
  } else {
    return(dat)
  }
}

sum(dat == "@") - sum(clear(rolls, neighbors, dat) == "@")
