library("readr")
library("tidyverse")
library("igraph")

#file <- "example.txt"
file <- "input10.txt"

dat <- read_fwf(
  file,
  col_positions = fwf_widths(rep(1, nchar(readLines(file, n = 1))))
) |>
  as.matrix()

## Create graph where each node is connected to adjacent squares in the matrix
## that have a value of n+1

long_dat <- as.data.frame.table(dat) |>
  rename(row = Var1, col = Var2, value = Freq) |>
  mutate(id = paste0(row, ",", col))

dat_adj <- expand_grid(x = long_dat$id, y = long_dat$id) |>
  left_join(long_dat, by = join_by(x == id)) |>
  left_join(long_dat, by = join_by(y == id)) |>
  mutate(across(matches("^(row|col)"), as.numeric)) |>
  filter(
    abs(col.y - col.x) == 1 & row.y == row.x |
      abs(row.y - row.x) == 1 & col.y == col.x
  ) |>
  filter(value.y - value.x == 1)

g <- graph_from_data_frame(
  dat_adj[, c("x", "y")],
  vertices = unique(long_dat[, c("id", "value")])
)

starts <- which(vertex_attr(g, "value") == 0)

## part 1
count_9s <- function(start, g) {
  sum(vertex_attr(g, "value", subcomponent(g, start, "out")) == 9)
}

map_dbl(starts, count_9s, g) |>
  sum()

## part 2
count_paths <- function(start, g) {
  sub <- subcomponent(g, start, "out")
  ends <- sub[which(sub$value == 9)]
  paths <- 0
  if (length(ends) > 0) {
    for (i in seq_along(ends)) {
      paths <- paths + length(all_simple_paths(g, start, ends[i]))
    }
  }
  paths
}

map_dbl(starts, count_paths, g) |>
  sum()
