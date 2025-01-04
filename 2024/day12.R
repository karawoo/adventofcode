library("readr")
library("tidyverse")
library("igraph")

dat <- read_fwf(
  "input12.txt",
  col_positions = fwf_widths(rep(1, nchar(readLines("input12.txt", n = 1))))
) |>
  as.matrix()

dimnames(dat) <- list(seq_len(nrow(dat)), seq_len(ncol(dat)))

oob <- function(x, dat) {
  x == 0 | x > max(dim(dat))
}

adjacents <- function(row, col, dat, dim) {
  tribble(
    ~dir,    ~row,    ~col,
     "l",     row, col - 1,
     "r",     row, col + 1,
     "t", row - 1,     col,
     "b", row + 1,     col
  )
}

needs_fence <- function(row, col, dat, l) {
  oob(row, dat) || oob(col, dat) || dat[row, col] != l
}

get_perimeter <- function(idx, l, dat) {
  region <- arrayInd(idx, .dim = dim(dat))
  apply(
    region,
    1,
    function(x) {
      adjacents(x[1], x[2], dat = dat, dim = max(dim(dat)))
    },
    simplify = TRUE
  ) |>
    bind_rows() |>
    unique() |>
    rowwise() |>
    mutate(perim = needs_fence(row, col, dat, l)) |>
    filter(perim == TRUE) |>
    select(-perim)
}

sides_horiz <- function(dat) {
  arrange(dat, row, col) |>
    group_by(row) |>
    summarize(n = 1 + sum(diff(col) != 1)) |>
    pull(n) |>
    sum()
}

sides_vert <- function(dat) {
  arrange(dat, col, row) |>
    group_by(col) |>
    summarize(n = 1 + sum(diff(row) != 1)) |>
    pull(n) |>
    sum()
}

get_sides <- function(idx, l, dat) {
  perim <- get_perimeter(idx, l, dat)

  perim_sides <- split(perim, perim$dir)
  n_sides <- c()
  for (i in names(perim_sides)) {
    if (i %in% c("t", "b")) {
      n_sides <- c(n_sides, sides_horiz(perim_sides[[i]]))
    } else if (i %in% c("l", "r")) {
      n_sides <- c(n_sides, sides_vert(perim_sides[[i]]))
    }
  }
  sum(n_sides)
}

## modify code from day 10 to convert to graph (expand_grid + two joins doesn't
## work so well on a matrix this size...)
long_dat <- as.data.frame.table(dat) |>
  rename(row = Var1, col = Var2, value = Freq) |>
  mutate(idx = row_number())

## Find neighbors so we can just join on those
rowcols <- which(dat == dat, arr.ind = TRUE)
d <- as.matrix(dist(rowcols, method = "manhattan", diag = TRUE, upper = TRUE))
neighbors_list <- apply(d, 1, \(x) which(x == 1))
names(neighbors_list) <- seq_along(dat)

neighbors <- neighbors_list |>
  map_dfr(\(x) tibble(idy = x), .id = "idx") |>
  mutate(idx = as.numeric(idx)) |>
  mutate(value = dat[idy])

## graph of indices that are neighbors and share the same value -- these form
## our regions
dat_adj <- long_dat |>
  left_join(neighbors, by = "idx") |>
  filter(value.x == value.y)

g <- graph_from_data_frame(
  dat_adj[, c("idx", "idy")],
  vertices = unique(long_dat[, c("idx", "value")])
)

## extract regions
regions <- split(V(g)$name, components(g)$membership) |>
  map(as.numeric)

names(regions) <- map_chr(regions, \(x, g) vertex_attr(g, "value", x[[1]]), g)

## part 1
perimeters <- imap(regions, \(idx, l) get_perimeter(idx, l, dat)) |>
  map_dbl(nrow)
areas <- map(regions, length)
sum(mapply(prod, perimeters, areas))

## part 2
sides <- imap(regions, \(idx, l) get_sides(idx, l, dat))
sum(mapply(prod, sides, areas))
