library("readr")
library("igraph")
library("tidyverse")

dat <- read_fwf(
  "input16.txt",
  col_positions = fwf_widths(rep(1, nchar(readLines("input16.txt", n = 1))))
) |>
  as.matrix()

dimnames(dat) <- list(seq_len(nrow(dat)), seq_len(ncol(dat)))

long_dat <- as.data.frame.table(dat) |>
  rename(row = Var1, col = Var2, value = Freq) |>
  mutate(across(matches("^row|col"), as.numeric)) |>
  filter(value != "#") |>
  cross_join(data.frame(dir = c("n", "s", "e", "w"))) |>
  mutate(id = row_number())

dat_adj <- cross_join(long_dat, long_dat) |>
  filter(
    (abs(row.x - row.y) == 1 & col.x == col.y & dir.x == dir.y) |
      (row.x == row.y & abs(col.x - col.y) == 1 & dir.x == dir.y) |
      row.x == row.y & col.x == col.y
  ) |>
  mutate(
    weight = case_when(
      row.x == row.y & col.x == col.y & dir.x != dir.y ~ 1000,
      TRUE ~ 1
    )
  )

g <- graph_from_data_frame(
  dat_adj[, c("id.x", "id.y", "weight")],
  vertices = unique(long_dat[, c("id", "value", "row", "col", "dir")])
)

start <- which(V(g)$value == "S" & V(g)$dir == "e")
end <- which(V(g)$value == "E")
paths <- shortest_paths(g, start, end)
edges <- rep(paths$vpath[[1]], each = 2)[-1]
edges <- edges[-length(edges)]
E(g)$weight[get_edge_ids(g, as.character(edges))]

# This doesn't quite work because my graph isn't set up right. I need to have
# edges only between (a) nodes in the same position with different directions
# and (b) nodes that are adjacent in the correct direction (so a node at
# row=1,col=1,dir=E would connect to row=1,col=2,dir=E but not row=2,col=1)
