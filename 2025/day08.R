library("dplyr")
library("igraph")

dat <- read.table("input08.txt", sep = ",", col.names = c("x", "y", "z"))

dm <- as.matrix(dist(dat, method = "euclidean"))
xy <- t(combn(rownames(dat), 2))
dm_df <- data.frame(xy, dist = dm[xy]) |>
  arrange(dist)

g <- make_empty_graph(n = nrow(dat), directed = FALSE)

## part 1

n <- 1000
for (i in seq_len(n)) {
  g <- add_edges(g, c(dm_df[i, "X1"], dm_df[i, "X2"]))
}

prod(head(sort(components(g)$csize, decreasing = TRUE), 3))

## part 2

i <- n # continue where we left off on the loop above
while (!is_connected(g)) {
  i <- i + 1
  g <- add_edges(g, c(dm_df[i, "X1"], dm_df[i, "X2"]))
}

prod(dat[c(dm_df[i, "X1"], dm_df[i, "X2"]), "x"])
