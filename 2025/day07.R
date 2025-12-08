library("readr")
library("dplyr")
library("igraph")

file <- "example.txt"
# file <- "input07.txt"
dat <- read_fwf(file, col_positions = fwf_widths(rep(1, nchar(readLines(file)[1])))) |>
  as.matrix()

## part 1

count <- 0
for (row in seq_len(nrow(dat))) {
  for (col in seq_along(dat[row, ])) {
    if (dat[row, col] == "^") {
      if (dat[row - 1, col] == "|") {
        dat[row, col - 1] <- "|"
        dat[row, col + 1] <- "|"
        count <- count + 1
      } else {
        if (dat[row - 1, col] %in% c("S", "|")) {
          dat[row, col] <- "|"
        }
      }
    } else {
      if (isTRUE(dat[row - 1, col] %in% c("S", "|"))) {
        dat[row, col] <- "|"
      }
    }
  }
}
count

## write.table(dat, "tmp.txt", quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)

## part 2

indArray <- function(i, dat) {
  matrix(seq_len(ncol(dat) * nrow(dat)), ncol = ncol(dat), nrow = nrow(dat))[i[1], i[2]]
}

splitters <- which(dat == "^")
edges <- data.frame(from = c(), to = c())

# replace ^ with an index so we can keep track of vertices
dat[which(dat == "^")] <- which(dat == "^")

## write for debugging
## dat_out <- stringr::str_pad(dat, 3, "both", " ")
## dat_out <- matrix(dat_out, ncol = ncol(dat), nrow = nrow(dat))
## write.table(dat_out, "tmp2.txt", sep = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

# also need dummy rows at the bottom
dat[nrow(dat), ] <- rep("end", ncol(dat))

for (row in seq_len(nrow(dat) - 1)) {
  for (col in seq_along(dat[row, ])) {
    if (grepl("\\d+", dat[row, col]) && dat[row - 1, col] == "|") {
      # if (dat[row, col] == "155") browser()
      # what splitters does this beam connect, if any?
      l <- dat[(row + 1):nrow(dat), col - 1]
      r <- dat[(row + 1):nrow(dat), col + 1]
      edges <- rbind(edges, data.frame(from = dat[row, col], to = l[which(grepl("\\d+", l))[1]]))
      edges <- rbind(edges, data.frame(from = dat[row, col], to = r[which(grepl("\\d+", r))[1]]))
      # also add edges to the end of the manifold
      l <- paste0(l, collapse = "")
      r <- paste0(r, collapse = "")
      if (grepl("^\\|*end", l)) {
        edges <- rbind(edges, data.frame(from = dat[row, col], to = "end"))
      }
      if (grepl("^\\|*end", r)) {
        edges <- rbind(edges, data.frame(from = dat[row, col], to = "end"))
      }
    }
  }
}

edges <- edges |>
  filter(!is.na(to))

g <- graph_from_data_frame(edges, vertices = c(splitters, "end", "S"))

length(all_simple_paths(g, from = "115"))
