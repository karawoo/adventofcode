library("readr")
options(scipen = 9999)

file <- "input07.txt"
dat <- read_fwf(
  file,
  col_positions = fwf_widths(rep(1, nchar(readLines(file)[1])))
) |>
  as.matrix()

count <- 0
tl <- vector(mode = "numeric", length = ncol(dat))
tl[which(dat == "S", arr.ind = TRUE)[, "col"]] <- 1

for (row in seq_len(nrow(dat))) {
  for (col in seq_along(dat[row, ])) {
    if (dat[row, col] == "^") {
      if (tl[col] > 0) {
        count <- count + 1
        tl[col - 1] <- tl[col - 1] + tl[col]
        tl[col + 1] <- tl[col + 1] + tl[col]
        tl[col] <- 0
      }
    }
  }
}

## part 1
count

## part 2
sum(tl)
