library("readr")

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
