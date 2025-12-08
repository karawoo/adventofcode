library("readr")
library("purrr")
library("dplyr")
options(scipen = 9999)

file <- "input06.txt"
orig <- readLines(file)
ncol <- length(strsplit(orig[[1]], "\\s+")[[1]])

## part 1

dat1 <- read_fwf(file, n_max = length(orig) - 1) |>
  mutate(across(everything(), as.numeric))

ops <- tail(orig, 1) |>
  strsplit("\\s+")

ops <- ops[[1]]

math <- function(x, op) {
  init <- ifelse(op == "*", 1, 0)
  Reduce(function(x, y) do.call(op, as.list(c(x, y))), x = x, init = init)
}

sum(mapply(math, dat1, ops))

## part 2

# read the data in one character per column
dat2 <- read_fwf(file, col_positions = fwf_widths(rep(1, max(nchar(orig)))))
dat2 <- dat2[-nrow(dat2), ]

# split into groups based on where there's a column of all NAs
seps <- map_dbl(dat2, ~ all(is.na(.x)))
splits <- cumsum(c(1, diff(seps) != 0))
split_problems <- split.default(dat2, splits) |>
  keep(~ !all(is.na(.x)))

ceph_math <- function(x, op) {
  vals <- as.list(x) |>
    map(~ paste0(na.omit(.x), collapse = "")) |>
    map_dbl(as.numeric)

  math(vals, op)
}

sum(mapply(ceph_math, split_problems, ops))
