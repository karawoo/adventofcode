library("stringr")
library("readr")
library("purrr")

dat <- read_fwf("input04.txt", col_positions = fwf_widths(rep(1, 140)))|>
  as.matrix()

## part 1

## get horizontal, vertical, and diagonal lines in the matrix
h <- apply(dat, 1, paste, collapse = "")
v <- apply(dat, 2, paste, collapse = "")
d1 <- row(dat) - col(dat)
d2 <- row(dat) + col(dat)
diag1 <- split(dat, d1) |> map_chr(paste, collapse = "")
diag2 <- split(dat, d2) |> map_chr(paste, collapse = "")
all_ways <- c(h, v, diag1, diag2)

sum(str_count(all_ways, "XMAS"), str_count(all_ways, "SAMX"))

## part 2

## find the indices of the letter A when it's not in the outer rows & columns
a <- which(dat == "A", arr.ind = TRUE)
a_interior <- a[apply(a, 1, \(x) !any(x %in% c(1, 140))), ]

## get the diagonals crossing each A
get_diagonals <- function(idx, dat) {
  rows <- seq(idx["row"] - 1, idx["row"] + 1)
  cols <- seq(idx["col"] - 1, idx["col"] + 1)
  diags <- list(
    mapply(\(x, y) dat[x, y], rows, cols),
    mapply(\(x, y) dat[x, y], rows, rev(cols))
  )
  map_chr(diags, paste, collapse = "")
}

diags <- apply(a_interior, 1, get_diagonals, dat = dat, simplify = FALSE)

sum(map_lgl(diags, \(x) all(x %in% c("MAS", "SAM"))))
