library("purrr")

dat <- readLines("input09.txt") |>
  strsplit("") |>
  unlist() |>
  as.numeric()

spaces <- dat[c(FALSE, TRUE)]
blocks <- dat[c(TRUE, FALSE)]
spaces <- setNames(spaces, seq_along(spaces) - 1)
blocks <- setNames(blocks, seq_along(blocks) - 1)

files <- rep(seq_along(blocks) - 1, blocks)

b <- imap(
  split(files, files),
  \(x, idx) c(x, rep(NA, max(spaces[as.numeric(idx) + 1], 0, na.rm = TRUE)))) |>
  unlist()

## part 1
to_move <- rev(tail(na.omit(b), sum(is.na(b))))
compacted <- b
compacted[is.na(compacted)] <- to_move
compacted <- compacted[1:(length(compacted) - length(to_move))]
sum((seq_along(compacted) - 1) * compacted, na.rm = TRUE)

## part 2

find_fit <- function(b, space_size) {
  result <- c()
  for (i in seq_along(b)) {
    if (b[i] > space_size) {
      next
    } else if (sum(result) == space_size) {
      break
    } else {
      result <- c(b[i], find_fit(b[-i], space_size - b[i]))
      break
    }
  }
  result
}

fill_ins <- vector(mode = "list", length(spaces))
x <- rev(blocks)

for (i in seq_along(spaces)) {
  fits <- find_fit(x, space_size = spaces[i])
  fit_expanded <- as.numeric(rep(names(fits), fits))
  fill_ins[[i]] <- c(fit_expanded, rep(NA, spaces[i] - length(fit_expanded)))
  x <- x[setdiff(names(x), names(fits))]
}

fill_ins <- unlist(fill_ins)

compacted2 <- b

compacted2[!is.na(compacted2) & compacted2 %in% fill_ins] <- 0
compacted2[is.na(compacted2)] <- fill_ins

sum((seq_along(compacted2) - 1) * compacted2, na.rm = TRUE)
