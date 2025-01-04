#' Day 06: Lanternfish
#'
#' [Lanternfish](https://adventofcode.com/2021/day/6)
#'
#' @name day06
#' @rdname day06
#' @param x some data
#' @param days number of days to simulate
#' @export
#' @examples
#' f06a(input_06())
#' f06b(input_06())
f06a <- function(x, days = 80) {
  for (i in seq_len(days)) {
    if (any(x == 0)) {
      x <- c(x, rep(9, sum(x == 0)))
    }
    x <- x - 1
    x[x == -1] <- 6
  }
  length(x)
}

#' @rdname day06
#' @export
f06b <- function(x, days = 256) {
  x <- table(x)
  x[setdiff(as.character(0:8), names(x))] <- 0
  x <- x[sort(names(x))]
  for (i in seq_len(days)) {
    tmp <- x["0"]
    names(x) <- names(x)[c(length(names(x)), 1:length(names(x)) - 1)]
    x["6"] <- tmp + x["6"]
  }
  sum(x)
}
