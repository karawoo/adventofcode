#' Day 07: The Treachery of Whales
#'
#' [The Treachery of Whales](https://adventofcode.com/2021/day/7)
#'
#' @name day07
#' @rdname day07
#' @param x some data
#' @return For Part One, `f07a(x)` returns .... For Part Two,
#'   `f07b(x)` returns ....
#' @export
#' @examples
#' f07a(input_07())
#' f07b(input_07())
f07a <- function(x) {
  sum(abs(x - median(x)))
}


#' @rdname day07
#' @export
f07b <- function(x) {
  positions <- seq(min(x), max(x))
  res <- lapply(positions, function(x, y) sum(fuel(abs(x - y))), x = x)
  res[[which.min(res)]]
}

fuel <- function(x) {
  (x + 1) * (x / 2)
}
