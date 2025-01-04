#' Day 01: Sonar Sweep
#'
#' [Sonar Sweep](https://adventofcode.com/2021/day/1)
#'
#' @name day01
#' @rdname day01
#' @param x some data
#' @return For Part One, `f01a(x)` returns .... For Part Two,
#'   `f01b(x)` returns ....
#' @export
#' @examples
#' f01a(example_data_01())
#' f01b(example_data_01())
f01a <- function(x) {
  sum(diff(x) > 0)
}

#' @rdname day01
#' @export
f01b <- function(x) {
  stop()
  sum(diff(x, lag = 3) > 0)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day01
#' @export
example_data_01 <- function(example = 1) {
  as.numeric(readLines(system.file("input01.txt", package = "adventofcode2021")))
}
