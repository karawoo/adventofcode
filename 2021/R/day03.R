#' Day 03: Binary Diagnostic
#'
#' [Binary Diagnostic](https://adventofcode.com/2021/day/3)
#'
#' @name day03
#' @rdname day03
#' @param x some data
#' @return For Part One, `f03a(x)` returns .... For Part Two,
#'   `f03b(x)` returns ....
#' @export
#' @examples
#' f03a(example_data_03())
#' f03b(example_data_03())
f03a <- function(x) {
  gamma <- strtoi(
    paste0(apply(x, MARGIN = 2, most_common), collapse = ""),
    base = 2
  )
  epsilon <- strtoi(
    paste0(apply(x, MARGIN = 2, least_common), collapse = ""),
    base = 2
  )
  gamma * epsilon
}

#' @rdname day03
#' @export
f03b <- function(x) {
  oxygen <- x
  co2 <- x

  i <- 1
  while(nrow(oxygen) > 1) {
    val <- most_common(oxygen[, i])
    oxygen <- oxygen[oxygen[, i] == val, ]
    i <- i + 1
  }

  i <- 1
  while(nrow(co2) > 1) {
    val <- least_common(co2[, i])
    co2 <- co2[co2[, i] == val, ]
    i <- i + 1
  }

  oxygen <- strtoi(paste0(oxygen, collapse = ""), base = 2)
  co2 <- strtoi(paste0(co2, collapse = ""), base = 2)
  oxygen * co2
}

most_common <- function(x) {
  x <- table(x)
  as.numeric(names(utils::tail(x[which(x == max(x))], 1)))
}

least_common <- function(x) {
  x <- table(x)
  as.numeric(names(utils::head(x[which(x == min(x))], 1)))
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export
example_data_03 <- function(example = 1) {
  utils::read.fwf(
    system.file("input03.txt", package = "adventofcode2021"),
    widths = rep(1, 12)
  )
}
