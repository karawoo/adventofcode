#' Day 05: Hydrothermal Venture
#'
#' [Hydrothermal Venture](https://adventofcode.com/2021/day/5)
#'
#' @name day05
#' @rdname day05
#' @param x some data
#' @param include_diag Include diagonal lines? Defaults to FALSE
#' @return Number of intersections
#' @export
#' @examples
#' f05(example_data_05())
#' f05(example_data_05(), include_diag = TRUE)
f05 <- function(x, include_diag = FALSE) {
  segments <- tibble(x) %>%
    separate(x, into = c("start", "end"), sep = " -> ") %>%
    rownames_to_column(var = "segment") %>%
    pivot_longer(c(.data$start, .data$end)) %>%
    separate(.data$value, into = c("x", "y"), sep = ",") %>%
    group_by(.data$segment)

  if (!include_diag) {
    segments <- filter(
      segments,
      duplicated(.data$x)
      | duplicated(.data$x, fromLast = TRUE)
      | duplicated(.data$y)
      | duplicated(.data$y, fromLast = TRUE)
    )
  }

  summarize(
    segments,
    x = list(.data$x[1]:.data$x[2]),
    y = list(.data$y[1]:.data$y[2])
  ) %>%
    unnest(c(.data$x, .data$y)) %>%
    count(.data$x, .data$y) %>%
    filter(n > 1) %>%
    nrow()
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  readLines(system.file("input05.txt", package = "adventofcode2021"))
}
