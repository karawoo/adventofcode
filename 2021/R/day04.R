#' Day 04: Giant Squid
#'
#' [Giant Squid](https://adventofcode.com/2021/day/4)
#'
#' @name day04
#' @rdname day04
#' @param numbers Numbers called so far
#' @param cards List of bingo cards
#' @return For Part One, `f04a(x)` returns .... For Part Two,
#'   `f04b(x)` returns ....
#' @export
#' @examples
#' f04a(get_numbers(), get_cards())
#' f04a(get_numbers(), get_cards())
f04a <- function(numbers, cards) {
  for (i in 5:length(numbers)) {
    for (card in cards) {
      res <- matrix(card %in% numbers[1:i], nrow = 5)
      if (any(rowMeans(res) == 1) | any(colMeans(res) == 1)) {
        return(score(card, numbers[1:i]))
      }
    }
  }
}

#' @rdname day04
#' @export
f04b <- function(numbers, cards) {
  when_it_won <- rep(NA, length(cards))
  for (i in seq_along(numbers)) {
    for (card in seq_along(cards)) {
      res <- matrix(cards[[card]] %in% numbers[1:i], nrow = 5)
      if (all(rowMeans(res) < 1) & all(colMeans(res) < 1)) {
        when_it_won[card] <- i
      }
    }
  }
  last_number <- max(when_it_won) + 1
  last_card <- cards[[which.max(when_it_won)]]
  score(last_card, numbers[1:last_number])
}

score <- function(card, numbers) {
  res <- matrix(card %in% numbers, nrow = 5)
  sum(card[!res]) * tail(numbers, 1)
}

#' @rdname day04
#' @export
get_numbers <- function() {
  as.numeric(
    strsplit(
      readLines(system.file("input04.txt", package = "adventofcode2021"), n = 1),
      ","
    )[[1]]
  )
}

#' @rdname day04
#' @export
get_cards <- function() {
  cards_df <- read.table(
    system.file("input04.txt", package = "adventofcode2021"),
    skip = 2
  )
  lapply(split(cards_df, rep(1:(nrow(cards_df) / 5), each = 5)), as.matrix)
}
