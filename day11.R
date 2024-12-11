library("purrr")

dat <- readLines("input11.txt") |>
  strsplit(" ")

dat <- as.numeric(dat[[1]])

change <- function(stone) {
  char <- as.character(stone)
  if (stone == 0) {
    return(1)
  } else if (nchar(char) %% 2 == 0) {
    return(
      c(
        as.numeric(substr(char, 1, nchar(char) / 2)),
        as.numeric(substr(char, nchar(char) / 2 + 1, nchar(char)))
      )
    )
  } else {
    return(stone * 2024)
  }
}

blink <- function(dat, times) {
  x <- table(dat)
  for (i in seq_len(times)) {
    tmp <- imap(x, function(x, idx) {
        new <- change(as.numeric(idx))
        setNames(rep(x, length(new)), new)
    }) |>
      unname() |>
      unlist()
    x <- tapply(tmp, names(tmp), sum)
  }
  x
}

## part 1
sum(blink(dat, 25))

## part 2
sum(blink(dat, 75))
