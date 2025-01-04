library("stringr")
library("purrr")
library("gtools")
library("parallel")
library("parallelly")

## Version of day 7 that has a few speed-ups:

## - switch() instead of do.call() (and only calls as.numeric() on the output of
##   the paste0() operation)
## - calculate permutations for each length of input only once per part and
##   reuse them
## - in part 2, only look at the ones that weren't already successfully matched
##   to results in part 1

dat <- readLines("input07.txt") |>
  str_split(":?\\s") |>
  map(as.numeric)

results <- map(dat, `[[`, 1)
eqs <- map(dat, \(x) x[2:length(x)])

get_all_perms <- function(length, operators) {
  permutations(
    length(operators),
    length,
    operators,
    repeats.allowed = TRUE
  )
}

calculate <- function(x, y, fun = c("+", "*", "||")) {
  switch(
    fun,
    "+" = x + y,
    "*" = x * y,
    "||" = as.numeric(paste0(x, y)),
    stop("oh no!")
  )
}

find_true_eq <- function(input, result, operators, all_perms) {
  perms <- all_perms[[as.character(length(input) - 1)]]
  for (i in seq_len(nrow(perms))) {
    output <- reduce2(input, perms[i, ], calculate)
    if (isTRUE(output == result)) {
      return(result)
    }
  }
}

lengths <- unique(map_dbl(eqs, \(x) length(x) - 1))

## part 1
all_perms1 <- map(set_names(lengths), get_all_perms, c("+", "*"))
p1 <- mapply(
  find_true_eq,
  eqs,
  results,
  MoreArgs = list(
    all_perms = all_perms1,
    operators = c("sum", "prod")
  )
)
sum(unlist(p1))

## part 2
keep_trying <- map_lgl(p1, is.null)
all_perms2 <- map(set_names(lengths), get_all_perms, c("+", "*", "||"))
p2 <- mcmapply(
  find_true_eq,
  eqs[keep_trying],
  results[keep_trying],
  MoreArgs = list(
    all_perms = all_perms2,
    operators = c("sum", "prod", "||")
  ),
  mc.cores = availableCores()
)

sum(unlist(p2), unlist(p1[!keep_trying]))
