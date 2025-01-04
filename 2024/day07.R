library("stringr")
library("purrr")
library("gtools")
library("parallel")
library("parallelly")

dat <- readLines("input07.txt") |>
  str_split(":?\\s") |>
  map(as.numeric)

results <- map(dat, `[[`, 1)
eqs <- map(dat, \(x) x[2:length(x)])

calculate <- function(x, y, fun = c("sum", "prod", "paste0")) {
  as.numeric(do.call(fun, list(x, y)))
}

find_true_eq <- function(input, result, operators) {
  perms <- permutations(
    length(operators),
    length(input) - 1,
    operators,
    repeats.allowed = TRUE
  )
  for (i in seq_len(nrow(perms))) {
    output <- reduce2(input, perms[i, ], calculate)
    if (isTRUE(output == result)) {
      return(result)
    }
  }
}

## part 1
p1 <- mapply(
  find_true_eq,
  eqs,
  results,
  MoreArgs = list(operators = c("sum", "prod"))
)
sum(unlist(p1))

## part 2
p2 <- mcmapply(
  find_true_eq,
  eqs,
  results,
  MoreArgs = list(operators = c("sum", "prod", "paste0")),
  mc.cores = availableCores()
)
sum(unlist(p2))
