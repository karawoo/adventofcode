dat <- readLines("input05.txt")

rules <- dat[1:which(dat == "") - 1] |>
  strsplit("\\|") |>
  lapply(as.numeric)

updates <- dat[(which(dat == "") + 1):length(dat)] |>
  strsplit(",") |>
  lapply(as.numeric)

is_ok <- function(u, rules) {
  for (rule in rules) {
    if (isTRUE(which(u == rule[1]) > which(u == rule[2]))) {
      return(FALSE)
    }
  }
  TRUE
}

## part 1

sapply(updates, \(x) ifelse(is_ok(x, rules), x[ceiling(length(x) / 2)], 0)) |>
  sum()

## part 2

bad <- updates[sapply(updates, \(x) !is_ok(x, rules))]

sum <- 0
for (update in bad) {
  for (i in seq_len(length(update) - 1)) {
    for (j in 2:length(update)) {
      if (!is_ok(update[(j-1):j], rules)) {
        update[(j-1):j] <- rev(update[(j-1):j])
      }
    }
  }
  sum <- sum + update[ceiling(length(update) / 2)]
}
sum
