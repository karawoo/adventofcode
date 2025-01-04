library("readr")
library("combinat")
library("purrr")

dat <- read_lines("input12.txt") %>%
  strsplit(" ") %>%
  lapply(as.list)

rows <- lapply(dat, `[[`, 1) %>%
  lapply(function(x) strsplit(x, "")[[1]])

sizes <- lapply(dat, `[[`, 2) %>%
  lapply(function(x) strsplit(x, ",")[[1]]) %>%
  lapply(as.numeric)

get_candidates <- function(row, sizes) {
  n_unknown <- sum(row == "?")
  n_unknown_broken <- sum(sizes) - sum(row == "#")

  poss_vals <- c(
    rep("#", n_unknown_broken),
    rep(".", n_unknown - n_unknown_broken)
  )
  opts <- unique(permn(poss_vals))

  row_rep <- rep(list(row), length(opts))

  perms <- mapply(
    function(row, opt) {
      row[which(row == "?")] <- opt
      row
    },
    row_rep,
    opts,
    SIMPLIFY = FALSE
  )

  keep(perms, \(x) all(x == row | row == "?"))
}

is_candidate_consistent <- function(candidate, sizes) {
  cand <- gsub("\\.", "0", candidate) %>%
    gsub("\\#", "1", .) %>%
    as.numeric()
  groups <- split(cand, cumsum(c(1, diff(cand) != 0)))
  sums <- vapply(groups, sum, numeric(1))
  identical(unname(sums[sums != 0]), sizes)
}

are_candidates_consistent <- function(candidates, sizes) {
  sum(vapply(candidates, is_candidate_consistent, logical(1), sizes))
}

candidates <- mapply(get_candidates, rows, sizes)
sum(mapply(are_candidates_consistent, candidates, sizes))
