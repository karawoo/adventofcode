library("tidyverse")

dat <- read_table("input02.txt", col_names = FALSE)

# Part 1

outcomes <- dat %>%
  mutate(
    X2 = case_when(
      X2 == "X" ~ "A",
      X2 == "Y" ~ "B",
      X2 == "Z" ~ "C"
    )
  ) %>%
  mutate(
    outcome = case_when(
      X1 == X2 ~ 3,
      X1 == "C" & X2 == "A" ~ 6,
      X2 == "C" & X1 == "A" ~ 0,
      X2 > X1 ~ 6,
      TRUE ~ 0
    ),
    shape_score = case_when(
      X2 == "A" ~ 1,
      X2 == "B" ~ 2,
      X2 == "C" ~ 3
    )
  ) %>%
  rowwise() %>%
  mutate(sum = sum(outcome, shape_score))

sum(outcomes$sum)

# Part 2

choose <- function(them, outcome) {
  mat <- matrix(
    c("Y", "X", "Z", "Z", "Y", "X", "X", "Z", "Y"),
    nrow = 3,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )
  names(which(mat[, them] == outcome))
}

outcomes <- dat %>%
  mutate(
    outcome = case_when(
      X2 == "X" ~ 0,
      X2 == "Y" ~ 3,
      X2 == "Z" ~ 6
    )
  ) %>%
  rowwise() %>%
  mutate(
    shape = choose(X1, X2),
    shape_score = case_when(
      shape == "A" ~ 1,
      shape == "B" ~ 2,
      shape == "C" ~ 3
    )
  ) %>%
  rowwise() %>%
  mutate(sum = sum(outcome, shape_score))

sum(outcomes$sum)
