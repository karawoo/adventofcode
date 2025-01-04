library("stringr")
library("purrr")

dat <- readLines("input03.txt")

run_program <- function(data) {
  str_extract_all(data, "(?<=mul\\()\\d{1,3},\\d{1,3}(?=\\))") |>
    unlist() |>
    str_split(",") |>
    map_dbl(\(x) prod(as.numeric(x))) |>
    sum()
}

## part 1
run_program(dat)

## part 2
str_split(paste(dat, collapse = ""), "do\\(\\)")[[1]] |>
  str_split_i("(don't\\(\\)|$)", 1) |>
  run_program()
