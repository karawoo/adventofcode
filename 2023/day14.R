library("stringr")
library("purrr")

dat <- readLines("input14.txt") %>%
  str_split("", simplify = TRUE)

tilt <- function(dish) {
  apply(dish, 2, paste0, collapse = "") %>%
    str_split("#") %>%
    map(\(x) str_split(x, "")) %>%
    map_depth(2, sort, decreasing = TRUE) %>%
    map(\(x) paste0(map_chr(x, paste0, collapse = ""), collapse = "#"))
}

## Part 1
tilt(dat) %>%
  str_split("") %>%
  map_dbl(\(x) sum(length(x) - which(x == "O") + 1)) %>%
  sum()
