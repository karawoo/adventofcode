library("readr")
library("stringr")
library("purrr")

dat_orig <- read_file("input05.txt") %>%
  str_split("\\n+")

dat_orig <- dat_orig[[1]][!dat_orig[[1]] == ""]

split_data <- function(x) {
  split_points <- which(c(1, diff(cumsum(str_detect(x, "map")))) == 1)
  dat_list <- split(x, cumsum(seq_along(x) %in% split_points))
  names(dat_list)[[1]] <- "seeds:"
  dat_list[[1]] <- str_remove(dat_list[[1]], "seeds: ")
  for (i in 2:length(dat_list)) {
    names(dat_list)[[i]] <- dat_list[[i]][[1]]
    dat_list[[i]] <- dat_list[[i]][-1]
  }
  dat_list
}

dat <- dat_orig %>%
  split_data() %>%
  map(str_split, " +") %>%
  map_depth(.depth = 2, as.numeric)

map_input_to_dest <- function(input, map, direction = "forward") {
  result <- input
  for (i in map) {
    if (direction == "forward") {
      if (i[[2]] <= input && input <= i[[2]] + i[[3]]) {
        result <- i[[1]] + input - i[[2]]
        break
      }
    } else if (direction == "backward") {
      if (i[[1]] <= input && input <= i[[1]] + i[[3]]) {
        result <- input + i[[2]] - i[[1]]
        break
      }
    }
  }
  result
}

## Part 1
results <- vector(mode = "list", length = length(dat))

results[[1]] <- dat[[1]][[1]]

for (i in 2:length(dat)) {
  results[[i]] <- vapply(results[[i - 1]], map_input_to_dest, numeric(1), map = dat[[i]])
}

min(results[[length(results)]])

## Part 2
seed_from_location <- function(loc_num, almanac) {
  result <- loc_num
  for (i in rev(seq_along(dat))[-length(almanac)]) {
    result <- map_input_to_dest(
      result,
      almanac[[i]],
      direction = "backward"
    )
  }
  result
}


seed_ranges <- matrix(dat[[1]][[1]], ncol = 2, byrow = TRUE)

seed_in_range <- function(seed, mat) {
  any(
    apply(
      mat,
      function(x) seed >= x[1] && seed <= (x[1] + x[2]),
      MARGIN = 1
    )
  )
}

seed <- 0
location <- 0
while (!seed_in_range(seed, seed_ranges)) {
  if (location %% 500000 == 0) {
    print(paste0("Current location is ", scales::label_comma()(location)))
  }
  location <- location + 1
  seed <- seed_from_location(location, dat)
}
location
