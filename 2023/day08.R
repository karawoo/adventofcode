library("readr")
library("stringr")
library("pracma")

Map <- R6::R6Class("Map", list(
  instructions = c(),
  map = list(),
  position = "AAA",
  step_count = 0,
  inst_count = 1,

  read_input = function() {
    dat <- read_lines("input08.txt")
    self$instructions <- str_split(dat[[1]], "")[[1]]
    map <- dat[3:length(dat)]
    map_names <- unlist(str_extract_all(map, "^[A-Z]{3}"))
    self$map <- map %>%
      str_remove_all("^[A-Z]{3} = \\(|\\)") %>%
      str_split("(, )") %>%
      setNames(map_names) %>%
      lapply(setNames, c("L", "R"))
    invisible(self)
  },

  new_position = function(instruction) {
    self$position <- self$map[[self$position]][[instruction]]
    self$step_count <- self$step_count + 1
    self$inst_count <- ifelse(
      self$inst_count == length(self$instructions),
      1,
      self$inst_count + 1
    )
    invisible(self)
  },

  reset_counts = function() {
    self$step_count <- 0
    self$inst_count <- 1
  },

  run = function(part = c(1, 2)) {
    self$read_input()
    if (part == 1) {
      while (self$position != "ZZZ") {
        self$new_position(self$instructions[self$inst_count])
      }
    } else {
      starting_positions <- na.omit(str_extract(names(self$map), "[A-Z]{2}A$"))
      counts <- vector(mode = "numeric", length = length(starting_positions))
      for (i in seq_along(starting_positions)) {
        self$position <- starting_positions[i]
        self$reset_counts()
        while (!str_detect(self$position, "Z$")) {
          self$new_position(self$instructions[self$inst_count])
        }
        counts[i] <- self$step_count
      }
      self$step_count <- Reduce(pracma::Lcm, counts)
    }
    self$step_count
  }
))

m1 <- Map$new()
m1$run(part = 1)
m2 <- Map$new()
m2$run(part = 2)
print(m2$step_count, digits = 14)
