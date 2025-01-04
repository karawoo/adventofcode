library("tidyverse")

dat <- read_fwf("input05.txt", n_max = 8)

instr <- readLines("input05.txt")
instr <- instr[11:length(instr)]

move <- function(instruction, stacks, cratemover = 9000) {
  nums <- as.numeric(str_extract_all(instruction, "\\d+")[[1]])
  names(nums) <- c("move", "from", "to")

  if (cratemover == 9000) {
    to_move <- rev(tail(stacks[[nums["from"]]], nums["move"]))
  } else if (cratemover == 9001) {
    to_move <- tail(stacks[[nums["from"]]], nums["move"])
  }

  stacks[[nums["to"]]] <- c(
    stacks[[nums["to"]]],
    to_move
  )
  stacks[[nums["from"]]] <- head(stacks[[nums["from"]]], -as.numeric(nums["move"]))
  stacks
}

# Part 1

stacks <- as.list(dat) %>%
  lapply(function(x) rev(x[!is.na(x)]))

for (i in seq_along(instr)) {
  stacks <- move(instr[i], stacks)
}

# Part 2

stacks <- as.list(dat) %>%
  lapply(function(x) rev(x[!is.na(x)]))

for (i in seq_along(instr)) {
  stacks <- move(instr[i], stacks, cratemover = 9001)
}
