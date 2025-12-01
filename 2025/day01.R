dat <- readLines("input01.txt")

x <- as.numeric(substring(dat, 2))
dir <- ifelse(substring(dat, 0, 1) == "R", 1, -1)

## part 1

position <- function(start, move) {
  (start + move) %% 100
}

positions <- Reduce(position, x * dir, init = 50, accumulate = TRUE)
sum(positions == 0)

## part 2

count <- 0
pos <- 50

for (i in x * dir) {
  new <- pos + i
  passed_zero <- floor(abs(new) / 100) + (sign(new) != sign(pos) && sign(pos) != 0)
  count <- count + passed_zero
  pos <- new %% 100
}

count
