dat <- readLines("input05.txt")
options(scipen = 9999)

ranges <- dat[1:(which(dat == "") - 1)] |>
  strsplit("-") |>
  lapply(as.numeric)

ingredients <- as.numeric(dat[(which(dat == "") + 1):length(dat)])

## part 1

count <- 0
for (i in ingredients) {
  for (j in ranges) {
    if (i >= j[1] && i <= j[2]) {
      count <- count + 1
      break
    }
  }
}
count

## part 2

sorted_ranges <- ranges[order(sapply(ranges, `[[`, 1))]
highest <- 0
count <- 0

for (i in sorted_ranges) {
  if (i[2] <= highest) {
    next
  }
  if (i[1] > highest) {
    count <- count + (i[2] - i[1]) + 1
  } else {
    count <- count + (i[2] - (highest))
  }
  highest <- i[2]
}
count
