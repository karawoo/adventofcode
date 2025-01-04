library("readr")
dat <- read_fwf("input01.txt", col_types = "nn")

## part 1
sum(abs(sort(dat$X2) - sort(dat$X1)))

## part 2
sum(sapply(dat$X1, \(x) x * sum(dat$X2 == x)))
