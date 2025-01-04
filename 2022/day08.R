library("readr")
dat <- as.matrix(read_fwf("input08.txt", fwf_widths(widths = rep(1, 99))))

# Part 1

visible <- function(x) {
  results <- vector(mode = "logical", length = length(x))

  for (i in seq_along(x)) {
    if (i == 1 || i == length(x)) {
      results[i] <- TRUE
    } else if (all(x[i] > x[1:(i-1)]) || all(x[i] > x[(i+1):length(x)])) {
      results[i] <- TRUE
    } else {
      results[i] <- FALSE
    }
  }
  results
}

sum(t(apply(dat, 1, visible)) | apply(dat, 2, visible))

# Part 2

scenic <- function(x) {
  r <- vector(mode = "numeric", length = length(x))
  l <- vector(mode = "numeric", length = length(x))

  for (i in seq_along(x)) {
    if(i == 1) {
      l[i] <- 0
    } else {
      ldist <- which(x[(i-1):1] >= x[i])[1]
      l[i] <- ifelse(is.na(ldist), i - 1, ldist)
    }
    if (i == length(x)) {
      r[length(x)] <- 0
    } else {
      rdist <- which(x[(i+1):length(x)] >= x[i])[1]
      r[i] <- ifelse(is.na(rdist), length(x) - i, rdist)
    }
  }

  l * r
}

max(t(apply(dat, 1, scenic)) * apply(dat, 2, scenic))
