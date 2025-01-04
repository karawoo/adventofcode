dat <- readLines("input15.txt")
dat <- strsplit(dat, ",")[[1]]

hash <- function(step) {
  spl <- strsplit(step, "")[[1]]
  ascii_codes <- vapply(spl, utf8ToInt, numeric(1))
  val <- 0
  for (i in ascii_codes) {
    val <- ((val + i) * 17) %% 256
  }
  val
}

## Part 1
sum(vapply(dat, hash, numeric(1)))

## Part 2
dat2 <- strsplit(dat, "=|-")

boxify <- function(step, boxes) {
  ind <- hash(step[[1]]) + 1
  if (length(step) == 1) {
    boxes[[ind]][[step]] <- NULL
  } else {
    if (step[[1]] %in% names(boxes[[ind]])) {
      boxes[[ind]][[step[[1]]]] <- step[[2]]
    } else {
      lens <- list(step[[2]])
      names(lens) <- step[[1]]
      boxes[[ind]] <- c(boxes[[ind]], lens)
    }
  }
  boxes
}

bx <- replicate(256, list())
for (i in dat2) {
  bx <- boxify(i, bx)
}

power <- 0
for (i in seq_along(bx)) {
  for (j in seq_along(bx[[i]])) {
    power <- power + as.numeric(bx[[i]][[j]]) * j * i
  }
}
power
