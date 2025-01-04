dat <- strsplit(readLines("input06.txt"), "")[[1]]

detect_start <- function(dat, sequence_length = 4) {
  for (i in sequence_length:length(dat)) {
    if (!any(duplicated(dat[(i-(sequence_length - 1)):i]))) {
      return(i)
    }
  }
}

# Part 1

detect_start(dat)

# Part 2

detect_start(dat, 14)
