dat <- readLines("input07.txt")

ls <- which(grepl("^\\$ ls", dat))

for (i in ls) {
  contents <- dat[i+1]
}

which(grepl("^\\d+", dat))
