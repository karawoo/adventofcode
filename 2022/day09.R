library("stringr")

# Part 1

Rope <- R6::R6Class("Rope", list(
  dat = c(),
  hx = 0,
  hy = 0,
  tx = 0,
  ty = 0,
  tvisited = list(),

  read_input = function() {
    self$dat <- readLines("input09.txt")
    invisible(self)
  },

  move = function(input) {
    dist <- as.numeric(str_extract(input, "\\d+$"))
    if (str_detect(input, "^R")) {
      replicate(n = dist, {
        self$hx <- self$hx + 1
        self$follow()
      })
    } else if (str_detect(input, "^L")) {
      replicate(n = dist, {
        self$hx <- self$hx - 1
        self$follow()
      })
    } else if (str_detect(input, "^U")) {
      replicate(n = dist, {
        self$hy <- self$hy + 1
        self$follow()
      })
    } else {
      replicate(n = dist, {
        self$hy <- self$hy - 1
        self$follow()
      })
    }
    invisible(self)
  },

  follow = function() {
    if (self$hx - self$tx > 1 & self$hy == self$ty) { # right
      self$tx <- self$hx - 1
    } else if (self$hy - self$ty > 1 & self$hx == self$tx) { # up
      self$ty <- self$hy - 1
    } else if (self$hx - self$tx < -1 & self$hy == self$ty) { # left
      self$tx <- self$hx + 1
    } else if (self$hy - self$ty < -1 & self$hx == self$tx) { # down
      self$ty <- self$hy + 1
    } else if (self$hx - self$tx > 1 & abs(self$hy - self$ty) > 0) {
      self$ty <- self$hy
      self$tx <- self$hx - 1
    } else if (self$hy - self$ty > 1 & abs(self$hx - self$tx) > 0) {
      self$tx <- self$hx
      self$ty <- self$hy - 1
    } else if (self$hx - self$tx < -1 & abs(self$hy - self$ty) > 0) {
      self$ty <- self$hy
      self$tx <- self$hx + 1
    } else if (self$hy - self$ty < -1 & abs(self$hx - self$tx) > 0) {
      self$tx <- self$hx
      self$ty <- self$hy + 1
    }

    self$tvisited[[length(self$tvisited) + 1]] <- c(self$tx, self$ty)
    invisible(self)
  },

  run = function() {
    self$read_input()
    lapply(self$dat, self$move)
    length(unique(self$tvisited))
  }
))

r <- Rope$new()
r$run()
