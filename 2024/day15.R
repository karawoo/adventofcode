library("readr")
library("R6")

dat <- read_fwf(
  "input15.txt",
  col_positions = fwf_widths(rep(1, nchar(readLines("input15.txt", n = 1)))),
  n_max = 50
) |>
  as.matrix()

moves <- readLines("input15.txt")
moves <- moves[(which(moves == "") + 1):length(moves)] |>
  strsplit("") |>
  unlist()

Warehouse <- R6Class(
  "Warehouse",
  private = list(
    up = function() {
      path <- self$map[1:self$pos[1], self$pos[2]]
      new_path <- private$move(rev(path))
      self$map[1:self$pos[1], self$pos[2]] <- rev(new_path)
      self$pos <- which(self$map == "@", arr.ind = TRUE)
    },
    down = function() {
      path <- self$map[self$pos[1]:nrow(self$map), self$pos[2]]
      new_path <- private$move(path)
      self$map[self$pos[1]:nrow(self$map), self$pos[2]] <- new_path
      self$pos <- which(self$map == "@", arr.ind = TRUE)
    },
    left = function() {
      path <- self$map[self$pos[1], 1:self$pos[2]]
      new_path <- private$move(rev(path))
      self$map[self$pos[1], 1:self$pos[2]] <- rev(new_path)
      self$pos <- which(self$map == "@", arr.ind = TRUE)
    },
    right = function() {
      path <- self$map[self$pos[1], self$pos[2]:ncol(self$map)]
      new_path <- private$move(path)
      self$map[self$pos[1], self$pos[2]:ncol(self$map)] <- new_path
      self$pos <- which(self$map == "@", arr.ind = TRUE)
    },
    move = function(path) {
      if (!"." %in% path) {
        return(path)
      } else {
        space <- which(path == ".")[1]
        blocker <- which(path == "#")[1]
        if (blocker < space) {
          return(path)
        } else {
          return(c(".", path[1:(space-1)], path[(space + 1):length(path)]))
        }
      }
    }
  ),
  public = list(
    map = NULL,
    pos = NULL,
    initialize = function(map) {
      self$map <- map
      self$pos <- which(map == "@", arr.ind = TRUE)
    },
    roam = function(moves) {
      for (i in moves) {
        switch(
          i,
          "^" = private$up(),
          "v" = private$down(),
          "<" = private$left(),
          ">" = private$right()
        )
      }
    },
    get_gps = function() {
      (which(self$map == "O", arr.ind = TRUE) - 1) |>
        apply(1, \(x) 100 * x[1] + x[2]) |>
        sum()
    }
  )
)

w <- Warehouse$new(map = dat)
w$roam(moves)
w$get_gps()

