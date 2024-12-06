library("readr")

dat <- read_fwf("input06.txt", col_positions = fwf_widths(rep(1, 130))) |>
  as.matrix()

Guard <- R6::R6Class("Guard",
  private = list(
    dir = "up",
    col = NULL,
    row = NULL,
    history = matrix(ncol = 2, nrow = 0),
    map = NULL,
    done = FALSE
  ),

  public = list(
    initialize = function(map) {
      ## assume a square matrix (necessary for the way the logic later determines
      ## if we're at an edge)
      stopifnot(length(unique(dim(map))) == 1)
      private$map <- map
      private$row <- which(map == "^", arr.ind = TRUE)[1]
      private$col <- which(map == "^", arr.ind = TRUE)[2]
      private$history <- c(private$row, private$col)
    },

    move = function() {
      next_loc <- switch(
        private$dir,
        up = c(private$row - 1, private$col),
        right = c(private$row, private$col + 1),
        down = c(private$row + 1, private$col),
        left = c(private$row, private$col - 1)
      )

      ## if we've reached the edge and the next move would be off the map, we're
      ## done
      if (any(next_loc == 0) || any(next_loc > dim(private$map))) {
        private$done <- TRUE
        return()
      }

      if (private$map[next_loc[1], next_loc[2]] == "#") {
        dirs <- c("up", "right", "down", "left")
        next_dir <- which(private$dir == dirs) + 1
        private$dir <- dirs[ifelse(next_dir == 5, 1, next_dir)]
      } else {
        private$row <- next_loc[1]
        private$col <- next_loc[2]
        private$history <- rbind(private$history, next_loc)
      }
    },

    patrol = function() {
      while (!private$done) {
        self$move()
      }
      nrow(unique(private$history))
    }
  )
)

g <- Guard$new(map = dat)
g$patrol()
