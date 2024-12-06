library("readr")

dat <- read_fwf("input06.txt", col_positions = fwf_widths(rep(1, 130))) |>
  as.matrix()


Guard <- R6::R6Class("Guard",
  private = list(
    dir = "up",
    dirs = c("up", "right", "down", "left"),
    col = NULL,
    row = NULL,
    history = matrix(ncol = 3, nrow = 0),
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
      private$history <- c(
        private$row,
        private$col,
        which(private$dir == private$dirs)
      )
      private$dir <- "up"
      private$done <- FALSE
    },

    move = function(map) {
      next_loc <- switch(
        private$dir,
        up = c(private$row - 1, private$col),
        right = c(private$row, private$col + 1),
        down = c(private$row + 1, private$col),
        left = c(private$row, private$col - 1)
      )

      ## if we've reached the edge and the next move would be off the map, we're
      ## done
      if (any(next_loc == 0) || any(next_loc > dim(map))) {
        private$done <- TRUE
        return()
      }

      if (map[next_loc[1], next_loc[2]] == "#") {
        next_dir <- which(private$dir == private$dirs) + 1
        private$dir <- private$dirs[ifelse(next_dir == 5, 1, next_dir)]
      } else {
        private$row <- next_loc[1]
        private$col <- next_loc[2]
        private$history <- rbind(private$history, c(next_loc, private$dir))
      }
    },

    patrol = function(map = private$map) {
      while (!private$done && !any(duplicated(private$history))) {
        self$move(map)
      }
    },

    n_positions = function() {
      nrow(unique(private$history[, 1:2])) # don't include direction
    },

    check_for_loop = function(obst, map) {
      ## reset history, etc.
      self$initialize(map = map)

      ## place obstacle and patrol
      map[obst[1], obst[2]] <- "#"
      self$patrol(map)

      ## If we never reach "done" it's because we got stuck in a loop
      if (!private$done) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },

    find_obstacle_positions = function(map = private$map) {
      options <- which(map == ".", arr.ind = TRUE)
      logger::log_info("{nrow(options)} options")
      #loops <- apply(options, 1, self$check_for_loop, map = map)
      loops <- apply(
        options,
        1,
        function(x) {
          logger::log_info("trying option {toString(x)}")
          self$check_for_loop(x, map)
        }
      )
      head(loops)
    }
  )
)

g <- Guard$new(map = dat)

## part 1
system.time(g$patrol())
## g$n_positions()


## part 2
profvis::profvis(g$find_obstacle_positions())
