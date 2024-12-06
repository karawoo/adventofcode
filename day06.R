library("readr")

dat <- read_fwf("input06.txt", col_positions = fwf_widths(rep(1, 10))) |>
  as.matrix()


Guard <- R6::R6Class("Guard",
  private = list(
    dir = "up",
    dirs = c("up", "right", "down", "left"),
    col = NULL,
    row = NULL,
    history = matrix(ncol = 2, nrow = 0),
    histhash = c(),
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
      private$history <- matrix(c(private$row, private$col), ncol = 2)
      private$histhash = rlang::hash(private$history[1, ])
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
        private$history <- rbind(
          private$history,
          next_loc
        )
        private$histhash <- c(
          private$histhash,
          rlang::hash(c(next_loc, which(private$dir == private$dirs)))
        )
      }
    },

    patrol = function(map = private$map) {
      while (!private$done && !rlang::hash(c(private$row, private$col, private$dir)) %in% private$histhash) {
        self$move(map)
      }
    },

    n_positions = function() {
      nrow(unique(private$history))
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
      options <- as.matrix(private$history[, 1:2])
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
g$patrol()
g$n_positions()


## part 2
g$find_obstacle_positions()
