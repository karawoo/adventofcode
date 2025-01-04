library("stringr")
library("tidyverse")
library("R6")

dat <- readLines("input17.txt")

registers <- str_extract_all(head(dat, 3), "\\d+") |>
  map(as.numeric) |>
  setNames(c("A", "B", "C"))

program <- as.numeric(str_extract_all(tail(dat, 1), "\\d+")[[1]])

Program <- R6Class(
  "Program",
  public = list(
    A = NULL,
    B = NULL,
    C = NULL,
    output = NULL,
    program = NULL,
    pointer = 1,
    initialize = function(program, registers) {
      self$A <- registers$A
      self$B <- registers$B
      self$C <- registers$b
      self$program <- program
    },
    literal = function(x) {
      x
    },
    combo = function(x) {
      if (x <= 3) {
        return(x)
      } else if (x == 4) {
        return(self$A)
      } else if (x == 5) {
        return(self$B)
      } else if (x == 6) {
        return(self$C)
      } else {
        stop("invalid")
      }
    },
    adv = function(op) {
      #browser()
      self$A <- floor(self$A / self$combo(op))
      self$pointer <- self$pointer + 2
    },
    bxl = function(op) {
      self$B <- bitwXor(self$B, literal(op))
      self$pointer <- self$pointer + 2
    },
    bst = function(op) {
      self$B <- self$combo(op) %% 8
      self$pointer <- self$pointer + 2
    },
    jnz = function(op) {
      if (self$A == 0) {
        self$pointer <- self$pointer + 2
        return()
      } else {
        self$pointer <- op + 1
      }
    },
    bxc = function(op) {
      self$B <- btwXor(self$B, self$C)
      self$pointer <- self$pointer + 2
    },
    out = function(op) {
      self$output <- c(self$output, self$combo(op) %% 8)
      self$pointer <- self$pointer + 2
    },
    bdv = function(op) {
      self$B <- floor(self$A / self$combo(op))
      self$pointer <- self$pointer + 2
    },
    cdv = function(op) {
      self$C <- floor(self$A / self$combo(op))
      self$pointer <- self$pointer + 2
    },
    inst = function(inst) {
      #browser()
      switch(
        as.character(inst),
        "0" = self$adv,
        "1" = self$bxl,
        "2" = self$bst,
        "3" = self$jnz,
        "4" = self$bxc,
        "5" = self$out,
        "6" = self$bdv,
        "7" = self$cdv
      )
    },
    run = function() {
      while(self$pointer < length(self$program)) {
        #print(glue::glue("Pointer: {self$pointer}"))
        fun <- self$inst(self$program[self$pointer])
        fun(program[self$pointer + 1])
      }
      return(self$output)
    }
  )
)

p <- Program$new(program = c(0,1,5,4,3,0), registers = list(A = 2024, B = 0, C = 0))
p$run()
p$A
p$B
p$C
p$output
